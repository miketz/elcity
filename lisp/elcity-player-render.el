;;; elcity-player-render.el --- Tile rendering for player UI -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; Tile rendering pipeline for the player-facing game UI.
;;
;; Provides graphical tile map insertion with status icon overlays
;; (power/traffic), heatmap border tinting, lazy tooltip dispatch,
;; and a text fallback for non-graphical displays.
;;
;; Image caching:
;;   - tile-cache: tile index -> Emacs image descriptor (file-based)
;;   - tile-xpm-cache: tile index -> raw XPM data string
;;   - overlay-xpm-cache: (index, power, traffic, congestion) -> composed XPM
;;   - border-icon-xpm-cache: (index, color, power, traffic) -> XPM
;;
;; Tooltip infrastructure uses buffer-local state and a single shared
;; help-echo function to avoid per-tile closure allocation.

;;; Code:

(require 'cl-lib)
(require 'elcity-building)
(require 'elcity-inspect-model)
(require 'elcity-state-api)
(require 'elcity-overlay)
(require 'elcity-power)
(require 'elcity-quality)
(require 'elcity-state)
(require 'elcity-tile)
(require 'elcity-tile-field)
(require 'elcity-tile-overlay)
(require 'elcity-traffic)
(require 'elcity-world)
(require 'elcity-zones)

;;; ---------- Tile image cache and status resolver ----------

;; Assumes this file lives in lisp/ under the project root.
;; Walks up: lisp/elcity-player-render.el -> lisp/ -> project-root -> tiles/.
(defvar elcity-player-render--tile-dir
  (expand-file-name "tiles/" (file-name-directory
                               (directory-file-name
                                (file-name-directory
                                 (or load-file-name buffer-file-name
                                     default-directory)))))
  "Directory containing generated XPM tile files.")

(defconst elcity-player-render-tile-pixel-size 16
  "Tile image dimension in pixels (width and height).
Shared source of truth for all render-path pixel arithmetic.")

(defvar elcity-player-render--tile-scale 1
  "Scale factor passed to `create-image' for tile display.
Changing scale requires clearing caches or restarting.")

(defvar elcity-player-render--tile-cache (make-hash-table :test #'eql)
  "Cache mapping tile INDEX to Emacs image descriptor.")

(defvar elcity-player-render--overlay-xpm-cache (make-hash-table :test #'equal)
  "Cache: (INDEX POWER TRAFFIC CONGESTION BURNING-P) -> XPM data.
Fresh `create-image' is returned each call to avoid Emacs `eq' display merging.")

(defvar elcity-player-render--border-icon-xpm-cache (make-hash-table :test #'equal)
  "Cache mapping (INDEX OVERLAY-COLOR POWER-STATUS TRAFFIC-STATUS) to XPM data.
Used when both heatmap border and status icons are active on a tile.")

(defvar elcity-player-render--tile-fallback nil
  "Cached fallback magenta 16x16 image descriptor.")

(defun elcity-player-render--tile-image-path (index)
  "Return absolute XPM file path for tile INDEX."
  (expand-file-name (format "tile-%04d.xpm" index)
                    elcity-player-render--tile-dir))

(defun elcity-player-render--tile-fallback-image ()
  "Return a cached magenta fallback XPM image descriptor for missing tiles."
  (or elcity-player-render--tile-fallback
      (let ((s elcity-player-render-tile-pixel-size))
        (setq elcity-player-render--tile-fallback
              (create-image
               (concat
                "/* XPM */\nstatic char *fallback[] = {\n"
                (format "\"%d %d 1 1\",\n" s s)
                "\"M c #FF00FF\",\n"
                (mapconcat (lambda (_)
                             (format "\"%s\"" (make-string s ?M)))
                           (number-sequence 1 s) ",\n")
                "\n};")
               'xpm t :scale elcity-player-render--tile-scale)))))

(defun elcity-player-render--tile-image-get (index)
  "Return image descriptor for tile INDEX.
Loads from file on first access; returns fallback for missing files."
  (or (gethash index elcity-player-render--tile-cache)
      (let ((path (elcity-player-render--tile-image-path index)))
        (puthash index
                 (if (file-exists-p path)
                     (create-image path 'xpm nil
                                   :scale elcity-player-render--tile-scale)
                   (elcity-player-render--tile-fallback-image))
                 elcity-player-render--tile-cache))))

(defvar elcity-player-render--tile-xpm-cache (make-hash-table :test #'eql)
  "Cache mapping tile INDEX to raw XPM data string read from file.")

(defun elcity-player-render--tile-xpm-data (index)
  "Return raw XPM data string for tile INDEX, or nil if file missing.
Caches file contents; returns the same string object on repeat calls."
  (let ((cached (gethash index elcity-player-render--tile-xpm-cache 'miss)))
    (if (not (eq cached 'miss))
        cached
      (let* ((path (elcity-player-render--tile-image-path index))
             (data (when (file-exists-p path)
                     (with-temp-buffer
                       (insert-file-contents path)
                       (buffer-string)))))
        (puthash index data elcity-player-render--tile-xpm-cache)
        data))))

(defun elcity-player-render--tile-image-get-overlaid (index power-status traffic-status
                                                            &optional congestion-severity
                                                            burning-p)
  "Return image descriptor for tile INDEX with icon overlays.
POWER-STATUS is nil, `unpowered', or `disconnected'.
TRAFFIC-STATUS is nil, `blocked', or `no-road'.
CONGESTION-SEVERITY is nil, `critical', or `blocked'.
BURNING-P is non-nil when the tile is on fire.
Caches the composed XPM data string; returns a fresh `create-image'
each call to avoid Emacs `eq' display merging."
  (let* ((key (list index power-status traffic-status congestion-severity
                    burning-p))
         (xpm-data (gethash key elcity-player-render--overlay-xpm-cache)))
    (unless xpm-data
      (let ((raw (elcity-player-render--tile-xpm-data index)))
        (when raw
          (setq xpm-data (elcity-tile-overlay-compose-xpm
                          raw power-status traffic-status congestion-severity
                          burning-p))
          (puthash key xpm-data elcity-player-render--overlay-xpm-cache))))
    (if xpm-data
        (create-image xpm-data 'xpm t
                      :scale elcity-player-render--tile-scale)
      (elcity-player-render--tile-fallback-image))))

(defun elcity-player-render--tile-status (tile power-scan connected-grid x y)
  "Return power status symbol for TILE at X,Y given POWER-SCAN and CONNECTED-GRID.
Returns nil (base/powered), `unpowered', or `disconnected'."
  (cond
   ((not (elcity-tile-conductive-p tile)) nil)
   ((elcity-power-grid-cell-at (elcity-power-scan-grid power-scan) x y nil) nil)
   ((elcity-power-grid-cell-at connected-grid x y nil) 'unpowered)
   (t 'disconnected)))

(defun elcity-player-render--clamp-zone-index (index)
  "Clamp and normalize zone INDEX for rendering.
Center indices are kind-base + 0..3 and are returned unchanged.
Border indices are kind-base + 4..7 and are remapped to center indices.
Values above border max clamp to border max then remap to center max.
Returns INDEX unchanged for non-zone tiles."
  (let ((offset elcity-zones-border-index-offset))
    (cl-labels
        ((normalize-kind (kind-base)
           (let* ((center-max (+ kind-base elcity-zones-level-max))
                  (border-max (+ kind-base offset elcity-zones-level-max))
                  (clamped (min index border-max)))
             (if (> clamped center-max)
                 (- clamped offset)
               clamped))))
      (cond
       ((and (<= elcity-tile-residential-index-min index)
             (< index elcity-tile-commercial-index-min))
        (normalize-kind elcity-tile-residential-index-min))
       ((and (<= elcity-tile-commercial-index-min index)
             (< index elcity-tile-industrial-index-min))
        (normalize-kind elcity-tile-commercial-index-min))
       ((and (<= elcity-tile-industrial-index-min index)
             (<= index elcity-tile-industrial-index-max))
        (normalize-kind elcity-tile-industrial-index-min))
       (t index)))))

(defun elcity-player-render--autotile-index (state x y index)
  "Return auto-tiled INDEX for roads/powerlines at X,Y in STATE.
Checks cardinal neighbors and builds a 4-bit mask (N=1 E=2 S=4 W=8)
to select the correct connectivity variant.  Non-road/powerline
indices are returned unchanged."
  (cond
   ((elcity-traffic-crossing-index-p index)
    index)
   ((elcity-traffic-road-index-p index)
    (+ elcity-traffic-road-index-min
       (elcity-building-cardinal-mask
        state x y #'elcity-building-road-neighbor-p)))
   ((elcity-traffic-powerline-index-p index)
    (+ elcity-traffic-powerline-index-min
       (elcity-building-cardinal-mask
        state x y #'elcity-building-powerline-neighbor-p)))
   (t index)))

(defun elcity-player-render--tile-traffic-status (tile x y traffic-scan)
  "Return traffic status for TILE at X,Y using TRAFFIC-SCAN.
Returns nil for non-zone-center tiles or when traffic scan is unavailable.
Returns `blocked' or `no-road' for zone centers failing the S3 gate."
  (when (and traffic-scan (elcity-tile-zone-center-p tile))
    (let ((status (elcity-traffic-route-status-at traffic-scan x y nil)))
      (when (and status (not (eq status 'reachable)))
        status))))

(defun elcity-player-render--tile-image-for (state tile x y power-scan connected-grid
                                                   &optional traffic-scan
                                                   overlay-color
                                                   congestion-severity
                                                   burning-p)
  "Return tile image descriptor for TILE at X,Y in STATE.
Uses POWER-SCAN and CONNECTED-GRID for status resolution.
Optional TRAFFIC-SCAN enables traffic icon overlays on zone centers.
Station centers also receive traffic overlay when not road-adjacent.
Optional OVERLAY-COLOR is a hex color string for heatmap border tinting.
Optional CONGESTION-SEVERITY is nil, `critical', or `blocked' for road
tiles in normal view (suppressed when OVERLAY-COLOR is active).
Optional BURNING-P is non-nil when the tile is currently on fire."
  (let* ((index (elcity-player-render--clamp-zone-index (elcity-tile-index tile)))
         (index (elcity-player-render--autotile-index state x y index))
         (power-status (elcity-player-render--tile-status
                        tile power-scan connected-grid x y))
         (traffic-status
          (or (elcity-player-render--tile-traffic-status
               tile x y traffic-scan)
              (let ((raw-index (elcity-tile-index tile)))
                (when (elcity-quality-station-center-index-p raw-index)
                  (unless (elcity-quality-station-road-adjacent-p
                           (elcity-state-world state) x y)
                    'no-road)))))
         (needs-overlay (elcity-tile-overlay-needs-overlay-p
                         power-status traffic-status congestion-severity
                         burning-p)))
    (cond
     ;; Overlay border active: always work with XPM data strings
     (overlay-color
      (let ((raw (elcity-player-render--tile-xpm-data index)))
        (if (not raw)
            (elcity-player-render--tile-fallback-image)
          (let* ((needs-icon (elcity-tile-overlay-needs-overlay-p
                              power-status traffic-status nil burning-p))
                 (key (list index overlay-color power-status traffic-status
                            burning-p))
                 (xpm-data (gethash key elcity-player-render--border-icon-xpm-cache)))
            (unless xpm-data
              (setq xpm-data (elcity-overlay-border-xpm-cached raw overlay-color))
              (when needs-icon
                (setq xpm-data (elcity-tile-overlay-compose-xpm
                                xpm-data power-status traffic-status
                                nil burning-p)))
              (puthash key xpm-data elcity-player-render--border-icon-xpm-cache))
            (create-image xpm-data 'xpm t
                          :scale elcity-player-render--tile-scale)))))
     ;; Status icons and/or congestion stripe (no overlay border)
     (needs-overlay
      (elcity-player-render--tile-image-get-overlaid
       index power-status traffic-status congestion-severity burning-p))
     ;; Plain tile
     (t
      (elcity-player-render--tile-image-get index)))))

;;; ---------- Congestion severity mapping ----------

(defun elcity-player-render--congestion-severity (value &optional effective-cap)
  "Return congestion severity symbol for integer VALUE.
EFFECTIVE-CAP defaults to `elcity-traffic-congestion-cap'.  `blocked'
starts at EFFECTIVE-CAP, and `critical' starts at the effective-cap-scaled
equivalent of `elcity-traffic-congestion-high-threshold'."
  (let* ((cap (or effective-cap elcity-traffic-congestion-cap))
         (critical-threshold (max 1 (/ (* elcity-traffic-congestion-high-threshold
                                         cap)
                                      elcity-traffic-congestion-cap))))
    (cond
     ((or (null value) (< value critical-threshold)) nil)
     ((>= value cap) 'blocked)
     (t 'critical))))

;;; ---------- Display helpers ----------

(defun elcity-player-render--display-power-scan (state)
  "Return S2 power scan for STATE used by rendering.
When STATE lacks cached S2 results, compute from current world."
  (or (elcity-state-power-scan state)
      (elcity-power-scan-world (elcity-state-world state))))

(defun elcity-player-render--display-traffic-scan (state)
  "Return S3 traffic scan for STATE, or nil when unavailable."
  (elcity-state-traffic-scan state))

(defun elcity-player-render--connected-conductive-grid (state power-scan)
  "Return grid of conductive tiles connected to any source in STATE.
POWER-SCAN provides deterministic source order."
  (let* ((world (elcity-state-world state))
         (capacity (* (elcity-world-map-width world)
                      (elcity-world-map-height world)))
         (sources (elcity-power-scan-sources power-scan)))
    (elcity-power-scan-grid
     (elcity-power-scan-conductive-network world sources capacity))))

;;; ---------- Overlay field helpers ----------

(defun elcity-player-render--overlay-field-map (state overlay-kind)
  "Return the tile-field overlay backing map for OVERLAY-KIND from STATE, or nil."
  (pcase overlay-kind
    ('pollution (elcity-state-api-quality-pollution-map state))
    ('crime (elcity-state-api-quality-crime-map state))
    ('land-value (elcity-state-api-quality-land-value-map state))
    ('congestion (elcity-state-api-traffic-congestion-map state))
    ('fire-coverage (elcity-state-api-quality-fire-coverage-map state))
    ('police-coverage (elcity-state-api-quality-police-coverage-map state))
    (_ nil)))

(defun elcity-player-render--overlay-tile-color (overlay-kind field-map x y
                                                             &optional value-max-override)
  "Return hex color for tile X,Y under OVERLAY-KIND using FIELD-MAP.
FIELD-MAP must be a tile-resolution `elcity-tile-field'.  Returns nil
when FIELD-MAP is nil."
  (when field-map
    (cl-check-type field-map elcity-tile-field)
    (let* ((value (elcity-tile-field-ref field-map x y 0))
           (config (elcity-overlay-kind-config overlay-kind))
           (value-max (or value-max-override
                          (plist-get config :value-max)))
           (direction (plist-get config :direction)))
      (elcity-overlay-value-to-color value value-max direction))))

;;; ---------- Tooltip buffer-local state ----------
;;
;; Tooltip data is stored buffer-locally so that a single named function
;; can serve as help-echo for every tile, avoiding per-tile closure
;; allocation (N*N closures per render on a 64x64 map = 4096 objects).

(defvar-local elcity-player-render--tooltip-state nil
  "Buffer-local state snapshot for tooltip computation.")

(defvar-local elcity-player-render--tooltip-power-scan nil
  "Buffer-local power scan for tooltip computation.")

(defvar-local elcity-player-render--tooltip-connected-grid nil
  "Buffer-local connected grid for tooltip computation.")

(defvar-local elcity-player-render--tooltip-fn nil
  "Buffer-local tooltip function for help-echo dispatch.")

(defvar-local elcity-player-render--tooltip-map-start nil
  "Buffer position where the tile image grid begins.")

(defvar-local elcity-player-render--tooltip-map-width nil
  "Tile map width for tooltip position arithmetic.")

(defvar-local elcity-player-render--tooltip-map-height nil
  "Tile map height for tooltip position arithmetic.")

(defun elcity-player-render-set-tooltip-map-start (pos)
  "Set the tooltip map-start position to POS.
Used by callers that re-render chrome without re-inserting the tile map."
  (setq elcity-player-render--tooltip-map-start pos))

(defun elcity-player-render--help-echo (_window _object pos)
  "Compute tooltip for tile at buffer POS using buffer-local state.
Used as a single shared help-echo function for all tile positions."
  (let ((map-start elcity-player-render--tooltip-map-start)
        (width elcity-player-render--tooltip-map-width)
        (height elcity-player-render--tooltip-map-height)
        (state elcity-player-render--tooltip-state)
        (tip-fn elcity-player-render--tooltip-fn))
    (when (and map-start width state tip-fn (>= pos map-start))
      (let* ((offset (- pos map-start))
             (row-len (1+ (* width 2)))  ; 2 chars per tile + newline
             (tile-y (/ offset row-len))
             (col (% offset row-len))
             (tile-x (/ col 2)))
        (when (and (>= tile-x 0) (< tile-x width)
                   (>= tile-y 0) (< tile-y height)
                   (< col (* width 2)))  ; not on the newline
          (let ((tile (elcity-state-api-tile-at state tile-x tile-y 0)))
            (funcall tip-fn state tile tile-x tile-y
                     elcity-player-render--tooltip-power-scan
                     elcity-player-render--tooltip-connected-grid)))))))

;;; ---------- Graphical tile map insertion ----------

(defun elcity-player-render--insert-world-images (state power-scan connected-grid
                                                        &optional tooltip-fn
                                                        traffic-scan
                                                        overlay-kind)
  "Insert tile images for STATE world into current buffer.
POWER-SCAN and CONNECTED-GRID drive status variant selection.
Tooltips use a single shared help-echo function (zero per-tile allocation).
Optional TOOLTIP-FN overrides the default tooltip; it is called
with (STATE TILE X Y POWER-SCAN CONNECTED-GRID) and should return a string.
Optional TRAFFIC-SCAN enables traffic icon overlays on zone centers.
Optional OVERLAY-KIND is an overlay kind symbol for heatmap borders."
  (let* ((world (elcity-state-world state))
         (width (elcity-world-map-width world))
         (height (elcity-world-map-height world))
         (effective-congestion-cap
          (elcity-state-api-traffic-effective-congestion-cap state))
         (overlay-map (when overlay-kind
                        (elcity-player-render--overlay-field-map
                         state overlay-kind)))
         ;; Fetch congestion map once for normal view road stripe rendering.
         ;; Suppressed when a heatmap overlay is active.
         (congestion-map (unless overlay-kind
                           (elcity-state-api-traffic-congestion-map state)))
         ;; Fire burn timers for burning overlay.
         (burn-timers (elcity-state-api-fire-burn-timers state)))
    ;; Store tooltip context as buffer-locals for the shared help-echo fn
    (setq elcity-player-render--tooltip-state state
          elcity-player-render--tooltip-power-scan power-scan
          elcity-player-render--tooltip-connected-grid connected-grid
          elcity-player-render--tooltip-fn (or tooltip-fn
                                               #'elcity-player-render--default-tooltip)
          elcity-player-render--tooltip-map-start (point)
          elcity-player-render--tooltip-map-width width
          elcity-player-render--tooltip-map-height height)
    (dotimes (y height)
      (dotimes (x width)
        (let* ((tile (elcity-state-api-tile-at state x y 0))
               (overlay-color
                (when overlay-map
                  (elcity-player-render--overlay-tile-color
                   overlay-kind overlay-map x y
                   (and (eq overlay-kind 'congestion)
                        effective-congestion-cap))))
               (congestion-severity
                (when congestion-map
                  (let ((raw-index (elcity-tile-index tile)))
                    (when (elcity-traffic-road-index-p raw-index)
                      (elcity-player-render--congestion-severity
                       (elcity-tile-field-ref congestion-map x y 0)
                       effective-congestion-cap)))))
               (burning-p (and burn-timers
                               (gethash (cons x y) burn-timers)))
               (img (elcity-player-render--tile-image-for
                     state tile x y power-scan connected-grid
                     traffic-scan overlay-color congestion-severity
                     burning-p))
               (start (point)))
          (insert-image img "  ")
          ;; Strip image-map keymap so caller mode bindings remain active.
          (remove-text-properties start (point) '(keymap nil local-map nil))
          (put-text-property start (point) 'help-echo
                             #'elcity-player-render--help-echo)))
      ;; Set newline to a small absolute pixel height so the default
      ;; line height does not add whitespace gaps between tile rows.
      (let ((nl-start (point)))
        (insert "\n")
        (put-text-property nl-start (point) 'face '(:height 10))))))

;;; ---------- Default tooltip ----------

(defun elcity-player-render--default-tooltip (state _tile x y power-scan connected-grid)
  "Return default tooltip string for tile at X,Y in STATE.
_TILE is accepted for call-site compatibility.
POWER-SCAN and CONNECTED-GRID provide power status context."
  (let* ((scan (or power-scan (elcity-state-api-power-scan state)))
         (connected (or connected-grid
                        (elcity-inspect-connected-conductive-grid state scan)))
         (info (elcity-inspect-tile-info-at state x y scan connected))
         (index (elcity-inspect-tile-info-tile-index info))
         (ctx (elcity-inspect-tile-info-context-kind info)))
    (cond
     ((eq ctx 'zone)
      (let ((kind (elcity-inspect-tile-info-zone-kind info))
            (level (elcity-inspect-tile-info-zone-level info))
            (pop (elcity-inspect-tile-info-zone-population info)))
        (format "(%d,%d) %s L%d pop=%d" x y kind level pop)))
     ((eq ctx 'building)
      (format "(%d,%d) %s" x y
              (downcase (elcity-inspect-tile-info-structure-name info))))
     ((eq ctx 'empty) (format "(%d,%d) empty" x y))
     ((eq ctx 'water) (format "(%d,%d) water" x y))
     (t (format "(%d,%d) idx=%d" x y index)))))

;;; ---------- Text fallback rendering ----------

(defun elcity-player-render--x-axis-string (width)
  "Return x-axis decimal index ruler string for WIDTH."
  (let ((axis (make-string width ?0)))
    (dotimes (x width)
      (aset axis x (+ ?0 (mod x 10))))
    axis))

(defun elcity-player-render--zone-base-glyph (kind)
  "Return uppercase glyph character for zone KIND."
  (pcase kind
    ('residential ?R)
    ('commercial ?C)
    ('industrial ?I)
    (_ ?Z)))

(defun elcity-player-render--zone-glyph (state x y kind)
  "Return one-character zone glyph for STATE zone KIND at X,Y.
Uppercase indicates both power and traffic gates are satisfied.
Lowercase indicates at least one gate is missing."
  (let ((base (elcity-player-render--zone-base-glyph kind))
        (powered (elcity-state-api-powered-tile-p state x y nil))
        (reachable (elcity-state-api-zone-destination-reachable-p state x y)))
    (if (and powered reachable)
        base
      (downcase base))))

(defun elcity-player-render--tile-glyph (state x y)
  "Return one-character world glyph for tile X,Y in STATE."
  (let* ((tile (elcity-state-api-tile-at state x y 0))
         (index (elcity-tile-index tile))
         (kind (and (elcity-tile-zone-center-p tile)
                    (elcity-zones-kind-for-index index)))
         (border-kind (and (not kind)
                           (elcity-zones-kind-for-index index))))
    (cond
     (kind (elcity-player-render--zone-glyph state x y kind))
     (border-kind (downcase (elcity-player-render--zone-base-glyph border-kind)))
     ((= index elcity-power-coal-plant-index) ?P)
     ((= index elcity-power-nuclear-plant-index) ?N)
     ((elcity-traffic-crossing-index-p index) ?X)
     ((elcity-traffic-road-index-p index) ?=)
     ((elcity-traffic-powerline-index-p index) ?p)
     ((elcity-tile-conductive-p tile) ?t)
     ((= index 0) ?.)
     (t ?#))))

(defun elcity-player-render-world-lines (state)
  "Return world view lines for STATE as a list of strings."
  (cl-check-type state elcity-state)
  (let* ((world (elcity-state-world state))
         (width (elcity-world-map-width world))
         (height (elcity-world-map-height world))
         (x-axis (elcity-player-render--x-axis-string width))
         lines)
    (push (format "    %s" x-axis) lines)
    (dotimes (y height)
      (let ((row (make-string width ?.)))
        (dotimes (x width)
          (aset row x (elcity-player-render--tile-glyph state x y)))
        (push (format "%3d %s" y row) lines)))
    (nreverse lines)))

;;; ---------- Public API ----------

(defun elcity-player-render-tile-view-available-p ()
  "Return non-nil when graphical tile view can be used."
  (and (display-graphic-p)
       (file-directory-p elcity-player-render--tile-dir)))

(defun elcity-player-render-tile-scale ()
  "Return the current tile display scale factor."
  elcity-player-render--tile-scale)

(defun elcity-player-render-insert-tile-map (state &optional tooltip-fn
                                                     overlay-kind)
  "Insert graphical tile map for STATE into current buffer.
Computes power-scan, connectivity grid, and traffic scan internally.
Tooltips are lazy (computed on hover, zero render-time cost).
Optional TOOLTIP-FN overrides the default tooltip.
Optional OVERLAY-KIND is an overlay kind symbol for heatmap borders."
  (let* ((power-scan (elcity-player-render--display-power-scan state))
         (connected-grid (elcity-player-render--connected-conductive-grid
                          state power-scan))
         (traffic-scan (elcity-player-render--display-traffic-scan state)))
    (elcity-player-render--insert-world-images
     state power-scan connected-grid tooltip-fn traffic-scan
     overlay-kind)))

(provide 'elcity-player-render)

;;; elcity-player-render.el ends here
