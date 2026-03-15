;;; elcity-player-input.el --- Player input and command handling -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; Movement, placement preview, building placement/inspect, budget
;; controls, save/load, mouse handlers, undo, and quit commands
;; for the player subsystem.
;;
;; Depends on `elcity-player-core' for session state, tool metadata,
;; and tile coordinate helpers, and `elcity-player-ui' for rendering.

;;; Code:

(require 'cl-lib)
(require 'elcity)
(require 'elcity-actions)
(require 'elcity-budget)
(require 'elcity-building)
(require 'elcity-persist)
(require 'elcity-player-core)
(require 'elcity-player-render)
(require 'elcity-player-session)
(require 'elcity-player-ui)

(defvar elcity-player-mode-map)

;;; ---------- Placement preview overlay ----------

(defface elcity-player-preview-face
  '((t :background "#444466"))
  "Face for placement preview footprint highlight.
Used in `text-mode' fallback only.")

(defconst elcity-player--highlight-color "#FFFF00"
  "Border color for placement preview highlight (yellow).")

(defvar elcity-player--highlight-cache (make-hash-table :test #'equal)
  "Cache mapping cache-key to highlighted image descriptor.
Global (not buffer-local) because tile images are shared across sessions.
Key space is bounded by the number of distinct tile XPM files/data strings.")

(defun elcity-player--highlight-xpm-data (xpm-data)
  "Return XPM string with a 1px highlight border drawn into XPM-DATA.
Adds a highlight color entry and replaces border pixels."
  (let* ((s elcity-player-render-tile-pixel-size)
         (pixel-re (format "^\"\\(.\\{%d\\}\\)\"" s))
         (header-re (format "^\"%d %d \\([0-9]+\\) 1\"" s s))
         (lines (split-string xpm-data "\n"))
         (result nil)
         (in-pixels nil)
         (row-count 0)
         (color-added nil))
    (dolist (line lines)
      (cond
       ;; First pixel row: insert highlight color def before it
       ((and (not color-added)
             (not in-pixels)
             (string-match pixel-re line))
        (push (format "\"H c %s\"," elcity-player--highlight-color)
              result)
        (setq color-added t
              in-pixels t)
        (push (elcity-player--highlight-row line row-count s) result)
        (setq row-count (1+ row-count)))
       ;; Subsequent pixel rows
       (in-pixels
        (if (string-match pixel-re line)
            (progn
              (push (elcity-player--highlight-row line row-count s) result)
              (setq row-count (1+ row-count)))
          (push line result)))
       ;; Header: bump color count
       ((string-match header-re line)
        (let ((n (string-to-number (match-string 1 line))))
          (push (format "\"%d %d %d 1\"," s s (1+ n)) result)))
       (t (push line result))))
    (mapconcat #'identity (nreverse result) "\n")))

(defun elcity-player--highlight-row (line row-index tile-size)
  "Return LINE with border pixels replaced for ROW-INDEX (0-based).
TILE-SIZE is the pixel dimension of the tile.
First and last rows get all pixels replaced; middle rows get only
first and last pixel replaced."
  (let ((pixel-re (format "^\"\\(.\\{%d\\}\\)\"\\(.*\\)" tile-size)))
    (if (string-match pixel-re line)
        (let ((pixels (match-string 1 line))
              (suffix (match-string 2 line)))
          (if (or (= row-index 0) (= row-index (1- tile-size)))
              (format "\"%s\"%s" (make-string tile-size ?H) suffix)
            (format "\"H%sH\"%s"
                    (substring pixels 1 (1- tile-size))
                    suffix)))
      line)))

(defun elcity-player--footprint-tiles (tool-kind tile-x tile-y)
  "Return list of (X . Y) tile coords for TOOL-KIND placed at TILE-X, TILE-Y."
  (let* ((fp (elcity-player-tool-footprint tool-kind))
         (size (plist-get fp :size))
         (anchor (plist-get fp :anchor)))
    (pcase anchor
      ('tile (list (cons tile-x tile-y)))
      ('center
       (let ((ox (- tile-x (/ size 2)))
             (oy (- tile-y (/ size 2)))
             coords)
         (dotimes (dy size)
           (dotimes (dx size)
             (push (cons (+ ox dx) (+ oy dy)) coords)))
         (nreverse coords))))))

(defun elcity-player--tile-highlight-image (pos)
  "Return highlighted image for tile at buffer POS, or nil.
Handles both file-based and in-memory XPM images."
  (let ((img (get-text-property pos 'display)))
    (when (and img (eq (car-safe img) 'image))
      (let* ((props (cdr img))
             (file (plist-get props :file))
             (data (plist-get props :data))
             (cache-key (or file data)))
        (when cache-key
          (let ((hi-img (or (gethash cache-key elcity-player--highlight-cache)
                            (let* ((xpm-str (cond
                                             (file (with-temp-buffer
                                                     (insert-file-contents file)
                                                     (buffer-string)))
                                             (data data)))
                                   (highlighted (elcity-player--highlight-xpm-data
                                                 xpm-str))
                                   (img (create-image highlighted 'xpm t
                                                      :scale (elcity-player-render-tile-scale))))
                              (puthash cache-key img
                                       elcity-player--highlight-cache)
                              img))))
            ;; Return a shallow copy so adjacent overlays with the same tile
            ;; do not share an eq object (Emacs merges eq display properties).
            (copy-sequence hi-img)))))))

;;; ---------- Map-bound helpers ----------

(defun elcity-player--clamp-to-nearest-tile ()
  "Return (TILE-X . TILE-Y) of the nearest valid map tile to point.
Always returns a valid tile coordinate when session and map positions
are initialized.  Returns nil if session is not ready."
  (when (and elcity-player--session
             elcity-player--map-start-pos
             elcity-player--palette-start-pos)
    (let* ((state (elcity-player-session-state elcity-player--session))
           (world (elcity-state-world state))
           (width (elcity-world-map-width world))
           (height (elcity-world-map-height world)))
      ;; First try: if already on map, return that tile
      (or (elcity-player-tile-at-point)
          ;; Otherwise clamp to edge tile
          (let* ((pos (point))
                 (graphical (elcity-player-render-tile-view-available-p))
                 (map-start elcity-player--map-start-pos)
                 (palette-start elcity-player--palette-start-pos))
            (cond
             ;; Before map: first tile (0,0)
             ((< pos map-start)
              (cons 0 0))
             ;; After or at palette: last tile
             ((>= pos palette-start)
              (cons (1- width) (1- height)))
             ;; In map region but tile-at-point returned nil (on newline, etc.)
             (t
              (if graphical
                  (let* ((offset (- pos map-start))
                         (row-len (1+ (* width 2)))
                         (tile-y (min (1- height) (/ offset row-len)))
                         (col (% offset row-len))
                         (tile-x (min (1- width) (/ col 2))))
                    (cons tile-x tile-y))
                ;; Text fallback: clamp to nearest valid tile
                (cons (1- width) (1- height))))))))))

(defun elcity-player--confinement-suppressed-p ()
  "Return non-nil when cursor confinement should be suppressed."
  (or elcity-player--inhibit-confinement
      (minibufferp)
      (bound-and-true-p isearch-mode)))

(defun elcity-player--confine-to-map ()
  "Clamp point to the nearest valid map tile if it drifted outside.
Also snaps to the nearest tile when point is in the map region but
not on a valid tile position (e.g. on a newline at row end).
Does nothing when confinement is suppressed."
  (when (and elcity-player--session
             (not (elcity-player--confinement-suppressed-p))
             (or (not (elcity-player-point-in-map-p))
                 (not (elcity-player-tile-at-point))))
    (let ((tile (elcity-player--clamp-to-nearest-tile)))
      (when tile
        (elcity-player-goto-tile (car tile) (cdr tile))))))

;;; ---------- Movement ----------

(defun elcity-player--move-tile (dx dy)
  "Move cursor by DX,DY tiles, clamping at map edges."
  (let ((tile (elcity-player-tile-at-point)))
    (when tile
      (let* ((state (elcity-player-session-state elcity-player--session))
             (world (elcity-state-world state))
             (width (elcity-world-map-width world))
             (height (elcity-world-map-height world))
             (nx (max 0 (min (1- width) (+ (car tile) dx))))
             (ny (max 0 (min (1- height) (+ (cdr tile) dy)))))
        (elcity-player-goto-tile nx ny)))))

(defun elcity-player-move-right ()
  "Move cursor one tile to the right, clamping at map edge."
  (interactive)
  (elcity-player--move-tile 1 0))

(defun elcity-player-move-left ()
  "Move cursor one tile to the left, clamping at map edge."
  (interactive)
  (elcity-player--move-tile -1 0))

(defun elcity-player-move-down ()
  "Move cursor one tile down, clamping at map edge."
  (interactive)
  (elcity-player--move-tile 0 1))

(defun elcity-player-move-up ()
  "Move cursor one tile up, clamping at map edge."
  (interactive)
  (elcity-player--move-tile 0 -1))

(defun elcity-player-move-beginning-of-row ()
  "Move cursor to the first tile in the current row."
  (interactive)
  (let ((tile (elcity-player-tile-at-point)))
    (when tile
      (elcity-player-goto-tile 0 (cdr tile)))))

(defun elcity-player-move-end-of-row ()
  "Move cursor to the last tile in the current row."
  (interactive)
  (let ((tile (elcity-player-tile-at-point)))
    (when tile
      (let* ((state (elcity-player-session-state elcity-player--session))
             (world (elcity-state-world state))
             (width (elcity-world-map-width world)))
        (elcity-player-goto-tile (1- width) (cdr tile))))))

;;; ---------- Preview update ----------

(defun elcity-player--snap-to-tile ()
  "Snap point to tile-start position in graphical mode.
Each tile occupies 2 chars; this ensures the cursor always sits on
the first char so arrow-key movement advances a full tile at a time."
  (when (and elcity-player--map-start-pos
             elcity-player--palette-start-pos
             (elcity-player-render-tile-view-available-p)
             (>= (point) elcity-player--map-start-pos)
             (< (point) elcity-player--palette-start-pos))
    (let* ((offset (- (point) elcity-player--map-start-pos))
           (state (elcity-player-session-state elcity-player--session))
           (width (elcity-world-map-width (elcity-state-world state)))
           (row-len (1+ (* width 2)))
           (col (% offset row-len)))
      ;; If on second char of a tile pair, advance to the next tile
      (when (and (< col (* width 2)) (= (% col 2) 1))
        (forward-char 1)))))

(defun elcity-player-input-post-command ()
  "Post-command handler: confine cursor then update preview."
  (elcity-player--confine-to-map)
  (elcity-player--update-preview))

(defun elcity-player-input--mode-map-command-for-current-keys ()
  "Return command bound in `elcity-player-mode-map' for current key input."
  (let* ((keys (this-single-command-keys))
         (binding (and keys (lookup-key elcity-player-mode-map keys))))
    (and (commandp binding) binding)))

(defun elcity-player-input-pre-command ()
  "Pre-command handler: block non-game commands in player buffers."
  (let ((allowed (elcity-player-input--mode-map-command-for-current-keys)))
    (unless (or (not (derived-mode-p 'elcity-player-mode))
                (eq this-command allowed))
      (setq this-command #'ignore
            real-this-command #'ignore))))

(defun elcity-player--update-preview ()
  "Update placement preview overlay for current point and tool.
Only redraws when the tile under point changes."
  (when (and elcity-player--session
             elcity-player--map-start-pos
             elcity-player--palette-start-pos)
    (elcity-player--snap-to-tile)
    (let* ((tool (elcity-player-session-active-tool elcity-player--session))
           (coords (and (not (eq tool 'inspect))
                        (elcity-player-tile-at-point))))
      (if (not coords)
          ;; Off-map or inspect mode: clear
          (when elcity-player--preview-tile
            (elcity-player-clear-preview))
        ;; Same tile: skip
        (unless (equal coords elcity-player--preview-tile)
          (elcity-player-clear-preview)
          (setq elcity-player--preview-tile coords)
          (let* ((tiles (elcity-player--footprint-tiles
                         tool (car coords) (cdr coords)))
                 (state (elcity-player-session-state
                         elcity-player--session))
                 (world (elcity-state-world state))
                 (width (elcity-world-map-width world))
                 (height (elcity-world-map-height world))
                 (graphical (elcity-player-render-tile-view-available-p))
                 (char-width (if graphical 2 1)))
            (dolist (tc tiles)
              (let ((tx (car tc)) (ty (cdr tc)))
                (when (and (>= tx 0) (< tx width)
                           (>= ty 0) (< ty height))
                  (let ((pos (elcity-player-tile-to-pos tx ty)))
                    (when pos
                      (if graphical
                          ;; Image mode: replace with border-highlighted tile
                          (let ((hi (elcity-player--tile-highlight-image pos)))
                            (when hi
                              (let ((ov (make-overlay pos (+ pos char-width))))
                                (overlay-put ov 'display hi)
                                (overlay-put ov 'elcity-player-preview t)
                                (push ov elcity-player--preview-overlays))))
                        ;; Text mode: face highlight
                        (let ((ov (make-overlay pos (+ pos char-width))))
                          (overlay-put ov 'face 'elcity-player-preview-face)
                          (overlay-put ov 'elcity-player-preview t)
                          (push ov elcity-player--preview-overlays))))))))))))))

;;; ---------- Tool selection ----------

(defun elcity-player-select-tool (tool-kind)
  "Select TOOL-KIND as the active building tool."
  (setf (elcity-player-session-active-tool elcity-player--session)
        tool-kind)
  (elcity-player-clear-preview)
  (elcity-player-ui-render-chrome elcity-player--session))

(defmacro elcity-player--define-tool-commands ()
  "Generate `elcity-player-select-KIND' commands from tool specs."
  `(progn
     ,@(mapcar
        (lambda (spec)
          (let* ((kind (car spec))
                 (plist (cdr spec))
                 (name (plist-get plist :name))
                 (cmd (intern (format "elcity-player-select-%s" kind))))
            `(defun ,cmd ()
               ,(format "Select %s tool." (downcase name))
               (interactive)
               (elcity-player-select-tool ',kind))))
        elcity-player-tool-specs)))

(elcity-player--define-tool-commands)

(defun elcity-player-next-tool ()
  "Cycle to next tool in `elcity-player-tool-specs'."
  (interactive)
  (let* ((current (elcity-player-session-active-tool elcity-player--session))
         (kinds (elcity-player-tool-kinds))
         (tail (cdr (memq current kinds)))
         (next (or (car tail) (car kinds))))
    (elcity-player-select-tool next)))

(defun elcity-player-prev-tool ()
  "Cycle to previous tool in `elcity-player-tool-specs'."
  (interactive)
  (let* ((current (elcity-player-session-active-tool elcity-player--session))
         (kinds (elcity-player-tool-kinds))
         (pos (cl-position current kinds))
         (prev (nth (if (zerop pos) (1- (length kinds)) (1- pos)) kinds)))
    (elcity-player-select-tool prev)))

;;; ---------- Placement ----------

(defun elcity-player--building-anchor (tool tile-x tile-y)
  "Translate cursor coords TILE-X, TILE-Y to engine anchor for TOOL.
The UI always uses center-based coordinates.  Building stamps that the
engine expects as top-left are offset accordingly.
Zones and 1x1 tools (roads, powerlines, bulldoze) already use the
cursor position directly in the engine, so no offset is needed."
  (let ((engine-anchor (elcity-player-tool-engine-anchor tool)))
    (if (eq engine-anchor 'top-left)
        (let* ((fp (elcity-player-tool-footprint tool))
               (size (plist-get fp :size)))
          (cons (- tile-x (/ size 2))
                (- tile-y (/ size 2))))
      (cons tile-x tile-y))))

(defun elcity-player-inspect-at (tile-x tile-y)
  "Display inspect tooltip for tile at TILE-X, TILE-Y.
Non-mutating: reads tile info and shows it via `message'.
Returns the tooltip string, or nil for unrecognized tiles."
  (let* ((state (elcity-player-session-state elcity-player--session))
         (tooltip (elcity-player-ui-tile-tooltip state nil tile-x tile-y nil nil)))
    (when tooltip
      (message "%s" tooltip))
    tooltip))

(defun elcity-player-place-at (tile-x tile-y)
  "Place active tool at TILE-X, TILE-Y in current session.
TILE-X, TILE-Y are center-based cursor coordinates.
In inspect mode, displays tile info instead of placing.
Return the cost on success, nil in inspect mode.
Signal error on rejection."
  (let ((tool (elcity-player-session-active-tool elcity-player--session)))
    (if (eq tool 'inspect)
        (progn (elcity-player-inspect-at tile-x tile-y) nil)
      (let* ((anchor (elcity-player--building-anchor tool tile-x tile-y))
             (ax (car anchor))
             (ay (cdr anchor))
             (state (elcity-player-session-state elcity-player--session))
             (treasury-before (elcity-state-api-treasury state))
             (new-state (elcity-building-place tool ax ay state))
             (cost (- treasury-before (elcity-state-api-treasury new-state))))
        ;; Push pre-placement state onto undo ring (placement succeeded)
        (push state elcity-player--undo-ring)
        (when (> (length elcity-player--undo-ring) elcity-player-undo-max)
          (setq elcity-player--undo-ring
                (seq-take elcity-player--undo-ring elcity-player-undo-max)))
        (setf (elcity-player-session-state elcity-player--session) new-state)
        (elcity-player-ui-render elcity-player--session)
        (message "%s placed ($%d, treasury: $%d)"
                 (elcity-player-tool-display-name tool)
                 cost
                 (elcity-state-api-treasury new-state))
        cost))))

;;; ---------- Mouse handlers ----------

(defun elcity-player-mouse-ignore (event)
  "Silently ignore mouse EVENT without moving point."
  (interactive "e")
  (ignore event))

(defun elcity-player-place-at-mouse (event)
  "Place active tool at the tile coordinate under mouse EVENT."
  (interactive "e")
  (let* ((posn (event-start event))
         (window (posn-window posn))
         (x-pixel (car (posn-x-y posn)))
         (y-pixel (cdr (posn-x-y posn)))
         (map-start elcity-player--map-start-pos))
    ;; Ignore clicks outside the map region
    (when (and map-start x-pixel y-pixel)
      (with-selected-window window
        ;; Compute pixel offset of map start in window
        (let ((map-posn (posn-at-point map-start)))
          (when map-posn
            (let* ((map-y-pixel (cdr (posn-x-y map-posn)))
                   (tile-pixel-size (* elcity-player-render-tile-pixel-size
                                      (elcity-player-render-tile-scale)))
                   (tile-x (floor (/ (float x-pixel) tile-pixel-size)))
                   (tile-y (floor (/ (float (- y-pixel map-y-pixel))
                                     tile-pixel-size)))
                   (state (elcity-player-session-state elcity-player--session))
                   (world (elcity-state-world state))
                   (width (elcity-world-map-width world))
                   (height (elcity-world-map-height world)))
              ;; Validate bounds
              (when (and (>= tile-x 0) (< tile-x width)
                         (>= tile-y 0) (< tile-y height))
                (condition-case err
                    (elcity-player-place-at tile-x tile-y)
                  (error
                   (message "%s" (error-message-string err))))))))))))

(defun elcity-player-place-at-point ()
  "Place active tool at the tile under point."
  (interactive)
  (let ((coords (elcity-player-tile-at-point)))
    (if coords
        (condition-case err
            (elcity-player-place-at (car coords) (cdr coords))
          (error
           (message "%s" (error-message-string err))))
      (user-error "Cursor is not on the map"))))

;;; ---------- Budget controls ----------

(defun elcity-player-cycle-budget-control ()
  "Cycle active budget control: tax -> infrastructure -> fire -> police -> ..."
  (interactive)
  (let* ((current (elcity-player-session-budget-control elcity-player--session))
         (rest (cdr (memq current elcity-player-budget-control-cycle)))
         (next (or (car rest) (car elcity-player-budget-control-cycle))))
    (setf (elcity-player-session-budget-control elcity-player--session) next)
    (elcity-player-ui-render-chrome elcity-player--session)
    (message "Budget control: %s" next)))

(defun elcity-player-budget-increase ()
  "Increase active budget control: tax rate or service funding percent."
  (interactive)
  (let ((ctl (elcity-player-session-budget-control elcity-player--session)))
    (if (eq ctl 'tax)
        (let* ((current (elcity-player-effective-tax-rate))
               (new-rate (min elcity-budget-tax-rate-max
                             (+ current elcity-player-tax-rate-step))))
          (elcity-player-enqueue-action (elcity-action-tax-rate new-rate))
          (elcity-player-ui-render-chrome elcity-player--session)
          (message "Tax rate: %d%% (pending)" new-rate))
      (let* ((current (elcity-player-effective-budget-percent ctl))
             (new-pct (min 100 (+ current elcity-player-budget-percent-step))))
        (elcity-player-enqueue-action
         (elcity-action-budget-percent ctl new-pct))
        (elcity-player-ui-render-chrome elcity-player--session)
        (message "Budget %s: %d%% (pending)" ctl new-pct)))))

(defun elcity-player-budget-decrease ()
  "Decrease active budget control: tax rate or service funding percent."
  (interactive)
  (let ((ctl (elcity-player-session-budget-control elcity-player--session)))
    (if (eq ctl 'tax)
        (let* ((current (elcity-player-effective-tax-rate))
               (new-rate (max elcity-budget-tax-rate-min
                             (- current elcity-player-tax-rate-step))))
          (elcity-player-enqueue-action (elcity-action-tax-rate new-rate))
          (elcity-player-ui-render-chrome elcity-player--session)
          (message "Tax rate: %d%% (pending)" new-rate))
      (let* ((current (elcity-player-effective-budget-percent ctl))
             (new-pct (max 0 (- current elcity-player-budget-percent-step))))
        (elcity-player-enqueue-action
         (elcity-action-budget-percent ctl new-pct))
        (elcity-player-ui-render-chrome elcity-player--session)
        (message "Budget %s: %d%% (pending)" ctl new-pct)))))

;;; ---------- Save/load ----------

(defun elcity-player-save-game (file)
  "Save current game state to FILE."
  (interactive
   (list
    (read-file-name "Save game to: "
                    default-directory
                    nil
                    nil
                    "elcity-save.elcity")))
  (unless (elcity-player-session-p elcity-player--session)
    (user-error "No active player session"))
  (let ((path (elcity-persist-save-game
               (elcity-player-session-state elcity-player--session)
               file)))
    (message "Game saved: %s" path)))

(defun elcity-player-load-game (file)
  "Load game state from FILE into the current player session."
  (interactive
   (list
    (read-file-name "Load game from: "
                    default-directory
                    nil
                    t)))
  (unless (elcity-player-session-p elcity-player--session)
    (user-error "No active player session"))
  (let ((state (elcity-persist-load-game file)))
    ;; Reset volatile UI/session state to keep post-load behavior deterministic.
    (setf (elcity-player-session-state elcity-player--session) state
          (elcity-player-session-running elcity-player--session) nil)
    (setq elcity-player--pending-actions nil
          elcity-player--undo-ring nil)
    (elcity-player-session-sync-timer)
    (elcity-player-ui-render elcity-player--session)
    (message "Game loaded: %s" (expand-file-name file))))

;;; ---------- Undo ----------

(defun elcity-player-undo ()
  "Undo the last building placement.
Pop the most recent state from the undo ring and restore it.
Signal `user-error' if the ring is empty."
  (interactive)
  (unless elcity-player--undo-ring
    (user-error "Nothing to undo"))
  (let ((prev-state (pop elcity-player--undo-ring)))
    (setf (elcity-player-session-state elcity-player--session) prev-state)
    (elcity-player-ui-render elcity-player--session)
    (message "Undo (treasury: $%d, %d more)"
             (elcity-state-api-treasury prev-state)
             (length elcity-player--undo-ring))))

;;; ---------- Quit ----------

(defun elcity-player-quit ()
  "Quit player by killing the buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(provide 'elcity-player-input)

;;; elcity-player-input.el ends here
