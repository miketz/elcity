;;; elcity-building.el --- S11 building tools -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; Pure-function building tools for player-facing tile placement.
;;
;; Each tool takes (tool-kind, anchor-x, anchor-y, state) and returns an
;; S10-compatible action batch of `tile-update' actions, or signals a
;; rejection error.  The caller feeds returned actions into `elcity-step'.
;;
;; Public API:
;;   (elcity-building-apply TOOL-KIND ANCHOR-X ANCHOR-Y STATE)
;;     -> (list :actions ACTION-BATCH :footprint-size N)  ; success
;;     -> (error "Rejection reason")                       ; failure

;;; Code:

(require 'cl-lib)
(require 'elcity-actions)
(require 'elcity-state-api)
(require 'elcity-tile)
(require 'elcity-tile-family)
(require 'elcity-traffic)
(require 'elcity-world)
(require 'elcity-zones)
(require 'elcity-power)

;;; Constants

(defconst elcity-building-water-index 1
  "Tile index for water.  Placement is never allowed on water tiles.")

(defconst elcity-building-empty-index 0
  "Tile index for empty land.")

(defconst elcity-building-fire-station-center-index
  (elcity-tile-family-center-index 'fire-station)
  "Tile index for fire station center tile.
Derived from the canonical family registry.")

(defconst elcity-building-police-station-center-index
  (elcity-tile-family-center-index 'police-station)
  "Tile index for police station center tile.
Derived from the canonical family registry.")

(defconst elcity-building-park-center-index
  (elcity-tile-family-center-index 'park)
  "Tile index for park center tile (1x1 green space).
Derived from the canonical family registry.")

(defun elcity-building-position-index (kind dx dy)
  "Return tile index for building KIND at stamp offset DX, DY.
KIND is one of `coal-plant', `nuclear-plant', `fire-station',
`police-station'.  DX, DY are 0-based offsets from the top-left
anchor.  Uses contiguous row-major encoding via the family registry."
  (elcity-tile-family-position-index kind dx dy))

(defun elcity-building-footprint-index-p (index kind)
  "Return non-nil when INDEX belongs to building KIND's footprint range.
KIND is one of `coal-plant', `nuclear-plant', `fire-station',
`police-station'.  Uses contiguous range check via the family registry.
Returns nil for unrecognized KIND values."
  (elcity-tile-family-member-p kind index))

(defconst elcity-building-tool-kinds
  '(zone-residential zone-commercial zone-industrial
    road powerline
    coal-plant nuclear-plant fire-station police-station
    park
    bulldoze)
  "All recognized building tool kinds.")

;;; Tool kind validation

(defun elcity-building--check-tool-kind (tool-kind)
  "Signal an error when TOOL-KIND is not a recognized building tool."
  (unless (memq tool-kind elcity-building-tool-kinds)
    (error "Unknown building tool kind: %S" tool-kind)))

;;; Footprint geometry

(defun elcity-building--stamp-footprint (anchor-x anchor-y size)
  "Return list of (X . Y) coordinates for a SIZE x SIZE stamp.
ANCHOR-X, ANCHOR-Y is the top-left corner of the stamp."
  (let (coords)
    (dotimes (dy size)
      (dotimes (dx size)
        (push (cons (+ anchor-x dx) (+ anchor-y dy)) coords)))
    (nreverse coords)))

;;; Validation

(defun elcity-building--validate-bounds (world coords)
  "Signal error if any coordinate in COORDS is out of bounds in WORLD."
  (dolist (coord coords)
    (unless (elcity-world-in-bounds-p world (car coord) (cdr coord))
      (error "Building placement out of bounds at (%d,%d)"
             (car coord) (cdr coord)))))

(defun elcity-building--validate-water (world coords)
  "Signal error if any coordinate in COORDS is a water tile in WORLD."
  (dolist (coord coords)
    (let* ((tile (elcity-world-tile-at world (car coord) (cdr coord) 0))
           (index (elcity-tile-index tile)))
      (when (= index elcity-building-water-index)
        (error "Building placement on water at (%d,%d)"
               (car coord) (cdr coord))))))

(defun elcity-building--validate-stamp-overlap (world coords)
  "Signal error if any coordinate in COORDS has a non-empty tile in WORLD."
  (dolist (coord coords)
    (let* ((tile (elcity-world-tile-at world (car coord) (cdr coord) 0))
           (index (elcity-tile-index tile)))
      (when (/= index elcity-building-empty-index)
        (error "Building placement overlaps existing tile at (%d,%d)"
               (car coord) (cdr coord))))))

(defun elcity-building--validate-infrastructure-overlap (world x y tool-kind)
  "Signal error if tile at X,Y in WORLD is incompatible with TOOL-KIND.
Infrastructure tools (road, powerline) may be placed on:
- Empty tiles.
- Tiles of the same kind (re-tile).
- Tiles of the opposite kind (crossing merge).
- Existing crossings (no-op)."
  (let* ((tile (elcity-world-tile-at world x y 0))
         (index (elcity-tile-index tile)))
    (unless (or (= index elcity-building-empty-index)
                (elcity-traffic-road-index-p index)
                (elcity-traffic-powerline-index-p index)
                (elcity-traffic-crossing-index-p index))
      (error "Cannot place %s on non-empty tile at (%d,%d)"
             tool-kind x y))))

(defun elcity-building--validate-stamp (world coords)
  "Run all stamp validations for COORDS in WORLD."
  (elcity-building--validate-bounds world coords)
  (elcity-building--validate-water world coords)
  (elcity-building--validate-stamp-overlap world coords))

(defun elcity-building--validate-infrastructure (world x y tool-kind)
  "Run all infrastructure validations for X,Y in WORLD for TOOL-KIND."
  (unless (elcity-world-in-bounds-p world x y)
    (error "Building placement out of bounds at (%d,%d)" x y))
  (let* ((tile (elcity-world-tile-at world x y 0))
         (index (elcity-tile-index tile)))
    (when (= index elcity-building-water-index)
      (error "Building placement on water at (%d,%d)" x y)))
  (elcity-building--validate-infrastructure-overlap world x y tool-kind))

;;; Stamp helper

(defun elcity-building--make-tile (index &rest flags)
  "Return a tile with INDEX and FLAGS set.
FLAGS are flag constants like `elcity-tile-condbit'."
  (let ((tile (elcity-tile-with-index 0 index)))
    (dolist (flag flags)
      (setq tile (logior tile flag)))
    tile))

(defun elcity-building--stamp-actions (anchor-x anchor-y size
                                       center-offset-x center-offset-y
                                       center-index center-flags
                                       border-index border-flags)
  "Generate tile-update actions for an NxN stamp.
ANCHOR-X, ANCHOR-Y is the top-left corner.  SIZE is the stamp dimension.
CENTER-OFFSET-X, CENTER-OFFSET-Y is the center tile offset from anchor.
CENTER-INDEX and CENTER-FLAGS define the center tile.
BORDER-INDEX is either an integer (single index for all borders) or a
function (DX DY) returning the index for each border position.
BORDER-FLAGS define all border tiles."
  (let ((center-tile (elcity-building--make-tile center-index))
        (border-index-fn (if (functionp border-index)
                             border-index
                           (lambda (_dx _dy) border-index)))
        actions)
    (dolist (flag center-flags)
      (setq center-tile (logior center-tile flag)))
    (dotimes (dy size)
      (dotimes (dx size)
        (let ((x (+ anchor-x dx))
              (y (+ anchor-y dy)))
          (if (and (= dx center-offset-x)
                   (= dy center-offset-y))
              (push (elcity-action-tile-update x y center-tile) actions)
            (let ((border-tile (elcity-building--make-tile
                                (funcall border-index-fn dx dy))))
              (dolist (flag border-flags)
                (setq border-tile (logior border-tile flag)))
              (push (elcity-action-tile-update x y border-tile) actions))))))
    (nreverse actions)))

;;; Auto-tile helpers

(defun elcity-building-cardinal-mask (state x y predicate)
  "Return 4-bit cardinal mask for tile at X,Y matching PREDICATE in STATE.
Bits: N=1 E=2 S=4 W=8.  PREDICATE receives the full tile integer
\(not just the index) so it can inspect status flags."
  (let ((mask 0))
    (let ((n (elcity-state-api-tile-at state x (1- y))))
      (when (and n (funcall predicate n))
        (setq mask (logior mask 1))))
    (let ((e (elcity-state-api-tile-at state (1+ x) y)))
      (when (and e (funcall predicate e))
        (setq mask (logior mask 2))))
    (let ((s (elcity-state-api-tile-at state x (1+ y))))
      (when (and s (funcall predicate s))
        (setq mask (logior mask 4))))
    (let ((w (elcity-state-api-tile-at state (1- x) y)))
      (when (and w (funcall predicate w))
        (setq mask (logior mask 8))))
    mask))

(defun elcity-building-road-neighbor-p (tile)
  "Return non-nil when TILE is a road or crossing tile."
  (let ((index (elcity-tile-index tile)))
    (or (elcity-traffic-road-index-p index)
        (elcity-traffic-crossing-index-p index))))

(defun elcity-building-powerline-neighbor-p (tile)
  "Return non-nil when TILE is a powerline-connectable neighbor.
A tile qualifies if it is a powerline, a crossing, or a conductive
non-road tile (building, plant, station, or zone)."
  (let ((index (elcity-tile-index tile)))
    (or (elcity-traffic-powerline-index-p index)
        (elcity-traffic-crossing-index-p index)
        (and (elcity-tile-conductive-p tile)
             (not (elcity-traffic-road-index-p index))))))

(defun elcity-building--retile-road-neighbor (state x y)
  "Return tile-update action to re-tile road at X,Y in STATE, or nil.
Preserves existing flags, updates index based on cardinal mask."
  (let* ((tile (elcity-state-api-tile-at state x y))
         (index (and tile (elcity-tile-index tile))))
    (when (and index
               (elcity-traffic-road-index-p index)
               (not (elcity-traffic-crossing-index-p index)))
      (let* ((mask (elcity-building-cardinal-mask
                    state x y #'elcity-building-road-neighbor-p))
             (new-index (+ elcity-traffic-road-index-min mask))
             (new-tile (elcity-tile-with-index tile new-index)))
        (elcity-action-tile-update x y new-tile)))))

(defun elcity-building--retile-powerline-neighbor (state x y)
  "Return tile-update action to re-tile powerline at X,Y in STATE, or nil.
Preserves existing flags, updates index based on cardinal mask."
  (let* ((tile (elcity-state-api-tile-at state x y))
         (index (and tile (elcity-tile-index tile))))
    (when (and index
               (elcity-traffic-powerline-index-p index))
      (let* ((mask (elcity-building-cardinal-mask
                    state x y #'elcity-building-powerline-neighbor-p))
             (new-index (+ elcity-traffic-powerline-index-min mask))
             (new-tile (elcity-tile-with-index tile new-index)))
        (elcity-action-tile-update x y new-tile)))))

(defun elcity-building--cardinal-neighbor-retile-actions (state x y kinds)
  "Return list of tile-update actions to re-tile cardinal neighbors in STATE.
X, Y is the center coordinate.  KINDS is a list of symbols `road' and/or
`powerline' indicating which neighbor types to re-tile."
  (let (actions)
    (dolist (delta elcity-cardinal-neighbor-deltas)
      (let ((nx (+ x (car delta)))
            (ny (+ y (cdr delta))))
        (when (memq 'road kinds)
          (let ((action (elcity-building--retile-road-neighbor state nx ny)))
            (when action (push action actions))))
        (when (memq 'powerline kinds)
          (let ((action (elcity-building--retile-powerline-neighbor state nx ny)))
            (when action (push action actions))))))
    (nreverse actions)))

;;; Zone tools

(defun elcity-building--apply-zone (kind anchor-x anchor-y state)
  "Apply zone stamp of KIND centered at ANCHOR-X, ANCHOR-Y in STATE."
  (let* ((world (elcity-state-world state))
         (top-left-x (1- anchor-x))
         (top-left-y (1- anchor-y))
         (coords (elcity-building--stamp-footprint top-left-x top-left-y 3))
         (center-index (elcity-zones-center-index kind 0))
         (border-index (elcity-zones-border-index kind 0)))
    (elcity-building--validate-stamp world coords)
    (let ((actions (elcity-building--stamp-actions
                    top-left-x top-left-y 3
                    1 1
                    center-index
                    (list elcity-tile-zonebit elcity-tile-bullbit
                          elcity-tile-condbit elcity-tile-burnbit)
                    border-index
                    (list elcity-tile-bullbit elcity-tile-condbit
                          elcity-tile-burnbit))))
      (list :actions actions :footprint-size 9))))

;;; Road tool

(defun elcity-building--apply-road (x y state)
  "Apply road placement at X,Y in STATE."
  (let* ((world (elcity-state-world state)))
    (elcity-building--validate-infrastructure world x y 'road)
    (let* ((tile (elcity-world-tile-at world x y 0))
           (index (elcity-tile-index tile)))
      (cond
       ;; Road on crossing: no-op
       ((elcity-traffic-crossing-index-p index)
        (list :actions nil :footprint-size 1))
       ;; Road on powerline: produce crossing
       ((elcity-traffic-powerline-index-p index)
        (let* ((crossing-tile (elcity-building--make-tile
                               elcity-traffic-crossing-index
                               elcity-tile-condbit))
               (place-action (elcity-action-tile-update x y crossing-tile))
               (retile-actions (elcity-building--cardinal-neighbor-retile-actions
                                state x y '(road powerline))))
          (list :actions (cons place-action retile-actions) :footprint-size 1)))
       ;; Road on road or empty: compute auto-tile
       (t
        (let* ((mask (elcity-building-cardinal-mask
                      state x y #'elcity-building-road-neighbor-p))
               (new-index (+ elcity-traffic-road-index-min mask))
               (new-tile (elcity-building--make-tile new-index))
               (place-action (elcity-action-tile-update x y new-tile))
               (retile-actions (elcity-building--cardinal-neighbor-retile-actions
                                state x y '(road))))
          (list :actions (cons place-action retile-actions) :footprint-size 1)))))))

;;; Powerline tool

(defun elcity-building--apply-powerline (x y state)
  "Apply powerline placement at X,Y in STATE."
  (let* ((world (elcity-state-world state)))
    (elcity-building--validate-infrastructure world x y 'powerline)
    (let* ((tile (elcity-world-tile-at world x y 0))
           (index (elcity-tile-index tile)))
      (cond
       ;; Powerline on crossing: no-op
       ((elcity-traffic-crossing-index-p index)
        (list :actions nil :footprint-size 1))
       ;; Powerline on road: produce crossing
       ((elcity-traffic-road-index-p index)
        (let* ((crossing-tile (elcity-building--make-tile
                               elcity-traffic-crossing-index
                               elcity-tile-condbit))
               (place-action (elcity-action-tile-update x y crossing-tile))
               (retile-actions (elcity-building--cardinal-neighbor-retile-actions
                                state x y '(road powerline))))
          (list :actions (cons place-action retile-actions) :footprint-size 1)))
       ;; Powerline on powerline or empty: compute auto-tile
       (t
        (let* ((mask (elcity-building-cardinal-mask
                      state x y #'elcity-building-powerline-neighbor-p))
               (new-index (+ elcity-traffic-powerline-index-min mask))
               (new-tile (elcity-building--make-tile new-index elcity-tile-condbit))
               (place-action (elcity-action-tile-update x y new-tile))
               (retile-actions (elcity-building--cardinal-neighbor-retile-actions
                                state x y '(powerline))))
          (list :actions (cons place-action retile-actions) :footprint-size 1)))))))

;;; Building stamp tools (plants, stations)

(defun elcity-building--apply-building-stamp (anchor-x anchor-y size
                                              center-index kind state)
  "Apply a building stamp at ANCHOR-X, ANCHOR-Y with SIZE and CENTER-INDEX.
KIND is the building kind symbol for per-position index computation.
STATE provides the world for validation.  Anchor is the top-left corner.
Center offset is kind-dependent: (2,2) for nuclear, (1,1) for others."
  (let* ((world (elcity-state-world state))
         (coords (elcity-building--stamp-footprint anchor-x anchor-y size))
         (center-offset (if (eq kind 'nuclear-plant) 2 1)))
    (elcity-building--validate-stamp world coords)
    (let ((actions (elcity-building--stamp-actions
                    anchor-x anchor-y size
                    center-offset center-offset
                    center-index
                    (list elcity-tile-condbit)
                    (lambda (dx dy)
                      (elcity-building-position-index kind dx dy))
                    (list elcity-tile-condbit elcity-tile-bullbit))))
      (list :actions actions :footprint-size (* size size)))))

;;; Station stamp tools (fire, police — with burnbit)

(defun elcity-building--apply-fire-station (anchor-x anchor-y state)
  "Apply fire station stamp at ANCHOR-X, ANCHOR-Y in STATE.
Anchor is the top-left corner.  Sets burnbit on all tiles."
  (let* ((world (elcity-state-world state))
         (coords (elcity-building--stamp-footprint anchor-x anchor-y 3)))
    (elcity-building--validate-stamp world coords)
    (let ((actions (elcity-building--stamp-actions
                    anchor-x anchor-y 3
                    1 1
                    elcity-building-fire-station-center-index
                    (list elcity-tile-condbit elcity-tile-burnbit)
                    (lambda (dx dy)
                      (elcity-building-position-index 'fire-station dx dy))
                    (list elcity-tile-condbit elcity-tile-bullbit
                          elcity-tile-burnbit))))
      (list :actions actions :footprint-size 9))))

(defun elcity-building--apply-police-station (anchor-x anchor-y state)
  "Apply police station stamp at ANCHOR-X, ANCHOR-Y in STATE.
Anchor is the top-left corner.  Sets burnbit on all tiles."
  (let* ((world (elcity-state-world state))
         (coords (elcity-building--stamp-footprint anchor-x anchor-y 3)))
    (elcity-building--validate-stamp world coords)
    (let ((actions (elcity-building--stamp-actions
                    anchor-x anchor-y 3
                    1 1
                    elcity-building-police-station-center-index
                    (list elcity-tile-condbit elcity-tile-burnbit)
                    (lambda (dx dy)
                      (elcity-building-position-index 'police-station dx dy))
                    (list elcity-tile-condbit elcity-tile-bullbit
                          elcity-tile-burnbit))))
      (list :actions actions :footprint-size 9))))

;;; Park tool (1x1)

(defun elcity-building--apply-park (x y state)
  "Apply park placement at X,Y in STATE.
Park is a 1x1 buildable with bullbit only (not conductive)."
  (let* ((world (elcity-state-world state))
         (coords (list (cons x y))))
    (elcity-building--validate-stamp world coords)
    (let ((park-tile (elcity-building--make-tile
                      elcity-building-park-center-index
                      elcity-tile-bullbit)))
      (list :actions (list (elcity-action-tile-update x y park-tile))
            :footprint-size 1))))

;;; Bulldoze tool

(defun elcity-building--find-zone-footprint (world x y)
  "Return 3x3 footprint coords if X,Y belongs to a zone stamp in WORLD, or nil.
Search ±1 around X,Y for a zone center."
  (catch 'found
    (let ((dx -1))
      (while (<= dx 1)
        (let ((dy -1))
          (while (<= dy 1)
            (let* ((cx (+ x dx))
                   (cy (+ y dy))
                   (tile (elcity-world-tile-at world cx cy 0)))
              (when (and tile (elcity-tile-zone-center-p tile))
                (let ((kind (elcity-zones-kind-for-index (elcity-tile-index tile))))
                  (when kind
                    ;; Zone center found; footprint is 3x3 around center
                    (throw 'found
                           (elcity-building--stamp-footprint (1- cx) (1- cy) 3))))))
            (setq dy (1+ dy))))
        (setq dx (1+ dx))))
    nil))

(defun elcity-building--find-plant-footprint (world x y)
  "Return stamp footprint coords if X,Y belongs to a plant in WORLD, or nil.
Search ±1 for coal (3x3), ±2 for nuclear (4x4)."
  (catch 'found
    ;; Search ±2 for nuclear center (4x4, center at offset 2,2)
    (let ((nuclear-center (elcity-tile-family-center-index 'nuclear-plant))
          (coal-center (elcity-tile-family-center-index 'coal-plant))
          (dx -2))
      (while (<= dx 2)
        (let ((dy -2))
          (while (<= dy 2)
            (let* ((cx (+ x dx))
                   (cy (+ y dy))
                   (tile (elcity-world-tile-at world cx cy 0))
                   (index (and tile (elcity-tile-index tile))))
              (when index
                (cond
                 ((= index nuclear-center)
                  ;; Nuclear center at cx,cy; anchor at cx-2,cy-2
                  (let ((footprint (elcity-building--stamp-footprint
                                    (- cx 2) (- cy 2) 4)))
                    ;; Verify x,y is actually in the footprint
                    (when (member (cons x y) footprint)
                      (throw 'found footprint))))
                 ((and (= index coal-center)
                       (<= (abs dx) 1) (<= (abs dy) 1))
                  ;; Coal center at cx,cy; anchor at cx-1,cy-1
                  (throw 'found
                         (elcity-building--stamp-footprint
                          (1- cx) (1- cy) 3))))))
            (setq dy (1+ dy))))
        (setq dx (1+ dx))))
    nil))

(defconst elcity-building--station-indices
  (list (elcity-tile-family-center-index 'fire-station)
        (elcity-tile-family-center-index 'police-station))
  "Station center tile indices (fire, police).")

(defun elcity-building--find-station-footprint (world x y)
  "Return 3x3 footprint coords if X,Y belongs to a station in WORLD, or nil.
Search ±1 for fire or police station center."
  (catch 'found
    (let ((dx -1))
      (while (<= dx 1)
        (let ((dy -1))
          (while (<= dy 1)
            (let* ((cx (+ x dx))
                   (cy (+ y dy))
                   (tile (elcity-world-tile-at world cx cy 0))
                   (index (and tile (elcity-tile-index tile))))
              (when (and index (memq index elcity-building--station-indices))
                (throw 'found
                       (elcity-building--stamp-footprint (1- cx) (1- cy) 3))))
            (setq dy (1+ dy))))
        (setq dx (1+ dx))))
    nil))

(defun elcity-building--detect-footprint (world x y)
  "Return footprint coordinates for tile at X,Y in WORLD.
Returns list of (X . Y) coords for the full building footprint,
or a single-element list for standalone tiles."
  (or (elcity-building--find-zone-footprint world x y)
      (elcity-building--find-plant-footprint world x y)
      (elcity-building--find-station-footprint world x y)
      (list (cons x y))))

(defun elcity-building--apply-bulldoze (x y state)
  "Apply bulldoze at X,Y in STATE."
  (let* ((world (elcity-state-world state)))
    (unless (elcity-world-in-bounds-p world x y)
      (error "Bulldoze out of bounds at (%d,%d)" x y))
    (let* ((tile (elcity-world-tile-at world x y 0))
           (index (elcity-tile-index tile)))
      (when (= index elcity-building-water-index)
        (error "Cannot bulldoze water at (%d,%d)" x y))
      (when (= index elcity-building-empty-index)
        (error "Nothing to bulldoze at (%d,%d)" x y))
      (let* ((footprint (elcity-building--detect-footprint world x y))
             (footprint-size (length footprint))
             (clear-actions
              (mapcar (lambda (coord)
                        (elcity-action-tile-update
                         (car coord) (cdr coord)
                         elcity-building-empty-index))
                      footprint))
             ;; Collect unique cardinal neighbors of all cleared tiles for re-tiling
             (neighbor-coords (make-hash-table :test 'equal)))
        (dolist (coord footprint)
          (dolist (delta elcity-cardinal-neighbor-deltas)
            (let ((nc (cons (+ (car coord) (car delta))
                            (+ (cdr coord) (cdr delta)))))
              ;; Only neighbors outside the footprint
              (unless (member nc footprint)
                (puthash nc t neighbor-coords)))))
        (let (retile-actions)
          (maphash
           (lambda (coord _)
             (let ((road-action (elcity-building--retile-road-neighbor
                                 state (car coord) (cdr coord)))
                   (power-action (elcity-building--retile-powerline-neighbor
                                  state (car coord) (cdr coord))))
               (when road-action (push road-action retile-actions))
               (when power-action (push power-action retile-actions))))
           neighbor-coords)
          (list :actions (append clear-actions retile-actions)
                :footprint-size footprint-size))))))

;;; Public API

(defun elcity-building-apply (tool-kind anchor-x anchor-y state)
  "Apply building TOOL-KIND at ANCHOR-X, ANCHOR-Y in STATE.
Return (list :actions ACTION-BATCH :footprint-size N) on success.
Signal an error on rejection.

TOOL-KIND is one of: `zone-residential', `zone-commercial',
`zone-industrial', `road', `powerline', `coal-plant',
`nuclear-plant', `fire-station', `police-station', `park',
`bulldoze'.

For zone stamps, anchor is the stamp center.
For building stamps (plants, stations), anchor is the top-left corner.
For infrastructure (road, powerline), anchor is the tile.
For bulldoze, anchor is any tile of the building to demolish."
  (elcity-building--check-tool-kind tool-kind)
  (cl-check-type anchor-x integer)
  (cl-check-type anchor-y integer)
  (cl-check-type state elcity-state)
  (pcase tool-kind
    ('zone-residential
     (elcity-building--apply-zone 'residential anchor-x anchor-y state))
    ('zone-commercial
     (elcity-building--apply-zone 'commercial anchor-x anchor-y state))
    ('zone-industrial
     (elcity-building--apply-zone 'industrial anchor-x anchor-y state))
    ('road
     (elcity-building--apply-road anchor-x anchor-y state))
    ('powerline
     (elcity-building--apply-powerline anchor-x anchor-y state))
    ('coal-plant
     (elcity-building--apply-building-stamp anchor-x anchor-y 3
                                            elcity-power-coal-plant-index
                                            'coal-plant state))
    ('nuclear-plant
     (elcity-building--apply-building-stamp anchor-x anchor-y 4
                                            elcity-power-nuclear-plant-index
                                            'nuclear-plant state))
    ('fire-station
     (elcity-building--apply-fire-station anchor-x anchor-y state))
    ('police-station
     (elcity-building--apply-police-station anchor-x anchor-y state))
    ('park
     (elcity-building--apply-park anchor-x anchor-y state))
    ('bulldoze
     (elcity-building--apply-bulldoze anchor-x anchor-y state))))

(provide 'elcity-building)

;;; elcity-building.el ends here
