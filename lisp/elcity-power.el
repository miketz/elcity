;;; elcity-power.el --- S2 power-network primitives -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; Pure S2 helpers for power-network behavior.
;;
;; This module owns:
;; - tile-resolution power-grid storage,
;; - source detection and capacity budgeting,
;; - deterministic conductive traversal,
;; - zone-level powered/unpowered summaries.

;;; Code:

(require 'cl-lib)
(require 'elcity-tile)
(require 'elcity-tile-family)
(require 'elcity-world)

(defconst elcity-power-coal-plant-index
  (elcity-tile-family-center-index 'coal-plant)
  "Tile index used for coal power plant source detection.
Derived from the canonical family registry.")

(defconst elcity-power-nuclear-plant-index
  (elcity-tile-family-center-index 'nuclear-plant)
  "Tile index used for nuclear power plant source detection.
Derived from the canonical family registry.")

(defconst elcity-power-coal-capacity 400
  "Per-tick powered-tile budget contributed by one coal plant.")

(defconst elcity-power-nuclear-capacity 1200
  "Per-tick powered-tile budget contributed by one nuclear plant.")

(defconst elcity-power-unpowered-zone-penalty 500
  "Penalty magnitude applied per unpowered zone center.")

(cl-defstruct (elcity-power-grid
               (:constructor elcity-power-grid-create))
  "Tile-resolution powered-state map.
WIDTH and HEIGHT are tile dimensions.  CELLS is a row-major vector of boolean
values where non-nil means powered."
  (width 0 :type integer)
  (height 0 :type integer)
  (cells [] :type vector))

(cl-defstruct (elcity-power-source
               (:constructor elcity-power-source-create))
  "Power source entry discovered from world tiles."
  (x 0 :type integer)
  (y 0 :type integer)
  (kind nil :type symbol)
  (capacity 0 :type integer))

(cl-defstruct (elcity-power-scan
               (:constructor elcity-power-scan-create))
  "Result bundle for one deterministic power-network scan."
  (grid nil :type elcity-power-grid)
  (sources nil :type list)
  (capacity 0 :type integer)
  (powered-tiles 0 :type integer)
  (overloaded nil :type boolean))

(cl-defstruct (elcity-zone-power-summary
               (:constructor elcity-zone-power-summary-create))
  "Zone-level powered/unpowered aggregate metrics."
  (powered 0 :type integer)
  (unpowered 0 :type integer)
  (ratio 1.0 :type number))

(defun elcity-power-grid-make (width height &optional initial-powered)
  "Return a new power grid for WIDTH and HEIGHT.
When INITIAL-POWERED is non-nil, all cells start powered."
  (cl-check-type width integer)
  (cl-check-type height integer)
  (when (or (<= width 0)
            (<= height 0))
    (error "WIDTH and HEIGHT must be positive"))
  (elcity-power-grid-create
   :width width
   :height height
   :cells (make-vector (* width height) initial-powered)))

(defun elcity-power-grid-for-world (world &optional initial-powered)
  "Return a new power grid sized for WORLD.
When INITIAL-POWERED is non-nil, all cells start powered."
  (cl-check-type world elcity-world-map)
  (elcity-power-grid-make (elcity-world-map-width world)
                          (elcity-world-map-height world)
                          initial-powered))

(defun elcity-power-grid-in-bounds-p (grid x y)
  "Return non-nil when X,Y are valid tile coordinates in GRID."
  (cl-check-type grid elcity-power-grid)
  (cl-check-type x integer)
  (cl-check-type y integer)
  (and (<= 0 x)
       (< x (elcity-power-grid-width grid))
       (<= 0 y)
       (< y (elcity-power-grid-height grid))))

(defun elcity-power-grid--cell-index (grid x y)
  "Return linear row-major index in GRID for X,Y, else nil."
  (when (elcity-power-grid-in-bounds-p grid x y)
    (+ x
       (* y (elcity-power-grid-width grid)))))

(defun elcity-power-grid-cell-at (grid x y &optional default)
  "Return powered flag at X,Y in GRID, or DEFAULT when out of bounds."
  (cl-check-type grid elcity-power-grid)
  (cl-check-type x integer)
  (cl-check-type y integer)
  (let ((index (elcity-power-grid--cell-index grid x y)))
    (if index
        (aref (elcity-power-grid-cells grid) index)
      default)))

(defun elcity-power-grid-with-cells (grid coords powered)
  "Return GRID with COORDS set to POWERED in one copied update."
  (cl-check-type grid elcity-power-grid)
  (cl-check-type coords list)
  (let ((cells (copy-sequence (elcity-power-grid-cells grid)))
        (next (copy-elcity-power-grid grid)))
    (dolist (coord coords)
      (let ((index (elcity-power-grid--cell-index grid (car coord) (cdr coord))))
        (when index
          (aset cells index powered))))
    (setf (elcity-power-grid-cells next) cells)
    next))

(defun elcity-power-source-kind-for-index (tile-index)
  "Return source kind symbol for TILE-INDEX, or nil when not a source."
  (cl-check-type tile-index integer)
  (cond
   ((= tile-index elcity-power-coal-plant-index) 'coal)
   ((= tile-index elcity-power-nuclear-plant-index) 'nuclear)))

(defun elcity-power-source-capacity-for-kind (kind)
  "Return per-source capacity for KIND."
  (cond
   ((eq kind 'coal) elcity-power-coal-capacity)
   ((eq kind 'nuclear) elcity-power-nuclear-capacity)
   (t (error "Unknown power source kind: %S" kind))))

(defun elcity-power-source-at-tile (world x y)
  "Return source struct at WORLD tile X,Y, or nil when not a source."
  (cl-check-type world elcity-world-map)
  (cl-check-type x integer)
  (cl-check-type y integer)
  (let* ((tile (elcity-world-tile-at world x y 0))
         (kind (elcity-power-source-kind-for-index
                (elcity-tile-index tile))))
    (when kind
      (elcity-power-source-create
       :x x
       :y y
       :kind kind
       :capacity (elcity-power-source-capacity-for-kind kind)))))

(defun elcity-power-collect-sources (world)
  "Return deterministic Y-then-X ordered source list discovered in WORLD."
  (cl-check-type world elcity-world-map)
  (let ((width (elcity-world-map-width world))
        (height (elcity-world-map-height world))
        sources)
    (dotimes (y height)
      (dotimes (x width)
        (let ((source (elcity-power-source-at-tile world x y)))
          (when source
            (push source sources)))))
    (nreverse sources)))

(defun elcity-power-total-capacity (sources)
  "Return total per-tick generation capacity for SOURCES."
  (cl-check-type sources list)
  (let ((capacity 0))
    (dolist (source sources capacity)
      (cl-check-type source elcity-power-source)
      (setq capacity (+ capacity
                        (elcity-power-source-capacity source))))))

(defun elcity-power--conductive-coordinate-p (world x y)
  "Return non-nil when WORLD tile X,Y is in bounds and conductive."
  (and (elcity-world-in-bounds-p world x y)
       (elcity-tile-conductive-p (elcity-world-tile-at world x y 0))))

(defun elcity-power-scan-conductive-network (world sources capacity)
  "Return `elcity-power-scan' for WORLD using SOURCES and CAPACITY.
Traversal is deterministic and cardinal.  Overload is signaled when more
reachable conductive tiles exist than CAPACITY allows."
  (cl-check-type world elcity-world-map)
  (cl-check-type sources list)
  (cl-check-type capacity integer)
  (when (< capacity 0)
    (error "CAPACITY must be non-negative"))
  (let* ((grid (elcity-power-grid-for-world world nil))
         (cells (elcity-power-grid-cells grid))
         (width (elcity-power-grid-width grid))
         (height (elcity-power-grid-height grid))
         (seen (make-vector (length cells) nil))
         (queue nil)
         (tail nil)
         (powered-tiles 0))
    ;; Deterministic BFS:
    ;; 1) Seed queue from conductive source tiles.
    ;; 2) Visit cardinal-connected conductive tiles.
    ;; 3) Stop after CAPACITY visits; remaining queue means overload.
    (cl-labels
        ((cell-index (x y)
           (+ x (* y width)))
         (enqueue (x y)
           (when (elcity-power--conductive-coordinate-p world x y)
             (let ((index (cell-index x y)))
               (unless (aref seen index)
                 (aset seen index t)
                 (let ((node (list (cons x y))))
                   (if tail
                       (setcdr tail node)
                     (setq queue node))
                   (setq tail node))))))
         (dequeue ()
           (prog1 (car queue)
             (setq queue (cdr queue))
             (when (null queue)
               (setq tail nil)))))
      (dolist (source sources)
        (cl-check-type source elcity-power-source)
        (enqueue (elcity-power-source-x source)
                 (elcity-power-source-y source)))
      (while (and queue
                  (< powered-tiles capacity))
        (let* ((coord (dequeue))
               (x (car coord))
               (y (cdr coord))
               (index (cell-index x y)))
          (aset cells index t)
          (setq powered-tiles (1+ powered-tiles))
          (dolist (neighbor (elcity-cardinal-neighbors x y width height))
            (enqueue (car neighbor) (cdr neighbor)))))
    (elcity-power-scan-create
     :grid grid
     :sources sources
     :capacity capacity
     :powered-tiles powered-tiles
     :overloaded (and queue
                      (>= powered-tiles capacity))))))

(defun elcity-power-capacity-permille (infra-effective)
  "Return power capacity permille for INFRA-EFFECTIVE funding level.
Maps infrastructure effectiveness [0..1000] to capacity permille [500..1000].
Linear scaling: 0% funding yields 50% capacity, 100% yields full capacity."
  (cl-check-type infra-effective integer)
  (+ 500 (/ infra-effective 2)))

(defun elcity-power-effective-capacity (rated-capacity infra-effective)
  "Return effective power capacity from RATED-CAPACITY and INFRA-EFFECTIVE.
Applies linear funding scaling: capacity = rated * permille / 1000.
When RATED-CAPACITY is zero, returns zero."
  (cl-check-type rated-capacity integer)
  (cl-check-type infra-effective integer)
  (/ (* rated-capacity (elcity-power-capacity-permille infra-effective))
     1000))

(defun elcity-power-scan-world (world)
  "Return deterministic power scan result for WORLD."
  (cl-check-type world elcity-world-map)
  (let* ((sources (elcity-power-collect-sources world))
         (capacity (elcity-power-total-capacity sources)))
    (elcity-power-scan-conductive-network world sources capacity)))

(defun elcity-power-summarize-zones (world grid)
  "Return powered/unpowered zone-center summary for WORLD and GRID.
For zero zone centers, ratio defaults to 1.0."
  (cl-check-type world elcity-world-map)
  (cl-check-type grid elcity-power-grid)
  (let ((width (elcity-world-map-width world))
        (height (elcity-world-map-height world))
        (powered 0)
        (unpowered 0))
    (dotimes (y height)
      (dotimes (x width)
        (let ((tile (elcity-world-tile-at world x y 0)))
          (when (elcity-tile-zone-center-p tile)
            (if (elcity-power-grid-cell-at grid x y nil)
                (setq powered (1+ powered))
              (setq unpowered (1+ unpowered)))))))
    (let ((total (+ powered unpowered)))
      (elcity-zone-power-summary-create
       :powered powered
       :unpowered unpowered
       :ratio (if (= total 0)
                  1.0
                (/ (float powered) total))))))

(defun elcity-power-zone-penalty (zone-summary)
  "Return integer penalty for ZONE-SUMMARY."
  (cl-check-type zone-summary elcity-zone-power-summary)
  (* elcity-power-unpowered-zone-penalty
     (elcity-zone-power-summary-unpowered zone-summary)))

(provide 'elcity-power)

;;; elcity-power.el ends here
