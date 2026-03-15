;;; elcity-traffic.el --- S3 traffic and accessibility primitives -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; Pure S3 helpers for traffic accessibility and congestion memory.
;;
;; This module owns:
;; - deterministic road and zone classification helpers,
;; - bounded route-outcome checks for zone connectivity,
;; - congestion memory updates (increment + monotonic decay),
;; - scan/summary structs consumed by the zones stage.

;;; Code:

(require 'cl-lib)
(require 'elcity-tile-field)
(require 'elcity-tile)
(require 'elcity-world)

(defconst elcity-traffic-road-index-min 64
  "Lowest drivable tile index considered part of the traffic graph.")

(defconst elcity-traffic-road-index-max 238
  "Highest drivable tile index considered part of the traffic graph.")

(defconst elcity-traffic-powerline-index-min 208
  "Lowest power-line tile index excluded from drivable traffic graph.")

(defconst elcity-traffic-powerline-index-max 223
  "Highest power-line tile index excluded from drivable traffic graph.")

(defconst elcity-traffic-crossing-index 160
  "Tile index for road-powerline crossing.
A crossing tile is both drivable (traffic BFS) and conductive (power BFS).")

(defun elcity-traffic-crossing-index-p (tile-index)
  "Return non-nil when TILE-INDEX is a road-powerline crossing tile."
  (= tile-index elcity-traffic-crossing-index))

(defun elcity-traffic-powerline-index-p (tile-index)
  "Return non-nil when TILE-INDEX falls within the powerline tile range."
  (and (<= elcity-traffic-powerline-index-min tile-index)
       (<= tile-index elcity-traffic-powerline-index-max)))

(defconst elcity-traffic-max-route-distance 30
  "Maximum cardinal road steps explored for one accessibility query.")

(defconst elcity-traffic-congestion-step 50
  "Base congestion increment per route touch on a tile cell.
The effective increment diminishes as value approaches cap;
see `elcity-traffic-congestion-increment'.")

(defconst elcity-traffic-congestion-step-floor 8
  "Minimum per-touch increment even at high congestion values.
Ensures multi-touch cells can still rise toward cap.")

(defconst elcity-traffic-congestion-cap 240
  "Maximum congestion value per tile cell.")

(defconst elcity-traffic-congestion-low-cutoff 24
  "Congestion values at or below this threshold decay to zero.")

(defconst elcity-traffic-congestion-high-threshold 200
  "Congestion values above this threshold use heavier decay.")

(defconst elcity-traffic-congestion-low-decay 24
  "Decay amount for mid-range congestion cells.")

(defconst elcity-traffic-congestion-high-decay 34
  "Decay amount for high congestion cells.")

(defconst elcity-traffic-blocked-zone-penalty 3000
  "Penalty magnitude applied per blocked route outcome.")

(defconst elcity-traffic-no-road-zone-penalty 3000
  "Penalty magnitude applied per no-road route outcome.")

(defconst elcity-traffic-STATUS-NONE 0
  "Integer code: no route result at this tile (default).")

(defconst elcity-traffic-STATUS-NO-ROAD 1
  "Integer code: zone has no adjacent road.")

(defconst elcity-traffic-STATUS-BLOCKED 2
  "Integer code: zone has road but destination unreachable.")

(defconst elcity-traffic-STATUS-REACHABLE 3
  "Integer code: zone can reach a destination.")

(defconst elcity-traffic-perimeter-road-offsets
  '((-1 . -2) (0 . -2) (1 . -2)
    (2 . -1) (2 . 0) (2 . 1)
    (1 . 2) (0 . 2) (-1 . 2)
    (-2 . 1) (-2 . 0) (-2 . -1))
  "Deterministic perimeter offsets used for zone road-entry lookup.")

(cl-defstruct (elcity-traffic-route-result
               (:constructor elcity-traffic-route-result-create))
  "Outcome of one bounded accessibility query for one source zone center.
X and Y identify the source zone center.
SOURCE-KIND is one of `residential', `commercial', or `industrial'.
OUTCOME is one of `reachable', `blocked', or `no-road'.
DISTANCE is traversed road-step distance for successful routes, else 0.
PATH is a list of (X . Y) road-tile coordinates from source entry road to
terminal road node.  It is nil for non-success outcomes."
  (x 0 :type integer)
  (y 0 :type integer)
  (source-kind nil :type symbol)
  (outcome 'no-road :type symbol)
  (distance 0 :type integer)
  (path nil :type list))

(cl-defstruct (elcity-traffic-summary
               (:constructor elcity-traffic-summary-create))
  "Aggregate route outcomes for one zones-stage S3 pass.
REACHABLE/BLOCKED/NO-ROAD are zone counts by route outcome class.
RATIO is REACHABLE divided by total outcomes, defaulting to 1.0 when empty."
  (reachable 0 :type integer)
  (blocked 0 :type integer)
  (no-road 0 :type integer)
  (ratio 1.0 :type number))

(cl-defstruct (elcity-traffic-scan
               (:constructor elcity-traffic-scan-create))
  "Result bundle for one deterministic S3 traffic scan.
RESULTS stores route outcomes in deterministic Y-then-X source order.
SUMMARY stores aggregate outcome metrics.
CONGESTION-MAP stores the updated tile-resolution congestion memory field.
STATUS-MAP is an integer-coded tile-field for O(1) route-status lookup."
  (results nil :type list)
  (summary nil :type elcity-traffic-summary)
  (congestion-map nil :type elcity-tile-field)
  (status-map nil :type (or null elcity-tile-field)))

(defun elcity-traffic--outcome-to-status-code (outcome)
  "Return integer status code for route OUTCOME symbol."
  (pcase outcome
    ('no-road  elcity-traffic-STATUS-NO-ROAD)
    ('blocked  elcity-traffic-STATUS-BLOCKED)
    ('reachable elcity-traffic-STATUS-REACHABLE)
    (_ (error "Unknown route outcome: %S" outcome))))

(defun elcity-traffic--status-code-to-outcome (code)
  "Return route outcome symbol for integer status CODE, or nil for NONE."
  (pcase code
    ((pred (= elcity-traffic-STATUS-NONE))      nil)
    ((pred (= elcity-traffic-STATUS-NO-ROAD))   'no-road)
    ((pred (= elcity-traffic-STATUS-BLOCKED))   'blocked)
    ((pred (= elcity-traffic-STATUS-REACHABLE)) 'reachable)
    (_ (error "Unknown status code: %S" code))))

(defun elcity-traffic-road-index-p (tile-index)
  "Return non-nil when TILE-INDEX belongs to drivable road/rail traffic graph."
  (cl-check-type tile-index integer)
  (and (<= elcity-traffic-road-index-min tile-index)
       (<= tile-index elcity-traffic-road-index-max)
       (not (and (<= elcity-traffic-powerline-index-min tile-index)
                 (<= tile-index elcity-traffic-powerline-index-max)))))

(defun elcity-traffic-road-tile-p (tile)
  "Return non-nil when TILE is drivable for S3 traversal."
  (cl-check-type tile integer)
  (elcity-traffic-road-index-p (elcity-tile-index tile)))

(defun elcity-traffic-road-coordinate-p (world x y)
  "Return non-nil when WORLD tile X,Y is in bounds and drivable."
  (cl-check-type world elcity-world-map)
  (cl-check-type x integer)
  (cl-check-type y integer)
  (and (elcity-world-in-bounds-p world x y)
       (elcity-traffic-road-tile-p (elcity-world-tile-at world x y 0))))

(defun elcity-traffic-zone-kind-for-index (tile-index)
  "Return zone kind symbol for TILE-INDEX, or nil when not S3 relevant."
  (cl-check-type tile-index integer)
  (cond
   ((and (<= elcity-tile-residential-index-min tile-index)
         (< tile-index elcity-tile-commercial-index-min))
    'residential)
   ((and (<= elcity-tile-commercial-index-min tile-index)
         (< tile-index elcity-tile-industrial-index-min))
    'commercial)
   ((and (<= elcity-tile-industrial-index-min tile-index)
         (<= tile-index elcity-tile-industrial-index-max))
    'industrial)))

(defun elcity-traffic-zone-kind-at (world x y)
  "Return source-zone kind at WORLD tile X,Y, or nil.
Only zone-center tiles with a recognized S3 kind are classified."
  (cl-check-type world elcity-world-map)
  (cl-check-type x integer)
  (cl-check-type y integer)
  (when (elcity-world-in-bounds-p world x y)
    (let ((tile (elcity-world-tile-at world x y 0)))
      (when (elcity-tile-zone-center-p tile)
        (elcity-traffic-zone-kind-for-index (elcity-tile-index tile))))))

(defun elcity-traffic--zone-center-context-at-footprint-tile (world x y)
  "Return destination zone-center context for WORLD tile X,Y, or nil.
When X,Y belongs to an R/C/I 3x3 footprint, return plist
`(:kind KIND :center-x CX :center-y CY)'.  The lookup is based on nearby
zone-center tiles so roads adjacent to zone border tiles remain valid
destination touch points."
  (cl-check-type world elcity-world-map)
  (cl-check-type x integer)
  (cl-check-type y integer)
  (let* ((width (elcity-world-map-width world))
         (height (elcity-world-map-height world))
         (tile-kind (when (and (<= 0 x) (< x width)
                               (<= 0 y) (< y height))
                      (elcity-traffic-zone-kind-for-index
                       (elcity-tile-index (elcity-world-tile-at world x y 0))))))
    (when tile-kind
      (catch 'found
        (dolist (dy '(-1 0 1))
          (dolist (dx '(-1 0 1))
            (let ((cx (+ x dx))
                  (cy (+ y dy)))
              (when (and (<= 0 cx) (< cx width)
                         (<= 0 cy) (< cy height))
                (let ((kind (elcity-traffic-zone-kind-at world cx cy)))
                  (when (eq kind tile-kind)
                    (throw 'found (list :kind kind
                                        :center-x cx
                                        :center-y cy))))))))
        nil))))

(defun elcity-traffic-destination-kinds-for-source (source-kind)
  "Return destination zone kind list for SOURCE-KIND.
SOURCE-KIND must be one of `residential', `commercial', or `industrial'."
  (pcase source-kind
    ('residential '(commercial))
    ('commercial '(industrial))
    ('industrial '(residential))
    (_ (error "Unknown source zone kind: %S" source-kind))))

(defun elcity-traffic-perimeter-road-coordinates (world x y)
  "Return deterministic road-entry coordinates around zone center X,Y in WORLD.
First checks cardinal-adjacent roads for single-tile scenarios.
If none are drivable, falls back to 5x5 perimeter offsets used by
Micropolis-style zone routing."
  (cl-check-type world elcity-world-map)
  (cl-check-type x integer)
  (cl-check-type y integer)
  (let (roads)
    ;; Phase 1: prefer immediate cardinal-adjacent roads.
    (dolist (coord (elcity-cardinal-neighbors x
                                              y
                                              (elcity-world-map-width world)
                                              (elcity-world-map-height world)))
      (when (elcity-traffic-road-coordinate-p world (car coord) (cdr coord))
        (push coord roads)))
    ;; Phase 2: fall back to Micropolis-style 5x5 perimeter offsets.
    (when (null roads)
      (dolist (offset elcity-traffic-perimeter-road-offsets)
        (let ((rx (+ x (car offset)))
              (ry (+ y (cdr offset))))
          (when (elcity-traffic-road-coordinate-p world rx ry)
            (push (cons rx ry) roads)))))
    (nreverse roads)))

(defun elcity-traffic-source-zones (world)
  "Return deterministic Y-then-X list of S3 source zones in WORLD.
Each entry is ((X . Y) . KIND)."
  (cl-check-type world elcity-world-map)
  (let ((width (elcity-world-map-width world))
        (height (elcity-world-map-height world))
        sources)
    (dotimes (y height)
      (dotimes (x width)
        (let ((kind (elcity-traffic-zone-kind-at world x y)))
          (when kind
            (push (cons (cons x y) kind) sources)))))
    (nreverse sources)))

(defun elcity-traffic--destination-adjacent-p (world x y destination-kinds source-x source-y)
  "Return non-nil when road tile X,Y in WORLD touches destination zone kind.
SOURCE-X,SOURCE-Y is excluded so routes do not immediately satisfy by touching
the origin zone center itself.
Destination checks are footprint-aware: touching any tile of a destination
zone footprint (not only its center tile) satisfies reachability."
  (cl-check-type world elcity-world-map)
  (cl-check-type x integer)
  (cl-check-type y integer)
  (cl-check-type destination-kinds list)
  (cl-check-type source-x integer)
  (cl-check-type source-y integer)
  (let ((width (elcity-world-map-width world))
        (height (elcity-world-map-height world))
        found)
    (dolist (coord (elcity-cardinal-neighbors x y width height) found)
      (let* ((nx (car coord))
             (ny (cdr coord))
             (context (elcity-traffic--zone-center-context-at-footprint-tile
                       world
                       nx
                       ny)))
        (when (and context
                   (not (and (= source-x (plist-get context :center-x))
                             (= source-y (plist-get context :center-y))))
                   (memq (plist-get context :kind)
                         destination-kinds))
          (setq found t))))))

(defun elcity-traffic--bounded-route-search (world start-roads destination-kinds source-x source-y
                                                   max-distance
                                                   &optional congestion-map congestion-threshold)
  "Return (PATH . DISTANCE) for first reachable route in WORLD, else nil.
Performs deterministic cardinal BFS from START-ROADS through drivable tiles.
Reachability is satisfied when path tiles touch DESTINATION-KINDS.
SOURCE-X and SOURCE-Y are excluded from destination checks.
Expansion stops when MAX-DISTANCE is reached.
When CONGESTION-MAP and CONGESTION-THRESHOLD are non-nil, neighbors at or
above the threshold are rejected from the BFS queue."
  (cl-check-type world elcity-world-map)
  (cl-check-type start-roads list)
  (cl-check-type destination-kinds list)
  (cl-check-type source-x integer)
  (cl-check-type source-y integer)
  (cl-check-type max-distance integer)
  (when (< max-distance 0)
    (error "MAX-DISTANCE must be non-negative"))
  (let* ((width (elcity-world-map-width world))
         (height (elcity-world-map-height world))
         (visited (make-vector (* width height) nil))
         (gate-active (and congestion-map congestion-threshold))
         queue
         tail)
    (cl-labels
        ((cell-index (x y)
           (+ x (* y width)))
         (enqueue (x y distance path-rev)
           (when (elcity-traffic-road-coordinate-p world x y)
             (unless (and gate-active
                          (>= (elcity-tile-field-ref congestion-map x y 0)
                               congestion-threshold))
               (let ((index (cell-index x y)))
                 (unless (aref visited index)
                   (aset visited index t)
                   (let ((node (list (list x y distance path-rev))))
                     (if tail
                         (setcdr tail node)
                       (setq queue node))
                     (setq tail node)))))))
         (dequeue ()
           (prog1 (car queue)
             (setq queue (cdr queue))
             (when (null queue)
               (setq tail nil)))))
      (dolist (start start-roads)
        (enqueue (car start)
                 (cdr start)
                 0
                 (list start)))
      (catch 'found
        (while queue
          (pcase-let* ((`(,x ,y ,distance ,path-rev) (dequeue)))
            (when (elcity-traffic--destination-adjacent-p world
                                                          x
                                                          y
                                                          destination-kinds
                                                          source-x
                                                          source-y)
              (throw 'found (cons (nreverse path-rev) distance)))
            (when (< distance max-distance)
              (dolist (neighbor (elcity-cardinal-neighbors x y width height))
                (enqueue (car neighbor)
                         (cdr neighbor)
                         (1+ distance)
                         (cons neighbor path-rev))))))
        nil))))

(cl-defun elcity-traffic-route-from-zone (world x y source-kind
                                               &key max-distance
                                               congestion-map
                                               congestion-threshold)
  "Return route result for source zone center X,Y of SOURCE-KIND in WORLD.
Outcomes:
- `no-road': no drivable perimeter/cardinal entry road,
- `blocked': road exists, but no destination reachable within max distance,
- `reachable': a route to destination kind exists.
MAX-DISTANCE (keyword `:max-distance') defaults to
`elcity-traffic-max-route-distance'.
When CONGESTION-MAP and CONGESTION-THRESHOLD are non-nil, start roads and
BFS neighbors at or above the threshold are rejected."
  (cl-check-type world elcity-world-map)
  (cl-check-type x integer)
  (cl-check-type y integer)
  (let* ((distance-limit (or max-distance elcity-traffic-max-route-distance))
         (start-roads (elcity-traffic-perimeter-road-coordinates world x y))
         (destination-kinds (elcity-traffic-destination-kinds-for-source source-kind))
         (gate-active (and congestion-map congestion-threshold)))
    (cl-check-type distance-limit integer)
    ;; Filter start roads by congestion gate.
    (when gate-active
      (setq start-roads
            (cl-remove-if
             (lambda (coord)
               (>= (elcity-tile-field-ref congestion-map
                                          (car coord) (cdr coord) 0)
                    congestion-threshold))
             start-roads)))
    (if (null start-roads)
        (elcity-traffic-route-result-create
         :x x
         :y y
         :source-kind source-kind
         :outcome (if (null (elcity-traffic-perimeter-road-coordinates world x y))
                      'no-road
                    'blocked))
      (let ((found (elcity-traffic--bounded-route-search world
                                                         start-roads
                                                         destination-kinds
                                                         x
                                                         y
                                                         distance-limit
                                                         congestion-map
                                                         congestion-threshold)))
        (if found
            (elcity-traffic-route-result-create
             :x x
             :y y
             :source-kind source-kind
             :outcome 'reachable
             :distance (cdr found)
             :path (car found))
          (elcity-traffic-route-result-create
           :x x
           :y y
           :source-kind source-kind
           :outcome 'blocked))))))

(defun elcity-traffic-summarize-results (results)
  "Return aggregate summary struct for route RESULTS."
  (cl-check-type results list)
  (let ((reachable 0)
        (blocked 0)
        (no-road 0))
    (dolist (result results)
      (cl-check-type result elcity-traffic-route-result)
      (pcase (elcity-traffic-route-result-outcome result)
        ('reachable (setq reachable (1+ reachable)))
        ('blocked (setq blocked (1+ blocked)))
        ('no-road (setq no-road (1+ no-road)))
        (_ (error "Unknown route outcome: %S"
                  (elcity-traffic-route-result-outcome result)))))
    (let ((total (+ reachable blocked no-road)))
      (elcity-traffic-summary-create
       :reachable reachable
       :blocked blocked
       :no-road no-road
       :ratio (if (= total 0)
                  1.0
                (/ (float reachable) total))))))

(defun elcity-traffic-empty-summary ()
  "Return default summary used before first S3 scan."
  (elcity-traffic-summary-create
   :reachable 0
   :blocked 0
   :no-road 0
   :ratio 1.0))

(defun elcity-traffic-congestion-map-for-world (world &optional initial-value)
  "Return tile-resolution congestion map sized for WORLD.
INITIAL-VALUE defaults to 0."
  (cl-check-type world elcity-world-map)
  (elcity-tile-field-make (elcity-world-map-width world)
                          (elcity-world-map-height world)
                          (or initial-value 0)))

(defun elcity-traffic-empty-scan-for-world (world)
  "Return default empty S3 scan for WORLD."
  (cl-check-type world elcity-world-map)
  (elcity-traffic-scan-create
   :results nil
   :summary (elcity-traffic-empty-summary)
   :congestion-map (elcity-traffic-congestion-map-for-world world 0)))

(defun elcity-traffic-congestion-map-compatible-p (world congestion-map)
  "Return non-nil when CONGESTION-MAP matches WORLD for S3 use."
  (cl-check-type world elcity-world-map)
  (cl-check-type congestion-map elcity-tile-field)
  (and (= (elcity-tile-field-width congestion-map)
          (elcity-world-map-width world))
       (= (elcity-tile-field-height congestion-map)
          (elcity-world-map-height world))))

(defun elcity-traffic-routing-capacity-permille (infra-eff)
  "Return routing capacity permille for INFRA-EFF [0,1000].
Capacity scales linearly from 250 (at 0% infra) to 1000 (at 100% infra)."
  (cl-check-type infra-eff integer)
  (+ 250 (floor (* 750 infra-eff) 1000)))

(defun elcity-traffic-routing-block-threshold (infra-eff)
  "Return congestion block threshold for INFRA-EFF [0,1000].
Tiles at or above this value are rejected by the routing gate."
  (cl-check-type infra-eff integer)
  (let ((capacity (elcity-traffic-routing-capacity-permille infra-eff)))
    (max 1 (floor (* elcity-traffic-congestion-cap capacity) 1000))))

(defun elcity-traffic-effective-congestion-cap (infra-eff)
  "Return effective congestion cap for INFRA-EFF.
When INFRA-EFF is nil (pre-first-scan state), return hard cap 240."
  (if infra-eff
      (elcity-traffic-routing-block-threshold infra-eff)
    elcity-traffic-congestion-cap))

(defun elcity-traffic-routing-decay-permille (infra-eff)
  "Return decay permille with 250 floor for INFRA-EFF [0,1000].
At 0% infra, decay uses 25% floor instead of zero to prevent persistent
gridlock."
  (cl-check-type infra-eff integer)
  (max 250 infra-eff))

(defun elcity-traffic-congestion-decay-value (value &optional infra-effectiveness)
  "Return decayed congestion VALUE for one tick.
Decay is monotonic toward zero and follows Micropolis-inspired thresholds.
INFRA-EFFECTIVENESS is permille [0, 1000] scaling decay amount; defaults to
1000 (full decay).  At 0, decay is zero and congestion never clears."
  (cl-check-type value integer)
  (let* ((eff (or infra-effectiveness 1000))
         (raw-decay (cond
                     ((<= value elcity-traffic-congestion-low-cutoff) value)
                     ((> value elcity-traffic-congestion-high-threshold)
                      elcity-traffic-congestion-high-decay)
                     (t
                      elcity-traffic-congestion-low-decay)))
         (scaled-decay (/ (* raw-decay eff) 1000)))
    (max 0 (- value scaled-decay))))

(defun elcity-traffic-congestion-decay-map (congestion-map &optional infra-effectiveness)
  "Return CONGESTION-MAP decayed by one tick.
INFRA-EFFECTIVENESS scales decay; see `elcity-traffic-congestion-decay-value'.
The returned map is a copy; input map is not mutated."
  (cl-check-type congestion-map elcity-tile-field)
  (let* ((next (copy-elcity-tile-field congestion-map))
         (cells (copy-sequence (elcity-tile-field-cells next))))
    (setf (elcity-tile-field-cells next) cells)
    (dotimes (i (length cells))
      (aset cells i (elcity-traffic-congestion-decay-value
                     (aref cells i) infra-effectiveness)))
    next))

(defun elcity-traffic-congestion-increment (value touch-count)
  "Return congestion increment for VALUE with TOUCH-COUNT route touches.
Increment diminishes as VALUE approaches cap, with a per-touch
floor of `elcity-traffic-congestion-step-floor'.  Result is
always non-negative and capped so VALUE + result <= cap."
  (cl-check-type value integer)
  (cl-check-type touch-count integer)
  (if (<= touch-count 0) 0
    (let* ((clamped-value (max 0
                               (min value elcity-traffic-congestion-cap)))
           (headroom (- elcity-traffic-congestion-cap clamped-value))
           (per-touch (max elcity-traffic-congestion-step-floor
                          (/ (* elcity-traffic-congestion-step headroom)
                             elcity-traffic-congestion-cap)))
           (total (* per-touch touch-count)))
      (min headroom total))))

(defun elcity-traffic-zone-penalty (summary)
  "Return derived zone penalty from traffic route SUMMARY."
  (cl-check-type summary elcity-traffic-summary)
  (+ (* elcity-traffic-blocked-zone-penalty
        (elcity-traffic-summary-blocked summary))
     (* elcity-traffic-no-road-zone-penalty
        (elcity-traffic-summary-no-road summary))))

(defun elcity-traffic-route-result-at (scan x y)
  "Return route-result entry at zone center X,Y in SCAN, or nil."
  (cl-check-type scan elcity-traffic-scan)
  (cl-check-type x integer)
  (cl-check-type y integer)
  (cl-find-if (lambda (result)
                (and (= x (elcity-traffic-route-result-x result))
                     (= y (elcity-traffic-route-result-y result))))
              (elcity-traffic-scan-results scan)))

(defun elcity-traffic-route-status-at (scan x y &optional default)
  "Return route status symbol at zone center X,Y in SCAN, or DEFAULT.
Uses O(1) status-map lookup when available, falling back to linear scan."
  (cl-check-type scan elcity-traffic-scan)
  (cl-check-type x integer)
  (cl-check-type y integer)
  (let ((smap (elcity-traffic-scan-status-map scan)))
    (if smap
        (let ((code (elcity-tile-field-ref smap x y 0)))
          (if (= code elcity-traffic-STATUS-NONE)
              default
            (elcity-traffic--status-code-to-outcome code)))
      ;; Fallback: linear scan for scans without status-map.
      (let ((result (elcity-traffic-route-result-at scan x y)))
        (if result
            (elcity-traffic-route-result-outcome result)
          default)))))

(defun elcity-traffic-zone-accessible-p (status)
  "Return non-nil when route STATUS permits growth/access gating."
  (eq status 'reachable))

(defun elcity-traffic-zone-road-connected-p (status)
  "Return non-nil when route STATUS indicates local road adjacency.
True for `reachable' and `blocked'; nil for `no-road'."
  (not (eq status 'no-road)))

(defun elcity-traffic-zone-destination-reachable-p (status)
  "Return non-nil when route STATUS indicates a reachable destination.
True only for `reachable'; nil for `blocked' and `no-road'."
  (eq status 'reachable))

(defun elcity-traffic--apply-route-congestion (congestion-map result)
  "Apply single route RESULT congestion to CONGESTION-MAP in place.
Each unique tile in the route path gets one touch.
CONGESTION-MAP cells are mutated directly; no copy is made.
The caller (`elcity-traffic-scan-world') owns the buffer: it creates a
fresh copy via `elcity-traffic-congestion-decay-map' and then feeds it
here repeatedly so each route sees cumulative load from prior routes."
  (when (eq (elcity-traffic-route-result-outcome result) 'reachable)
    (let ((cells (elcity-tile-field-cells congestion-map))
          (width (elcity-tile-field-width congestion-map))
          (seen (make-hash-table :test #'equal)))
      (dolist (coord (elcity-traffic-route-result-path result))
        (unless (gethash coord seen)
          (puthash coord t seen)
          (let* ((x (car coord))
                 (y (cdr coord))
                 (index (+ x (* y width)))
                 (value (aref cells index))
                 (inc (elcity-traffic-congestion-increment value 1)))
            (aset cells index (+ value inc))))))))

(defun elcity-traffic-scan-world (world &optional previous-congestion-map
                                       infra-effectiveness)
  "Return deterministic S3 scan result for WORLD.
PREVIOUS-CONGESTION-MAP is optional prior congestion memory.
INFRA-EFFECTIVENESS scales congestion decay (permille, default 1000).
The scan classifies each source zone as `reachable', `blocked', or `no-road'.
Pipeline ordering:
1. select compatible previous map,
2. compute routing decay permille (with floor),
3. decay previous map (returns a copy),
4. compute routing block threshold,
5. for each source zone (Y-then-X deterministic order):
   a. route zone against working map (sees prior routes' load),
   b. if reachable, apply route congestion to working map immediately,
6. build status-map (integer-coded tile-field for O(1) status lookup),
7. publish scan."
  (cl-check-type world elcity-world-map)
  (let* ((eff (or infra-effectiveness 1000))
         ;; 1. Select compatible previous map.
         (base-map (if (and previous-congestion-map
                            (elcity-traffic-congestion-map-compatible-p
                             world previous-congestion-map))
                       previous-congestion-map
                     (elcity-traffic-congestion-map-for-world world 0)))
         ;; 2. Compute routing decay permille (with floor).
         (decay-eff (elcity-traffic-routing-decay-permille eff))
         ;; 3. Decay previous map (returns a fresh copy).
         (working-map (elcity-traffic-congestion-decay-map base-map decay-eff))
         ;; 4. Compute routing block threshold.
         (block-threshold (elcity-traffic-routing-block-threshold eff))
         ;; 5. Progressive per-route application.
         (source-zones (elcity-traffic-source-zones world))
         results)
    (dolist (entry source-zones)
      (let* ((coord (car entry))
             (kind (cdr entry))
             (result (elcity-traffic-route-from-zone
                      world
                      (car coord)
                      (cdr coord)
                      kind
                      :max-distance elcity-traffic-max-route-distance
                      :congestion-map working-map
                      :congestion-threshold block-threshold)))
        ;; 5b. Apply this route's congestion immediately.
        (elcity-traffic--apply-route-congestion working-map result)
        (push result results)))
    (setq results (nreverse results))
    ;; 6. Build status-map for O(1) route-status lookup.
    (let* ((width (elcity-world-map-width world))
           (height (elcity-world-map-height world))
           (status-field (elcity-tile-field-make width height 0))
           (status-cells (elcity-tile-field-cells status-field)))
      (dolist (result results)
        (let* ((rx (elcity-traffic-route-result-x result))
               (ry (elcity-traffic-route-result-y result))
               (code (elcity-traffic--outcome-to-status-code
                      (elcity-traffic-route-result-outcome result))))
          (aset status-cells (+ rx (* ry width)) code)))
      ;; 7. Publish scan.
      (let ((summary (elcity-traffic-summarize-results results)))
        (elcity-traffic-scan-create
         :results results
         :summary summary
         :congestion-map working-map
         :status-map status-field)))))

(defun elcity-traffic-congestion-distribution-stats (congestion-map)
  "Return distribution statistics for CONGESTION-MAP.
Return value is a plist with:
  :total-cells  - total tile cells,
  :active-cells - cells with value > 0,
  :capped-cells - cells at cap,
  :cap-ratio    - capped / active (0.0 when none active),
  :mid-cells    - cells with 0 < value < cap,
  :mid-ratio    - mid / active (0.0 when none active).
Counts are non-negative integers; ratios are floats
in [0.0, 1.0]."
  (cl-check-type congestion-map elcity-tile-field)
  (let* ((cells (elcity-tile-field-cells congestion-map))
         (total (length cells))
         (active 0)
         (capped 0))
    (dotimes (i total)
      (let ((v (aref cells i)))
        (when (> v 0)
          (setq active (1+ active))
          (when (>= v elcity-traffic-congestion-cap)
            (setq capped (1+ capped))))))
    (let ((mid (- active capped)))
      (list :total-cells total
            :active-cells active
            :capped-cells capped
            :cap-ratio (if (zerop active) 0.0
                         (/ (float capped) active))
            :mid-cells mid
            :mid-ratio (if (zerop active) 0.0
                         (/ (float mid) active))))))

(defun elcity-traffic-congestion-map-sum (congestion-map)
  "Return sum of all congestion-memory cells in CONGESTION-MAP."
  (cl-check-type congestion-map elcity-tile-field)
  (cl-reduce #'+ (elcity-tile-field-cells congestion-map) :initial-value 0))


(defun elcity-traffic-zone-bottleneck-congestion (world congestion-map
                                                        center-x center-y)
  "Return max congestion on perimeter roads near CENTER-X,CENTER-Y in WORLD.
CONGESTION-MAP provides tile-level congestion values.
Returns 0 when no perimeter roads are present."
  (cl-check-type world elcity-world-map)
  (cl-check-type congestion-map elcity-tile-field)
  (cl-check-type center-x integer)
  (cl-check-type center-y integer)
  (let ((roads (elcity-traffic-perimeter-road-coordinates
                world center-x center-y))
        (max-val 0))
    (dolist (coord roads max-val)
      (let ((value (elcity-tile-field-ref congestion-map
                                          (car coord) (cdr coord) 0)))
        (when (> value max-val)
          (setq max-val value))))))

(defun elcity-traffic-zone-average-congestion (world congestion-map
                                                     center-x center-y)
  "Return average congestion on perimeter roads near CENTER-X,CENTER-Y.
WORLD provides bounds and road classification.
CONGESTION-MAP provides tile-level congestion values.
Average is rounded up to nearest integer so low non-zero pressure remains
visible in player-facing UI.  Returns 0 when no perimeter roads are present."
  (cl-check-type world elcity-world-map)
  (cl-check-type congestion-map elcity-tile-field)
  (cl-check-type center-x integer)
  (cl-check-type center-y integer)
  (let ((roads (elcity-traffic-perimeter-road-coordinates
                world center-x center-y))
        (sum 0)
        (count 0))
    (dolist (coord roads)
      (setq sum (+ sum (elcity-tile-field-ref congestion-map
                                              (car coord) (cdr coord) 0)))
      (setq count (1+ count)))
    (if (zerop count) 0
      (/ (+ sum (1- count)) count))))

(defun elcity-traffic-congestion-road-avg (world congestion-map)
  "Return average congestion over road-active tiles in WORLD.
CONGESTION-MAP provides tile-level congestion values.
Only drivable road/crossing tiles are counted.  Returns 0 when none exist."
  (cl-check-type world elcity-world-map)
  (cl-check-type congestion-map elcity-tile-field)
  (let ((world-width (elcity-world-map-width world))
        (world-height (elcity-world-map-height world))
        (active-count 0)
        (sum 0))
    (dotimes (y world-height)
      (dotimes (x world-width)
        (when (elcity-traffic-road-index-p
               (elcity-tile-index (elcity-world-tile-at world x y 0)))
          (setq active-count (1+ active-count))
          (setq sum (+ sum (elcity-tile-field-ref congestion-map x y 0))))))
    (if (zerop active-count) 0
      (/ sum active-count))))

(provide 'elcity-traffic)

;;; elcity-traffic.el ends here
