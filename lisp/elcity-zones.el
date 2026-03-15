;;; elcity-zones.el --- S4 zone dynamics primitives -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; Pure S4 helpers for zone dynamics (RCI).
;;
;; This module owns:
;; - zone kind and level contracts,
;; - population-per-level tables and aggregate summaries,
;; - bounded migration cadence and deterministic decision rolls,
;; - local congestion penalty scaling for effective-score computation,
;; - zone transition records and center-tile update helpers.

;;; Code:

(require 'cl-lib)
(require 'elcity-tile)
(require 'elcity-traffic)
(require 'elcity-util)
(require 'elcity-world)

(defconst elcity-zones-level-min 0
  "Minimum zone level for S4 dynamics.")

(defconst elcity-zones-level-max 3
  "Maximum zone level for S4 dynamics.")

(defconst elcity-zones-residential-level-0-index 240
  "Residential zone-center tile index used for level 0.")

(defconst elcity-zones-commercial-level-0-index 423
  "Commercial zone-center tile index used for level 0.")

(defconst elcity-zones-industrial-level-0-index 612
  "Industrial zone-center tile index used for level 0.")

(defconst elcity-zones-border-index-offset 4
  "Offset from kind base index to border tile sub-range.
Border tile index = kind-base + this offset + level.
Center tile index = kind-base + level.
This keeps center indices (base+0..base+3) and border indices
\(base+4..base+7) in adjacent non-overlapping sub-ranges.")

(defconst elcity-zones-residential-population-table [16 24 32 40]
  "Residential population-per-level table for levels 0..3.")

(defconst elcity-zones-commercial-population-table [8 16 24 32]
  "Commercial population-per-level table for levels 0..3.")

(defconst elcity-zones-industrial-population-table [8 16 24 32]
  "Industrial population-per-level table for levels 0..3.")

(defconst elcity-zones-demand-min -2000
  "Minimum bounded demand value accepted by S4 decision model.")

(defconst elcity-zones-demand-max 2000
  "Maximum bounded demand value accepted by S4 decision model.")

(defconst elcity-zones-local-quality-min -1000
  "Minimum bounded local-quality value accepted by S4 decision model.")

(defconst elcity-zones-local-quality-max 1000
  "Maximum bounded local-quality value accepted by S4 decision model.")

(defconst elcity-zones-default-local-quality 0
  "Default local-quality placeholder used before S6 fields subsystem exists.")

(defconst elcity-zones-congestion-penalty-max 200
  "Maximum effective-score penalty from local congestion.
Scales linearly from 0 at congestion=0 to this value at congestion cap.
Penalty is subtracted from raw score before gate application.")

(defconst elcity-zones-migration-cadence-divisor 8
  "Deterministic migration cadence divisor (evaluate one eighth per tick).")

(defconst elcity-zones-pollution-gate-threshold 300
  "Local pollution threshold above which residential in-migration is blocked.
Applies only to `:residential' zones.  When S6 local pollution at a zone
center exceeds this value, growth is blocked by capping effective-score below
`elcity-zones-growth-threshold'.  This implements no-growth semantics (block
in-migration) without forcing decline pressure.
Calibration range: [250, 350].")

(defconst elcity-zones-gate-decline-floor -1200
  "Effective-score floor applied when power/traffic gates fail.")

(defconst elcity-zones-blocked-decline-floor -300
  "Effective-score floor applied when traffic is blocked but road-connected.
This is less harsh than `elcity-zones-gate-decline-floor' (-1200) used for
complete gate failures (no-road or no-power).  The value sits past the decline
threshold so decline is possible, but growth is inhibited.")

(defconst elcity-zones-growth-threshold 100
  "Effective-score threshold where growth attempts become eligible.")

(defconst elcity-zones-decline-threshold -100
  "Effective-score threshold where decline attempts become eligible.")

(defconst elcity-zones-decision-probability-scale 1000
  "Exclusive upper bound for deterministic zone decision roll.")

(defconst elcity-zones-decision-base-probability 500
  "Base growth/decline probability once thresholds are crossed.")

(defconst elcity-zones-decision-score-scale 2
  "Score-to-probability scaling factor for growth/decline chance.")

(cl-defstruct (elcity-zone-level-map
               (:constructor elcity-zone-level-map-create))
  "Tile-resolution map of S4 zone levels.
WIDTH and HEIGHT are tile dimensions.  CELLS is a row-major vector of integer
levels where coordinates without active zone centers may keep default values."
  (width 0 :type integer)
  (height 0 :type integer)
  (cells [] :type vector))

(cl-defstruct (elcity-zone-transition
               (:constructor elcity-zone-transition-create))
  "One S4 zone-level transition at zone-center tile X,Y.
KIND is one of `residential', `commercial', or `industrial'.
FROM-LEVEL and TO-LEVEL are clamped level values in [0,3].
REASON is one of `grow', `decline', or `hold'."
  (x 0 :type integer)
  (y 0 :type integer)
  (kind nil :type symbol)
  (from-level 0 :type integer)
  (to-level 0 :type integer)
  (reason 'hold :type symbol))

(cl-defstruct (elcity-zone-dynamics-summary
               (:constructor elcity-zone-dynamics-summary-create))
  "Aggregate S4 outputs for one zones-stage pass.
RESIDENTIAL-POP, COMMERCIAL-POP, and INDUSTRIAL-POP are derived from zone
levels via per-kind population tables.
GROWN/DECLINED/HELD classify per-zone level outcomes for this tick.
BLOCKED-POWER/BLOCKED-TRAFFIC/BLOCKED-BOTH count infrastructure gate failures.
BLOCKED-POLLUTION counts residential zones gated by high local pollution.
BLOCKED-ANY counts the union of all gate failures (power, traffic, pollution)
without double-counting zones that fail multiple gates.
EVALUATED is the number of cadence-eligible zone centers this tick.
TOTAL-ZONES is the number of zone centers scanned."
  (residential-pop 0 :type integer)
  (commercial-pop 0 :type integer)
  (industrial-pop 0 :type integer)
  (grown 0 :type integer)
  (declined 0 :type integer)
  (held 0 :type integer)
  (blocked-power 0 :type integer)
  (blocked-traffic 0 :type integer)
  (blocked-both 0 :type integer)
  (blocked-pollution 0 :type integer)
  (blocked-any 0 :type integer)
  (evaluated 0 :type integer)
  (total-zones 0 :type integer))

(cl-defstruct (elcity-zone-dynamics-scan
               (:constructor elcity-zone-dynamics-scan-create))
  "S4 scan result bundle for one deterministic zones-stage pass.
LEVEL-MAP stores resulting zone levels for scanned zone centers.
SUMMARY stores aggregate population and transition metrics.
TRANSITIONS stores changed level entries in deterministic scan order.
ZONE-CENTERS stores deterministic Y-then-X scanned zone entries."
  (level-map nil :type elcity-zone-level-map)
  (summary nil :type elcity-zone-dynamics-summary)
  (transitions nil :type list)
  (zone-centers nil :type list))

(defun elcity-zones-check-kind (kind)
  "Signal error when KIND is not one of supported R/C/I symbols."
  (unless (memq kind '(residential commercial industrial))
    (error "Unknown zone kind: %S" kind)))

(defun elcity-zones-clamp-integer (value minimum maximum)
  "Return VALUE clamped to inclusive MINIMUM..MAXIMUM range."
  (elcity-util-clamp value minimum maximum))

(defun elcity-zones-clamp-level (level)
  "Return LEVEL clamped to S4 level range 0..3."
  (elcity-zones-clamp-integer level
                              elcity-zones-level-min
                              elcity-zones-level-max))

(defun elcity-zones-clamp-demand (value)
  "Return demand VALUE clamped to S4 demand bounds."
  (elcity-zones-clamp-integer value
                              elcity-zones-demand-min
                              elcity-zones-demand-max))

(defun elcity-zones-clamp-local-quality (value)
  "Return local-quality VALUE clamped to S4 quality bounds."
  (elcity-zones-clamp-integer value
                              elcity-zones-local-quality-min
                              elcity-zones-local-quality-max))

(defun elcity-zones-level-map-make (width height &optional initial-level)
  "Return a zone-level map for WIDTH and HEIGHT.
INITIAL-LEVEL defaults to 0 and is clamped to valid zone-level range."
  (cl-check-type width integer)
  (cl-check-type height integer)
  (when (or (<= width 0)
            (<= height 0))
    (error "WIDTH and HEIGHT must be positive"))
  (let ((level (elcity-zones-clamp-level (or initial-level 0))))
    (elcity-zone-level-map-create
     :width width
     :height height
     :cells (make-vector (* width height) level))))

(defun elcity-zones-level-map-for-world (world &optional initial-level)
  "Return a zone-level map sized for WORLD.
INITIAL-LEVEL defaults to 0 and is clamped to valid zone-level range."
  (cl-check-type world elcity-world-map)
  (elcity-zones-level-map-make (elcity-world-map-width world)
                               (elcity-world-map-height world)
                               initial-level))

(defun elcity-zones-level-map-in-bounds-p (level-map x y)
  "Return non-nil when X,Y are valid coordinates in LEVEL-MAP."
  (cl-check-type level-map elcity-zone-level-map)
  (cl-check-type x integer)
  (cl-check-type y integer)
  (and (<= 0 x)
       (< x (elcity-zone-level-map-width level-map))
       (<= 0 y)
       (< y (elcity-zone-level-map-height level-map))))

(defun elcity-zones-level-map--cell-index (level-map x y)
  "Return row-major index in LEVEL-MAP for X,Y, else nil."
  (when (elcity-zones-level-map-in-bounds-p level-map x y)
    (+ x
       (* y (elcity-zone-level-map-width level-map)))))

(defun elcity-zones-level-at (level-map x y &optional default)
  "Return zone level at X,Y in LEVEL-MAP, or DEFAULT when out of bounds."
  (cl-check-type level-map elcity-zone-level-map)
  (cl-check-type x integer)
  (cl-check-type y integer)
  (let ((index (elcity-zones-level-map--cell-index level-map x y)))
    (if index
        (aref (elcity-zone-level-map-cells level-map) index)
      default)))

(defun elcity-zones-level-map-compatible-p (world level-map)
  "Return non-nil when LEVEL-MAP dimensions match WORLD dimensions."
  (cl-check-type world elcity-world-map)
  (cl-check-type level-map elcity-zone-level-map)
  (and (= (elcity-zone-level-map-width level-map)
          (elcity-world-map-width world))
       (= (elcity-zone-level-map-height level-map)
          (elcity-world-map-height world))))

(defun elcity-zones-kind-for-index (tile-index)
  "Return zone kind symbol for TILE-INDEX, or nil when not a zone kind."
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

(defun elcity-zones-kind-at-world (world x y)
  "Return zone kind at WORLD tile X,Y, or nil when not an S4 zone center."
  (cl-check-type world elcity-world-map)
  (cl-check-type x integer)
  (cl-check-type y integer)
  (when (elcity-world-in-bounds-p world x y)
    (let ((tile (elcity-world-tile-at world x y 0)))
      (when (elcity-tile-zone-center-p tile)
        (elcity-zones-kind-for-index (elcity-tile-index tile))))))

(defun elcity-zones-kind-base-index (kind)
  "Return level-0 center tile index for zone KIND."
  (elcity-zones-check-kind kind)
  (pcase kind
    ('residential elcity-zones-residential-level-0-index)
    ('commercial elcity-zones-commercial-level-0-index)
    ('industrial elcity-zones-industrial-level-0-index)))

(defun elcity-zones-center-index (kind level)
  "Return center tile index for KIND at LEVEL.
KIND must be `residential', `commercial', or `industrial'."
  (+ (elcity-zones-clamp-level level)
     (elcity-zones-kind-base-index kind)))

(defun elcity-zones-border-index (kind level)
  "Return border tile index for KIND at LEVEL.
KIND must be `residential', `commercial', or `industrial'.
Border index is kind-base + `elcity-zones-border-index-offset' + level."
  (+ (elcity-zones-clamp-level level)
     elcity-zones-border-index-offset
     (elcity-zones-kind-base-index kind)))

(defun elcity-zones-level-from-index (kind tile-index)
  "Return decoded level for KIND from TILE-INDEX.
Values outside the KIND base range return 0."
  (cl-check-type tile-index integer)
  (let ((base (elcity-zones-kind-base-index kind)))
    (if (and (<= base tile-index)
             (<= tile-index (+ base elcity-zones-level-max)))
        (- tile-index base)
      0)))

(defun elcity-zones-population-for-level (kind level)
  "Return per-zone population for KIND and LEVEL.
LEVEL is clamped to 0..3 before lookup."
  (elcity-zones-check-kind kind)
  (let ((next-level (elcity-zones-clamp-level level)))
    (aref (pcase kind
            ('residential elcity-zones-residential-population-table)
            ('commercial elcity-zones-commercial-population-table)
            ('industrial elcity-zones-industrial-population-table))
          next-level)))

(defun elcity-zones-empty-summary ()
  "Return default empty zone-dynamics summary.
For empty worlds this reflects no zones, no transitions, and zero populations."
  (elcity-zone-dynamics-summary-create
   :residential-pop 0
   :commercial-pop 0
   :industrial-pop 0
   :grown 0
   :declined 0
   :held 0
   :blocked-power 0
   :blocked-traffic 0
   :blocked-both 0
   :evaluated 0
   :total-zones 0))

(defun elcity-zones-empty-scan-for-world (world)
  "Return default empty S4 scan for WORLD."
  (cl-check-type world elcity-world-map)
  (elcity-zone-dynamics-scan-create
   :level-map (elcity-zones-level-map-for-world world 0)
   :summary (elcity-zones-empty-summary)
   :transitions nil
   :zone-centers nil))

(defun elcity-zones-migration-eligible-p (zone-index zones-cycle)
  "Return non-nil when ZONE-INDEX is cadence-eligible at ZONES-CYCLE.
Cadence uses a deterministic one-eighth partition of scanned zone centers."
  (cl-check-type zone-index integer)
  (cl-check-type zones-cycle integer)
  (= 0 (mod (+ zone-index zones-cycle)
            elcity-zones-migration-cadence-divisor)))

(defun elcity-zones--kind-salt (kind)
  "Return deterministic integer salt for KIND."
  (elcity-zones-check-kind kind)
  (pcase kind
    ('residential 1013904223)
    ('commercial 1664525)
    ('industrial 69069)))

(defun elcity-zones-deterministic-roll (seed zones-cycle x y kind)
  "Return deterministic integer decision roll for SEED at X,Y and ZONES-CYCLE.
The roll is in [0, `elcity-zones-decision-probability-scale')."
  (cl-check-type seed integer)
  (cl-check-type zones-cycle integer)
  (cl-check-type x integer)
  (cl-check-type y integer)
  (elcity-zones-check-kind kind)
  (let* ((n (+ (* 1103515245 (logand seed #xffffffff))
               (* 12345 (1+ zones-cycle))
               (* 2654435761 (1+ x))
               (* 2246822519 (1+ y))
               (elcity-zones--kind-salt kind))))
    (setq n (logand n #xffffffff))
    (setq n (logxor n (ash n -16)))
    (mod n elcity-zones-decision-probability-scale)))

(defun elcity-zones-growth-probability (effective-score)
  "Return growth probability for EFFECTIVE-SCORE as integer in [0,1000]."
  (cl-check-type effective-score integer)
  (if (< effective-score elcity-zones-growth-threshold)
      0
    (min elcity-zones-decision-probability-scale
         (+ elcity-zones-decision-base-probability
            (/ (- effective-score elcity-zones-growth-threshold)
               elcity-zones-decision-score-scale)))))

(defun elcity-zones-decline-probability (effective-score)
  "Return decline probability for EFFECTIVE-SCORE as integer in [0,1000]."
  (cl-check-type effective-score integer)
  (if (> effective-score elcity-zones-decline-threshold)
      0
    (min elcity-zones-decision-probability-scale
         (+ elcity-zones-decision-base-probability
            (/ (- elcity-zones-decline-threshold effective-score)
               elcity-zones-decision-score-scale)))))

(defun elcity-zones-decision-for-score (effective-score roll)
  "Return decision symbol for EFFECTIVE-SCORE and deterministic ROLL.
Result is one of `grow', `decline', or `hold'."
  (cl-check-type effective-score integer)
  (cl-check-type roll integer)
  (cond
   ((and (>= effective-score elcity-zones-growth-threshold)
         (< roll (elcity-zones-growth-probability effective-score)))
    'grow)
   ((and (<= effective-score elcity-zones-decline-threshold)
         (< roll (elcity-zones-decline-probability effective-score)))
    'decline)
   (t 'hold)))

(defun elcity-zones-gate-block-reason (powered road-connected destination-reachable)
  "Return gate block reason from POWERED, ROAD-CONNECTED, DESTINATION-REACHABLE.
Returns nil when all gates pass, otherwise one of:
  `power'            — only power missing,
  `traffic-no-road'  — no road adjacency (with or without power),
  `traffic-blocked'  — road-connected but destination unreachable,
  `both-no-road'     — power missing and no road,
  `both-blocked'     — power missing and blocked."
  (let ((traffic-ok (and road-connected destination-reachable)))
    (cond
     ((and powered traffic-ok) nil)
     ((and (not powered) (not road-connected)) 'both-no-road)
     ((and (not powered) (not destination-reachable)) 'both-blocked)
     ((not powered) 'power)
     ((not road-connected) 'traffic-no-road)
     (t 'traffic-blocked))))

(defun elcity-zones-congestion-penalty (congestion)
  "Return bounded effective-score penalty for CONGESTION value.
Scales linearly from 0 to `elcity-zones-congestion-penalty-max'.
CONGESTION is clamped to [0, `elcity-traffic-congestion-cap']."
  (cl-check-type congestion integer)
  (let ((clamped (max 0 (min congestion elcity-traffic-congestion-cap))))
    (/ (* clamped elcity-zones-congestion-penalty-max)
       elcity-traffic-congestion-cap)))

(defun elcity-zones-effective-score (raw-score powered road-connected
                                               destination-reachable)
  "Return effective score from RAW-SCORE and gate signals.
POWERED, ROAD-CONNECTED, and DESTINATION-REACHABLE are boolean gate inputs.
- All pass: RAW-SCORE is returned unchanged.
- No power or no road: hard decline floor (`elcity-zones-gate-decline-floor').
- Road-connected but blocked: soft decline floor
  (`elcity-zones-blocked-decline-floor')."
  (cl-check-type raw-score integer)
  (cond
   ((and powered road-connected destination-reachable)
    raw-score)
   ((or (not powered) (not road-connected))
    elcity-zones-gate-decline-floor)
   (t
    elcity-zones-blocked-decline-floor)))

(defun elcity-zones-apply-decision (level decision)
  "Return next clamped LEVEL after applying DECISION symbol.
DECISION must be one of `grow', `decline', or `hold'."
  (cl-check-type level integer)
  (let ((next (pcase decision
                ('grow (1+ level))
                ('decline (1- level))
                ('hold level)
                (_ (error "Unknown decision: %S" decision)))))
    (elcity-zones-clamp-level next)))

(defun elcity-zones--transition-stamp-updates (world transition)
  "Return deterministic 3x3 tile-update list for TRANSITION in WORLD.
Updates center tile index to `center-index(kind, to-level)' and all 8 border
tiles to `border-index(kind, to-level)'.  Ordering is row-major (top-left to
bottom-right).  Out-of-bounds positions are skipped.
Border tiles whose index falls in the drivable road range are preserved so
that zone transitions in compact layouts do not destroy traffic connectivity.
Each update is ((X . Y) . TILE)."
  (cl-check-type world elcity-world-map)
  (cl-check-type transition elcity-zone-transition)
  (let* ((cx (elcity-zone-transition-x transition))
         (cy (elcity-zone-transition-y transition))
         (kind (elcity-zone-transition-kind transition))
         (to-level (elcity-zone-transition-to-level transition))
         (center-idx (elcity-zones-center-index kind to-level))
         (border-idx (elcity-zones-border-index kind to-level))
         updates)
    (dotimes (dy 3)
      (dotimes (dx 3)
        (let* ((tx (+ (1- cx) dx))
               (ty (+ (1- cy) dy))
               (tile (elcity-world-tile-at world tx ty)))
          (when tile
            (if (and (= dx 1) (= dy 1))
                (push (cons (cons tx ty)
                            (elcity-tile-with-index tile center-idx))
                      updates)
              ;; Skip border tiles that serve as traffic infrastructure.
              (unless (elcity-traffic-road-index-p (elcity-tile-index tile))
                (push (cons (cons tx ty)
                            (elcity-tile-with-index tile border-idx))
                      updates)))))))
    (nreverse updates)))

(defun elcity-zones-transition-tile-updates (world transitions)
  "Return TILE-UPDATES list for WORLD from zone TRANSITIONS.
Each transition emits a deterministic 3x3 stamp update (center + 8 border).
Updates are in `elcity-state-api-apply-tile-updates' format: ((X . Y) . TILE)."
  (cl-check-type world elcity-world-map)
  (cl-check-type transitions list)
  (let (updates)
    (dolist (transition transitions)
      (cl-check-type transition elcity-zone-transition)
      (unless (= (elcity-zone-transition-from-level transition)
                 (elcity-zone-transition-to-level transition))
        (dolist (update (elcity-zones--transition-stamp-updates
                         world transition))
          (push update updates))))
    (nreverse updates)))

(defun elcity-zones-normalize-footprints (world)
  "Return tile-update list that normalizes all zone footprints in WORLD.
For each zone center, rewrites the 8 surrounding in-bounds border tiles to
use `border-index(kind, center-level)'.  Center tiles are not modified.
Overlap tie-break: Y-then-X scan order (last writer wins for shared borders).
The result is idempotent: applying it to an already-normalized world produces
no changes.  Updates are in `elcity-state-api-apply-tile-updates' format."
  (cl-check-type world elcity-world-map)
  (let ((width (elcity-world-map-width world))
        (height (elcity-world-map-height world))
        updates)
    (dotimes (cy height)
      (dotimes (cx width)
        (let ((kind (elcity-zones-kind-at-world world cx cy)))
          (when kind
            (let* ((center-tile (elcity-world-tile-at world cx cy))
                   (level (elcity-zones-level-from-index
                           kind (elcity-tile-index center-tile)))
                   (border-idx (elcity-zones-border-index kind level)))
              (dotimes (dy 3)
                (dotimes (dx 3)
                  (unless (and (= dx 1) (= dy 1))
                    (let* ((tx (+ (1- cx) dx))
                           (ty (+ (1- cy) dy))
                           (tile (elcity-world-tile-at world tx ty)))
                      (when (and tile
                                 (/= (elcity-tile-index tile) border-idx))
                        (push (cons (cons tx ty)
                                    (elcity-tile-with-index tile border-idx))
                              updates)))))))))))
    (nreverse updates)))

(defun elcity-zones-scan-tile-updates (world scan)
  "Return TILE-UPDATES for WORLD from zone dynamics SCAN transitions."
  (cl-check-type world elcity-world-map)
  (cl-check-type scan elcity-zone-dynamics-scan)
  (elcity-zones-transition-tile-updates world
                                        (elcity-zone-dynamics-scan-transitions scan)))

(defun elcity-zones--zone-center-entries (world)
  "Return deterministic Y-then-X list of zone-center entries for WORLD.
Each entry is ((X . Y) . KIND)."
  (cl-check-type world elcity-world-map)
  (let ((width (elcity-world-map-width world))
        (height (elcity-world-map-height world))
        entries)
    (dotimes (y height)
      (dotimes (x width)
        (let ((kind (elcity-zones-kind-at-world world x y)))
          (when kind
            (push (cons (cons x y) kind) entries)))))
    (nreverse entries)))

(defun elcity-zones--initial-level (world previous-level-map x y kind)
  "Return initial level for WORLD zone center X,Y of KIND.
When PREVIOUS-LEVEL-MAP is compatible, use it.  Otherwise decode from tile
index, defaulting to 0 when tile index is outside S4 level mapping."
  (cl-check-type world elcity-world-map)
  (cl-check-type x integer)
  (cl-check-type y integer)
  (elcity-zones-check-kind kind)
  (if (and previous-level-map
           (elcity-zones-level-map-compatible-p world previous-level-map))
      (elcity-zones-clamp-level
       (elcity-zones-level-at previous-level-map x y 0))
    (elcity-zones-level-from-index kind
                                   (elcity-tile-index
                                    (elcity-world-tile-at world x y 0)))))

(defun elcity-zones-pollution-gated-p (kind pollution)
  "Return non-nil when KIND zone is blocked by local POLLUTION.
Only `:residential' zones are subject to the pollution gate.
Returns non-nil when POLLUTION exceeds `elcity-zones-pollution-gate-threshold'."
  (and (eq kind 'residential)
       (> pollution elcity-zones-pollution-gate-threshold)))

(defun elcity-zones--apply-pollution-growth-cap (effective-score pollution-gated)
  "Return EFFECTIVE-SCORE after applying no-growth cap when POLLUTION-GATED.
When POLLUTION-GATED is non-nil, score is capped below growth threshold so
residential in-migration is blocked without forcing decline pressure."
  (cl-check-type effective-score integer)
  (if pollution-gated
      (min effective-score
           (1- elcity-zones-growth-threshold))
    effective-score))

(defun elcity-zones-scan-with-zone-centers (world zone-centers previous-level-map
                                                  zones-cycle seed
                                                  gate-provider demand-provider
                                                  quality-provider
                                                  &optional congestion-provider
                                                  pollution-provider)
  "Return deterministic S4 scan for WORLD using explicit ZONE-CENTERS list.
ZONE-CENTERS entries must be ((X . Y) . KIND) in deterministic scan order.
PREVIOUS-LEVEL-MAP may be nil or a prior level map snapshot.
ZONES-CYCLE and SEED control deterministic cadence and stochastic rolls.
GATE-PROVIDER is called as (X Y KIND) and must return plist keys
`:powered', `:road-connected', and `:destination-reachable'.
DEMAND-PROVIDER is called as (KIND) and returns bounded demand.
QUALITY-PROVIDER is called as (X Y KIND) and returns bounded local quality.
CONGESTION-PROVIDER, when non-nil, is called as (X Y) and returns a
congestion value used to compute a bounded soft penalty on raw score.
POLLUTION-PROVIDER, when non-nil, is called as (X Y) and returns local
pollution for residential pollution gate checks."
  (cl-check-type world elcity-world-map)
  (cl-check-type zone-centers list)
  (cl-check-type zones-cycle integer)
  (cl-check-type seed integer)
  (unless (or (null previous-level-map)
              (elcity-zone-level-map-p previous-level-map))
    (error "PREVIOUS-LEVEL-MAP must be nil or `elcity-zone-level-map'"))
  (unless (functionp gate-provider)
    (error "GATE-PROVIDER must be a function"))
  (unless (functionp demand-provider)
    (error "DEMAND-PROVIDER must be a function"))
  (unless (functionp quality-provider)
    (error "QUALITY-PROVIDER must be a function"))
  (if (and (null zone-centers)
           previous-level-map
           (elcity-zones-level-map-compatible-p world previous-level-map))
      (elcity-zone-dynamics-scan-create
       :level-map previous-level-map
       :summary (elcity-zones-empty-summary)
       :transitions nil
       :zone-centers nil)
    (let* ((level-map (elcity-zones-level-map-for-world world 0))
           (cells (copy-sequence (elcity-zone-level-map-cells level-map)))
           (width (elcity-zone-level-map-width level-map))
           (residential-pop 0)
           (commercial-pop 0)
           (industrial-pop 0)
           (grown 0)
           (declined 0)
           (held 0)
           (blocked-power 0)
           (blocked-traffic 0)
           (blocked-both 0)
           (blocked-pollution 0)
           (blocked-any 0)
           (evaluated 0)
           (zone-index 0)
           transitions)
      (dolist (entry zone-centers)
        (let* ((coord (car entry))
               (x (car coord))
               (y (cdr coord))
               (kind (cdr entry))
               (from-level (elcity-zones--initial-level
                            world
                            previous-level-map
                            x
                            y
                            kind))
               (to-level from-level)
               (decision 'hold))
          ;; Cadence gate: only 1/8 of discovered zone centers are evaluated
          ;; per tick; non-eligible zones hold current level by construction.
          (when (elcity-zones-migration-eligible-p zone-index zones-cycle)
            (setq evaluated (1+ evaluated))
            (let* ((gate (funcall gate-provider x y kind))
                   (powered (plist-get gate :powered))
                   (road-connected (plist-get gate :road-connected))
                   (destination-reachable (plist-get gate :destination-reachable))
                   (block-reason (elcity-zones-gate-block-reason
                                   powered
                                   road-connected
                                   destination-reachable)))
              ;; Infrastructure gate reasons feed summary counters and later
              ;; floor/cap behavior in effective-score computation.
              (pcase block-reason
                ('power (setq blocked-power (1+ blocked-power)))
                ((or 'traffic-no-road 'traffic-blocked)
                 (setq blocked-traffic (1+ blocked-traffic)))
                ((or 'both-no-road 'both-blocked)
                 (setq blocked-both (1+ blocked-both))))
              (let* ((demand (elcity-zones-clamp-demand
                              (funcall demand-provider kind)))
                     (local-quality (elcity-zones-clamp-local-quality
                                     (funcall quality-provider x y kind)))
                     ;; Congestion is a soft drag applied to otherwise
                     ;; demand+quality-driven desirability.
                     (congestion-pen (if congestion-provider
                                         (elcity-zones-congestion-penalty
                                          (funcall congestion-provider x y))
                                       0))
                     (infra-gated (not (null block-reason)))
                     (local-pollution (if (and pollution-provider
                                              (eq kind 'residential)
                                              (not infra-gated))
                                         (funcall pollution-provider x y)
                                       0))
                     (pollution-gated (elcity-zones-pollution-gated-p
                                       kind local-pollution))
                     (any-gated (or infra-gated pollution-gated))
                     ;; Decision score pipeline:
                     ;; 1) raw desirability = demand + local-quality - congestion
                     ;; 2) infra gates override score to hard/soft decline floors
                     ;; 3) residential pollution gate caps score below growth
                     ;;    threshold (blocks growth without forced decline)
                     (effective-score (elcity-zones--apply-pollution-growth-cap
                                       (elcity-zones-effective-score
                                        (- (+ demand local-quality)
                                           congestion-pen)
                                        powered
                                        road-connected
                                        destination-reachable)
                                       pollution-gated))
                     (roll (elcity-zones-deterministic-roll
                            seed
                            zones-cycle
                            x
                            y
                            kind)))
                (when (and (not infra-gated) pollution-gated)
                  (setq blocked-pollution (1+ blocked-pollution)))
                (when any-gated
                  (setq blocked-any (1+ blocked-any)))
                ;; Deterministic roll keeps outcomes replay-stable while still
                ;; translating score into grow/hold/decline probabilities.
                (setq decision (elcity-zones-decision-for-score effective-score roll))
                (setq to-level (elcity-zones-apply-decision from-level decision)))))
          (pcase decision
            ('grow (setq grown (1+ grown)))
            ('decline (setq declined (1+ declined)))
            ('hold (setq held (1+ held))))
          ;; Transitions only track true level changes; holds are summary-only.
          (when (/= from-level to-level)
            (push (elcity-zone-transition-create
                   :x x
                   :y y
                   :kind kind
                   :from-level from-level
                   :to-level to-level
                   :reason decision)
                  transitions))
          ;; Population is derived from final per-zone level each tick, never
          ;; from mutable counters stored on zone tiles.
          (aset cells (+ x (* y width)) to-level)
          (pcase kind
            ('residential
             (setq residential-pop (+ residential-pop
                                      (elcity-zones-population-for-level
                                       kind
                                       to-level))))
            ('commercial
             (setq commercial-pop (+ commercial-pop
                                     (elcity-zones-population-for-level
                                      kind
                                      to-level))))
            ('industrial
             (setq industrial-pop (+ industrial-pop
                                     (elcity-zones-population-for-level
                                      kind
                                      to-level)))))
          (setq zone-index (1+ zone-index))))
      (setf (elcity-zone-level-map-cells level-map) cells)
      (elcity-zone-dynamics-scan-create
       :level-map level-map
       :summary (elcity-zone-dynamics-summary-create
                 :residential-pop residential-pop
                 :commercial-pop commercial-pop
                 :industrial-pop industrial-pop
                 :grown grown
                 :declined declined
                 :held held
                 :blocked-power blocked-power
                 :blocked-traffic blocked-traffic
                 :blocked-both blocked-both
                 :blocked-pollution blocked-pollution
                 :blocked-any blocked-any
                 :evaluated evaluated
                 :total-zones zone-index)
       :transitions (nreverse transitions)
       :zone-centers zone-centers))))

(defun elcity-zones-scan-world (world previous-level-map zones-cycle seed
                                      gate-provider demand-provider quality-provider
                                      &optional congestion-provider
                                      pollution-provider)
  "Return deterministic S4 zone dynamics scan for WORLD.
PREVIOUS-LEVEL-MAP may be nil or a prior level map snapshot.
ZONES-CYCLE and SEED control deterministic cadence and stochastic rolls.
GATE-PROVIDER is called as (X Y KIND) and must return plist keys
`:powered', `:road-connected', and `:destination-reachable'.
DEMAND-PROVIDER is called as (KIND) and returns bounded demand.
QUALITY-PROVIDER is called as (X Y KIND) and returns bounded local quality.
CONGESTION-PROVIDER, when non-nil, is called as (X Y) returning congestion.
POLLUTION-PROVIDER, when non-nil, is called as (X Y) returning pollution."
  (cl-check-type world elcity-world-map)
  (elcity-zones-scan-with-zone-centers
   world
   (elcity-zones--zone-center-entries world)
   previous-level-map
   zones-cycle
   seed
   gate-provider
   demand-provider
   quality-provider
   congestion-provider
   pollution-provider))

(defun elcity-zones-scan-with-cached-zone-centers (world previous-scan zones-cycle seed
                                                         gate-provider demand-provider
                                                         quality-provider
                                                         &optional congestion-provider
                                                         pollution-provider)
  "Return deterministic S4 scan for WORLD at ZONES-CYCLE with cached centers.
PREVIOUS-SCAN provides prior zone-center entries and level map snapshots.
This avoids a full world zone-center discovery pass when world topology is
unchanged and cached entries are still valid.
CONGESTION-PROVIDER, when non-nil, is called as (X Y) returning congestion.
POLLUTION-PROVIDER, when non-nil, is called as (X Y) returning pollution."
  (cl-check-type world elcity-world-map)
  (cl-check-type previous-scan elcity-zone-dynamics-scan)
  (let ((cached-centers (elcity-zone-dynamics-scan-zone-centers previous-scan)))
    (elcity-zones-scan-with-zone-centers
     world
     cached-centers
     (elcity-zone-dynamics-scan-level-map previous-scan)
     zones-cycle
     seed
     gate-provider
     demand-provider
     quality-provider
     congestion-provider
     pollution-provider)))

(provide 'elcity-zones)

;;; elcity-zones.el ends here
