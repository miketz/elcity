;;; elcity-quality.el --- S6 spatial quality fields contracts -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; Pure S6 helpers for spatial quality fields.
;;
;; This module owns:
;; - S6 scalar bounds and clamping helpers,
;; - published S6 snapshot struct and deterministic defaults,
;; - deterministic source-map and station-coverage collectors,
;; - center-bias normalization contract for distance-from-map-center inputs,
;; - source-index contracts shared with traffic/zones/station collectors.

;;; Code:

(require 'cl-lib)
(require 'elcity-terrain)
(require 'elcity-tile-field)
(require 'elcity-tile)
(require 'elcity-tile-family)
(require 'elcity-power)
(require 'elcity-traffic)
(require 'elcity-util)
(require 'elcity-world)
(require 'elcity-zones)

(declare-function elcity-building-footprint-index-p "elcity-building" (index kind))

(defconst elcity-quality-scalar-min 0
  "Minimum scalar value accepted by S6 fields and source maps.")

(defconst elcity-quality-scalar-max 1000
  "Maximum scalar value accepted by S6 fields and source maps.")

;; S6 road-source contracts intentionally mirror S3 road index constants.
(defconst elcity-quality-road-index-min elcity-traffic-road-index-min
  "Lowest drivable tile index accepted as an S6 road source.")

(defconst elcity-quality-road-index-max elcity-traffic-road-index-max
  "Highest drivable tile index accepted as an S6 road source.")

(defconst elcity-quality-fire-station-center-index
  (elcity-tile-family-center-index 'fire-station)
  "Tile index for fire station center source used by S6 coverage collectors.
Derived from the canonical family registry.")

(defconst elcity-quality-police-station-center-index
  (elcity-tile-family-center-index 'police-station)
  "Tile index for police station center source used by S6 coverage collectors.
Derived from the canonical family registry.")

(defconst elcity-quality-park-center-index
  (elcity-tile-family-center-index 'park)
  "Tile index for park center tile used by S6 park coverage collectors.
Derived from the canonical family registry.")


(defun elcity-quality-station-center-index-p (index)
  "Return non-nil when tile INDEX is a fire or police station center."
  (or (= index elcity-quality-fire-station-center-index)
      (= index elcity-quality-police-station-center-index)))

(defconst elcity-quality-center-bias-min elcity-quality-scalar-min
  "Minimum normalized center-bias value.")

(defconst elcity-quality-center-bias-max elcity-quality-scalar-max
  "Maximum normalized center-bias value.")

(defconst elcity-quality-road-source-normalization-denominator 4
  "Calibration denominator for road source raw->1000 normalization.
This is a scaling knob, not a physical per-cell maximum.")

(defconst elcity-quality-population-source-normalization-denominator 40
  "Calibration denominator for population source raw->1000 normalization.
Calibrated so max level-3 residential zone population (40) maps to 1000.
This is a scaling knob, not a physical per-cell maximum.")

(defconst elcity-quality-industry-source-normalization-denominator 1
  "Calibration denominator for industry source raw->1000 normalization.
This is a scaling knob, not a physical per-cell maximum.")

(defconst elcity-quality-plant-source-normalization-denominator 1
  "Calibration denominator for plant source raw->1000 normalization.
This is a scaling knob, not a physical per-cell maximum.")

(defconst elcity-quality-station-source-normalization-denominator 1
  "Calibration denominator for station source raw->1000 normalization.
This is a scaling knob, not a physical per-cell maximum.")

(defconst elcity-quality-station-coverage-radius 20
  "Coverage falloff radius in tile cells for station influence.")

(defconst elcity-quality-park-coverage-radius 4
  "Coverage falloff radius in tile cells for park pollution mitigation.")

(defconst elcity-quality--park-offset-table
  (let ((radius elcity-quality-park-coverage-radius)
        (smax elcity-quality-scalar-max)
        entries)
    (cl-loop for dy from (- radius) to radius do
             (cl-loop for dx from (- radius) to radius do
                      (let ((dist (+ (abs dx) (abs dy))))
                        (when (<= dist radius)
                          (let ((influence
                                 (max 0 (min smax
                                             (round (* smax (- radius dist))
                                                    radius)))))
                            (when (> influence 0)
                              (push (vector dx dy influence) entries)))))))
    (vconcat (nreverse entries)))
  "Precomputed offset table for park coverage stamping.
Each entry is [dx dy influence] for Manhattan distance <= radius.")

(defconst elcity-quality-smoothing-alpha-numerator 1
  "S6 smoothing alpha numerator for cardinal-neighbor diffusion pass.")

(defconst elcity-quality-smoothing-alpha-denominator 4
  "S6 smoothing alpha denominator for cardinal-neighbor diffusion pass.")

(defconst elcity-quality-smoothing-passes 2
  "Number of cardinal-neighbor smoothing passes applied per S6 tick.")

(defconst elcity-quality-pollution-smoothing-passes 4
  "Number of Micropolis-kernel smoothing passes for pollution per S6 tick.
Pollution uses a stronger diffusion kernel than crime/land-value fields.
Calibration keeps passes moderate and drives near-source pressure primarily
through source intensity (coal/industry weights).")

(defconst elcity-quality-pollution-weight-industry 400
  "Weight for industry intensity in S6 pollution raw formula.")

(defconst elcity-quality-pollution-weight-roads 200
  "Weight for road intensity in S6 pollution raw formula.")

(defconst elcity-quality-pollution-weight-population 200
  "Weight for population intensity in S6 pollution raw formula.")

(defconst elcity-quality-pollution-weight-plant-coal 800
  "Weight for coal plant intensity in S6 pollution raw formula.
Dominant pollution source — visually strong on overlay.")

(defconst elcity-quality-pollution-weight-plant-nuclear 75
  "Weight for nuclear plant intensity in S6 pollution raw formula.
Reduced vs coal — clean energy theme divergence from Micropolis.")

(defconst elcity-quality-pollution-weight-fire-cover -250
  "Weight for fire coverage in S6 pollution raw formula.")

(defconst elcity-quality-pollution-weight-park-cover -300
  "Weight for park coverage in S6 pollution raw formula.
Parks provide local green-space mitigation within a small radius.")

(defconst elcity-quality-crime-weight-population 700
  "Weight for population intensity in S6 crime raw formula.")

(defconst elcity-quality-crime-weight-roads 400
  "Weight for road intensity in S6 crime raw formula.")

(defconst elcity-quality-crime-weight-pollution 400
  "Weight for pollution raw field in S6 crime raw formula.")

(defconst elcity-quality-crime-weight-police-cover -600
  "Weight for police coverage in S6 crime raw formula.")

(defconst elcity-quality-crime-weight-fire-cover -200
  "Weight for fire coverage in S6 crime raw formula.")

(defconst elcity-quality-land-value-weight-center-bias 450
  "Weight for center-bias input in S6 land-value raw formula.")

(defconst elcity-quality-land-value-weight-road-access 200
  "Weight for road-access input in S6 land-value raw formula.")

(defconst elcity-quality-land-value-weight-fire-cover 150
  "Weight for fire coverage input in S6 land-value raw formula.")

(defconst elcity-quality-land-value-weight-police-cover 200
  "Weight for police coverage input in S6 land-value raw formula.")

(defconst elcity-quality-land-value-weight-pollution-complement 500
  "Weight for pollution complement input in S6 land-value raw formula.")

(defconst elcity-quality-land-value-weight-crime-complement 500
  "Weight for crime complement input in S6 land-value raw formula.")

(defconst elcity-quality-land-value-weight-water-bonus 500
  "Weight for water-proximity bonus input in S6 land-value raw formula.
Applies shoreline bonus from `elcity-terrain-shoreline-bonus-map'.")

(defconst elcity-quality-congestion-bonus-max 1800
  "Maximum congestion bonus permille added to road-pollution scaling.
At cap congestion with default infra, a single road tile contributes ~140
pollution points via the road term.")

(defconst elcity-quality-congestion-scaling-base 1000
  "Base multiplier permille for congestion scaling.
Preserves zero-congestion baseline.")

(cl-defstruct (elcity-quality-snapshot
               (:constructor elcity-quality-snapshot-create))
  "Published S6 fields and aggregates.
POLLUTION-MAP, LAND-VALUE-MAP, and CRIME-MAP are tile-resolution
`elcity-tile-field' values with per-cell scalars in [0,1000].
POLLUTION-AVG and LAND-VALUE-AVG are map averages in [0,1000].
CRIME-AVG is a center-population-weighted aggregate scalar in [0,1000].
FIRE-COVERAGE-MAP and POLICE-COVERAGE-MAP are optional tile-resolution
`elcity-tile-field' values with per-cell scalars in [0,1000],
representing effectiveness-scaled station coverage falloff.  May be nil
when loaded from older saves or before first S6 tick."
  (pollution-map nil :type (or null elcity-tile-field))
  (land-value-map nil :type (or null elcity-tile-field))
  (crime-map nil :type (or null elcity-tile-field))
  (pollution-avg 0 :type integer)
  (land-value-avg 0 :type integer)
  (crime-avg 0 :type integer)
  (fire-coverage-map nil :type (or null elcity-tile-field))
  (police-coverage-map nil :type (or null elcity-tile-field)))

(cl-defstruct (elcity-quality-source-bundle
               (:constructor elcity-quality-source-bundle-create))
  "Collected normalized S6 source and coverage maps.
All maps are tile-resolution `elcity-tile-field' values with cell scalars in [0,1000].
COAL-PLANT-MAP and NUCLEAR-PLANT-MAP are normalized plant-center presence
maps used as separate pollution source terms.
POPULATION-MAP is center-only zone population intensity used by pollution.
CRIME-DENSITY-MAP is footprint-stamped zone density used by crime."
  (industry-map nil :type elcity-tile-field)
  (road-map nil :type elcity-tile-field)
  (population-map nil :type elcity-tile-field)
  (crime-density-map nil :type (or null elcity-tile-field))
  (coal-plant-map nil :type elcity-tile-field)
  (nuclear-plant-map nil :type elcity-tile-field)
  (fire-station-map nil :type elcity-tile-field)
  (police-station-map nil :type elcity-tile-field)
  (center-bias-map nil :type elcity-tile-field)
  (fire-coverage-map nil :type elcity-tile-field)
  (police-coverage-map nil :type elcity-tile-field)
  (road-pollution-map nil :type (or null elcity-tile-field))
  (park-coverage-map nil :type (or null elcity-tile-field))
  (water-bonus-map nil :type (or null elcity-tile-field)))

(defun elcity-quality-clamp-scalar (value)
  "Return S6 scalar VALUE clamped to [0,1000]."
  (elcity-util-clamp value
                         elcity-quality-scalar-min
                         elcity-quality-scalar-max))

(defun elcity-quality-road-index-p (tile-index)
  "Return non-nil when TILE-INDEX is a drivable S6 road source.
This intentionally delegates to `elcity-traffic-road-index-p' so S3 and S6
road-source classification stays aligned."
  (cl-check-type tile-index integer)
  (elcity-traffic-road-index-p tile-index))

(defun elcity-quality--check-station-kind (kind)
  "Signal an error when KIND is not one supported S6 station symbol."
  (unless (memq kind '(fire police))
    (error "Unknown S6 station kind: %S" kind)))

(defun elcity-quality--station-index-for-kind (kind)
  "Return station center tile index for S6 station KIND."
  (elcity-quality--check-station-kind kind)
  (pcase kind
    ('fire elcity-quality-fire-station-center-index)
    ('police elcity-quality-police-station-center-index)))

(defun elcity-quality--field-template-for-world (world)
  "Return zero-initialized S6 tile field template for WORLD."
  (cl-check-type world elcity-world-map)
  (elcity-tile-field-make (elcity-world-map-width world)
                           (elcity-world-map-height world)
                           0))

(defun elcity-quality--field-zero-like (field)
  "Return zero-initialized S6 tile field with same dimensions as FIELD."
  (cl-check-type field elcity-tile-field)
  (elcity-tile-field-create
   :width (elcity-tile-field-width field)
   :height (elcity-tile-field-height field)
   :cells (make-vector (length (elcity-tile-field-cells field)) 0)))

(defun elcity-quality--field-cell-index (field x y)
  "Return row-major index in FIELD for tile coordinates X,Y."
  (+ x
     (* y (elcity-tile-field-width field))))

(defun elcity-quality--maps-compatible-p (map-a map-b)
  "Return non-nil when MAP-A and MAP-B share S6-compatible dimensions."
  (and (= (elcity-tile-field-width map-a)
          (elcity-tile-field-width map-b))
       (= (elcity-tile-field-height map-a)
          (elcity-tile-field-height map-b))))

(defun elcity-quality--assert-compatible-maps (maps)
  "Signal an error when MAPS contain incompatible S6 field dimensions."
  (cl-check-type maps list)
  (dolist (field maps)
    (cl-check-type field elcity-tile-field))
  (let ((base (car maps)))
    (dolist (field (cdr maps))
      (unless (elcity-quality--maps-compatible-p base field)
        (error "Incompatible S6 field map dimensions")))))

(defun elcity-quality--zone-level-at (world level-map kind x y tile-index)
  "Return deterministic S4 level in WORLD/LEVEL-MAP for KIND at X,Y and TILE-INDEX."
  (cl-check-type world elcity-world-map)
  (when level-map
    (cl-check-type level-map elcity-zone-level-map))
  (elcity-zones-check-kind kind)
  (cl-check-type x integer)
  (cl-check-type y integer)
  (cl-check-type tile-index integer)
  (if (and level-map
           (elcity-zones-level-map-compatible-p world level-map))
      (elcity-zones-level-at level-map x y 0)
    (elcity-zones-level-from-index kind tile-index)))

(defun elcity-quality-center-bias-from-distance (distance max-distance)
  "Return normalized center-bias from DISTANCE and MAX-DISTANCE.
DISTANCE is non-negative distance-from-map-center for an S6 tile-field cell.
MAX-DISTANCE is the non-negative farthest in-map distance used for
normalization.  Output is clamped to [0,1000], where:
- 1000 means at/near map center,
- 0 means at/after the farthest in-map distance."
  (cl-check-type distance number)
  (cl-check-type max-distance number)
  (when (< distance 0)
    (error "DISTANCE must be non-negative: %S" distance))
  (when (< max-distance 0)
    (error "MAX-DISTANCE must be non-negative: %S" max-distance))
  (if (<= max-distance 0)
      elcity-quality-center-bias-max
    (elcity-quality-clamp-scalar
     (round (* elcity-quality-center-bias-max
               (- 1.0
                  (/ (float distance)
                     (float max-distance))))))))

(defun elcity-quality--crime-density-map-from-population-centers (population-map)
  "Return footprint-stamped crime density map derived from center POPULATION-MAP.
POPULATION-MAP is expected to contain zone intensity at center tiles only.
For each center tile (value > 0), stamp the same value into in-bounds 3x3
footprint cells around the center.  Deterministic overwrite order is Y-then-X."
  (cl-check-type population-map elcity-tile-field)
  (let* ((next (elcity-quality--field-zero-like population-map))
         (width (elcity-tile-field-width population-map))
         (height (elcity-tile-field-height population-map))
         (center-cells (elcity-tile-field-cells population-map))
         (next-cells (elcity-tile-field-cells next)))
    (dotimes (y height)
      (dotimes (x width)
        (let* ((center-index (+ x (* y width)))
               (center-value (aref center-cells center-index)))
          (when (> center-value 0)
            (dotimes (dy 3)
              (dotimes (dx 3)
                (let ((fx (+ x (1- dx)))
                      (fy (+ y (1- dy))))
                  (when (and (<= 0 fx) (< fx width)
                             (<= 0 fy) (< fy height))
                    (aset next-cells
                          (+ fx (* fy width))
                          (elcity-quality-clamp-scalar center-value))))))))))
    next))

(defun elcity-quality-station-road-adjacent-p (world x y)
  "Return non-nil when station at X,Y in WORLD has a road-adjacent footprint tile.
Checks all tiles in the 3x3 stamp footprint.  If any tile has a cardinal
road neighbor outside the footprint, the station is road-adjacent."
  (cl-check-type world elcity-world-map)
  (cl-check-type x integer)
  (cl-check-type y integer)
  (catch 'found
    (dotimes (dy 3)
      (dotimes (dx 3)
        (let ((bx (+ (1- x) dx))
              (by (+ (1- y) dy)))
          (when (elcity-world-in-bounds-p world bx by)
            (dolist (delta elcity-cardinal-neighbor-deltas)
              (let ((nx (+ bx (car delta)))
                    (ny (+ by (cdr delta))))
                (when (and (elcity-world-in-bounds-p world nx ny)
                           (elcity-traffic-road-index-p
                            (elcity-tile-index
                             (elcity-world-tile-at world nx ny 0))))
                  (throw 'found t))))))))
    nil))

(defun elcity-quality-station-active-p (world power-grid x y)
  "Return non-nil when station at X,Y in WORLD is powered and road-adjacent.
POWER-GRID is the S2 tile-resolution power grid.  A station is active when
its center tile is powered and at least one tile in the 3x3 stamp footprint
has a cardinal road neighbor outside the footprint."
  (cl-check-type world elcity-world-map)
  (cl-check-type power-grid elcity-power-grid)
  (cl-check-type x integer)
  (cl-check-type y integer)
  (and (elcity-power-grid-cell-at power-grid x y nil)
       (elcity-quality-station-road-adjacent-p world x y)))

(defvar elcity-quality--center-bias-cache nil
  "Cached center-bias map as (WIDTH HEIGHT FIELD).
FIELD is treated as immutable shared data.  Center-bias only depends on map
geometry, not tile contents.")

(defun elcity-quality-center-bias-map (world)
  "Return normalized S6 center-bias map for WORLD tile-field cells.
Result is cached by geometry since center-bias depends only on dimensions."
  (cl-check-type world elcity-world-map)
  (let ((width (elcity-world-map-width world))
        (height (elcity-world-map-height world)))
    (when (or (null elcity-quality--center-bias-cache)
              (/= width (car elcity-quality--center-bias-cache))
              (/= height (cadr elcity-quality--center-bias-cache)))
      (let* ((field (elcity-quality--field-template-for-world world))
             (center-x (/ (float (1- width)) 2.0))
             (center-y (/ (float (1- height)) 2.0))
             (cells (copy-sequence (elcity-tile-field-cells field)))
             (max-distance 0.0))
        ;; Float sqrt/expt are used only in source preprocessing here.
        ;; S6 field formulas and smoothing remain integer-only.
        (dotimes (y height)
          (dotimes (x width)
            (setq max-distance
                  (max max-distance
                       (sqrt (+ (expt (- x center-x) 2)
                                (expt (- y center-y) 2)))))))
        (dotimes (y height)
          (dotimes (x width)
            (let* ((distance (sqrt (+ (expt (- x center-x) 2)
                                      (expt (- y center-y) 2))))
                   (index (elcity-quality--field-cell-index field x y)))
              (aset cells
                    index
                    (elcity-quality-center-bias-from-distance distance max-distance)))))
        (setf (elcity-tile-field-cells field) cells)
        (setq elcity-quality--center-bias-cache (list width height field))))
    ;; Return the cached field by reference; downstream code must treat it
    ;; as read-only.
    (nth 2 elcity-quality--center-bias-cache)))

(defun elcity-quality--water-bonus-field (world)
  "Return tile-field with water-proximity bonus values for WORLD.
Each cell contains a land-value bonus in
[0, `elcity-terrain-shoreline-bonus-max'].
Water tiles and tiles beyond the falloff radius get 0."
  (cl-check-type world elcity-world-map)
  (let* ((width (elcity-world-map-width world))
         (height (elcity-world-map-height world))
         (bonus-vec (elcity-terrain-shoreline-bonus-map world))
         (field (elcity-tile-field-make width height 0)))
    (let ((cells (elcity-tile-field-cells field)))
      (dotimes (i (length bonus-vec))
        (aset cells i (aref bonus-vec i))))
    field))

(defun elcity-quality--station-source-coordinates (station-map)
  "Return deterministic Y-then-X station coordinates from STATION-MAP."
  (cl-check-type station-map elcity-tile-field)
  (let ((width (elcity-tile-field-width station-map))
        (height (elcity-tile-field-height station-map))
        (cells (elcity-tile-field-cells station-map))
        coords)
    (dotimes (y height)
      (dotimes (x width)
        (let ((index (+ x (* y width))))
          (when (> (aref cells index) 0)
            (push (cons x y) coords)))))
    (nreverse coords)))

(defun elcity-quality--station-influence-at-distance (distance)
  "Return unscaled station influence for tile-cell DISTANCE."
  (cl-check-type distance integer)
  (when (< distance 0)
    (error "DISTANCE must be non-negative: %S" distance))
  (if (> distance elcity-quality-station-coverage-radius)
      0
    (elcity-quality-clamp-scalar
     (round (* elcity-quality-scalar-max
               (- elcity-quality-station-coverage-radius distance))
            elcity-quality-station-coverage-radius))))

(defun elcity-quality-station-coverage-map (station-map effectiveness)
  "Return normalized S6 station coverage map from STATION-MAP and EFFECTIVENESS.
EFFECTIVENESS is S7 service effectiveness in permille [0,1000]."
  (cl-check-type station-map elcity-tile-field)
  (cl-check-type effectiveness integer)
  (let* ((effective (elcity-quality-clamp-scalar effectiveness))
         (sources (elcity-quality--station-source-coordinates station-map))
         (coverage (elcity-quality--field-zero-like station-map))
         (width (elcity-tile-field-width coverage))
         (height (elcity-tile-field-height coverage))
         (cells (elcity-tile-field-cells coverage)))
    (dotimes (y height)
      (dotimes (x width)
        (let ((base-value 0))
          (dolist (source sources)
            (let* ((distance (+ (abs (- x (car source)))
                                (abs (- y (cdr source)))))
                   (influence (elcity-quality--station-influence-at-distance
                               distance)))
              (when (> influence base-value)
                (setq base-value influence))))
          (aset cells
                (elcity-quality--field-cell-index coverage x y)
                (elcity-quality-clamp-scalar
                 (round (* base-value effective)
                        elcity-quality-scalar-max))))))
    coverage))

(defun elcity-quality-park-coverage-map (park-map)
  "Return normalized S6 park coverage map from PARK-MAP.
Parks always provide full coverage (no effectiveness scaling).
Uses source-centric bounded stamping with precomputed offset table."
  (cl-check-type park-map elcity-tile-field)
  (let* ((sources (elcity-quality--station-source-coordinates park-map))
         (coverage (elcity-quality--field-zero-like park-map))
         (width (elcity-tile-field-width coverage))
         (height (elcity-tile-field-height coverage))
         (cells (elcity-tile-field-cells coverage))
         (offsets elcity-quality--park-offset-table)
         (n-offsets (length offsets)))
    (dolist (source sources)
      (let ((sx (car source))
            (sy (cdr source)))
        (dotimes (i n-offsets)
          (let* ((entry (aref offsets i))
                 (tx (+ sx (aref entry 0)))
                 (ty (+ sy (aref entry 1))))
            (when (and (>= tx 0) (< tx width)
                       (>= ty 0) (< ty height))
              (let* ((idx (+ tx (* ty width)))
                     (influence (aref entry 2))
                     (cur (aref cells idx)))
                (when (> influence cur)
                  (aset cells idx influence))))))))
    coverage))

(defun elcity-quality--scale-road-map-by-infra (road-map infra-effectiveness)
  "Return ROAD-MAP with values scaled inversely by INFRA-EFFECTIVENESS.
At 1000 (full funding): unchanged.  At 500: 50% boost.  At 0: doubled.
Formula: min(1000, value * (2000 - infra-eff) / 1000)."
  (cl-check-type road-map elcity-tile-field)
  (cl-check-type infra-effectiveness integer)
  (let* ((next (copy-elcity-tile-field road-map))
         (cells (copy-sequence (elcity-tile-field-cells next)))
         (multiplier (- 2000 infra-effectiveness)))
    (setf (elcity-tile-field-cells next) cells)
    (dotimes (i (length cells))
      (aset cells i
            (elcity-quality-clamp-scalar
             (/ (* (aref cells i) multiplier) 1000))))
    next))

(defun elcity-quality--scale-road-map-by-congestion (road-map congestion-map)
  "Return ROAD-MAP with values scaled up by CONGESTION-MAP congestion.
When CONGESTION-MAP is nil, return a copy with unchanged values.
Formula: min(1000, road * (base + bonus) / 1000) where
bonus = (bonus-max * C + cap/2) / cap, C = congestion per tile."
  (cl-check-type road-map elcity-tile-field)
  (if (null congestion-map)
      (let* ((next (copy-elcity-tile-field road-map))
             (cells (copy-sequence (elcity-tile-field-cells next))))
        (setf (elcity-tile-field-cells next) cells)
        next)
    (cl-check-type congestion-map elcity-tile-field)
    (unless (and (= (elcity-tile-field-width road-map)
                    (elcity-tile-field-width congestion-map))
                 (= (elcity-tile-field-height road-map)
                    (elcity-tile-field-height congestion-map)))
      (error "S6 congestion scaling: road-map %dx%d != congestion-map %dx%d"
             (elcity-tile-field-width road-map)
             (elcity-tile-field-height road-map)
             (elcity-tile-field-width congestion-map)
             (elcity-tile-field-height congestion-map)))
    (let* ((next (copy-elcity-tile-field road-map))
           (cells (copy-sequence (elcity-tile-field-cells next)))
           (cong-cells (elcity-tile-field-cells congestion-map))
           (cap elcity-traffic-congestion-cap)
           (half-cap (/ cap 2))
           (bonus-max elcity-quality-congestion-bonus-max)
           (base elcity-quality-congestion-scaling-base))
      (setf (elcity-tile-field-cells next) cells)
      (dotimes (i (length cells))
        (let* ((c (aref cong-cells i))
               (bonus (/ (+ (* bonus-max c) half-cap) cap))
               (multiplier (+ base bonus)))
          (aset cells i
                (elcity-quality-clamp-scalar
                 (/ (* (aref cells i) multiplier) 1000)))))
      next)))

(defun elcity-quality--fused-world-scan (world level-map power-grid)
  "Scan WORLD tiles once and return plist of raw source vectors.
LEVEL-MAP is optional S4 level map.  POWER-GRID is optional S2 power grid.
Returns plist with keys :industry :road :population :coal :nuclear
:fire-station :police-station :park, each a raw integer vector."
  (let* ((tiles (elcity-world-map-tiles world))
         (world-width (elcity-world-map-width world))
         (world-height (elcity-world-map-height world))
         (cell-count (* world-width world-height))
         (industry-raw (make-vector cell-count 0))
         (road-raw (make-vector cell-count 0))
         (population-raw (make-vector cell-count 0))
         (coal-raw (make-vector cell-count 0))
         (nuclear-raw (make-vector cell-count 0))
         (fire-station-raw (make-vector cell-count 0))
         (police-station-raw (make-vector cell-count 0))
         (park-raw (make-vector cell-count 0))
         ;; Plant footprint constants.
         (coal-center elcity-power-coal-plant-index)
         (nuclear-center elcity-power-nuclear-plant-index)
         (fire-center-idx elcity-quality-fire-station-center-index)
         (police-center-idx elcity-quality-police-station-center-index)
         (park-center-idx elcity-quality-park-center-index)
         ;; Deferred plant footprint stamps (lists of center tile indices).
         coal-centers nuclear-centers
         fire-centers police-centers)
    ;; Single pass over all tiles.
    (dotimes (i cell-count)
      (let* ((tile (aref tiles i))
             (idx (elcity-tile-index tile)))
        ;; Road classification.
        (when (elcity-traffic-road-index-p idx)
          (aset road-raw i 1))
        ;; Industry classification (center + border tiles).
        (when (eq 'industrial (elcity-zones-kind-for-index idx))
          (aset industry-raw i 1))
        ;; Zone population (center tiles only).
        (when (elcity-tile-zone-center-p tile)
          (let ((kind (elcity-zones-kind-for-index idx)))
            (when kind
              (let* ((x (% i world-width))
                     (y (/ i world-width))
                     (level (elcity-quality--zone-level-at world level-map
                                                           kind x y idx))
                     (pop (elcity-zones-population-for-level kind level)))
                (aset population-raw i pop)))))
        ;; Plant centers — record position for footprint stamp.
        (cond
         ((= idx coal-center)
          (push i coal-centers))
         ((= idx nuclear-center)
          (push i nuclear-centers)))
        ;; Station centers — record position for active check.
        (cond
         ((= idx fire-center-idx)
          (let* ((x (% i world-width))
                 (y (/ i world-width)))
            (when (or (null power-grid)
                      (elcity-quality-station-active-p world power-grid x y))
              (push i fire-centers))))
         ((= idx police-center-idx)
          (let* ((x (% i world-width))
                 (y (/ i world-width)))
            (when (or (null power-grid)
                      (elcity-quality-station-active-p world power-grid x y))
              (push i police-centers)))))
        ;; Park tiles (1x1, no footprint stamp needed).
        (when (= idx park-center-idx)
          (aset park-raw i 1))))
    ;; Stamp plant footprints.
    (dolist (ci coal-centers)
      (let* ((cx (% ci world-width))
             (cy (/ ci world-width))
             (left-x (1- cx))
             (top-y (1- cy)))
        (dotimes (dy 3)
          (dotimes (dx 3)
            (let ((fx (+ left-x dx))
                  (fy (+ top-y dy)))
              (when (and (<= 0 fx) (< fx world-width)
                         (<= 0 fy) (< fy world-height))
                (let* ((fi (+ fx (* fy world-width)))
                       (fid (elcity-tile-index (aref tiles fi))))
                  (when (elcity-building-footprint-index-p fid 'coal-plant)
                    (cl-incf (aref coal-raw fi))))))))))
    (dolist (ci nuclear-centers)
      (let* ((cx (% ci world-width))
             (cy (/ ci world-width))
             (left-x (- cx 2))
             (top-y (- cy 2)))
        (dotimes (dy 4)
          (dotimes (dx 4)
            (let ((fx (+ left-x dx))
                  (fy (+ top-y dy)))
              (when (and (<= 0 fx) (< fx world-width)
                         (<= 0 fy) (< fy world-height))
                (let* ((fi (+ fx (* fy world-width)))
                       (fid (elcity-tile-index (aref tiles fi))))
                  (when (elcity-building-footprint-index-p fid 'nuclear-plant)
                    (cl-incf (aref nuclear-raw fi))))))))))
    ;; Stamp station source marks.
    (dolist (ci fire-centers)
      (cl-incf (aref fire-station-raw ci)))
    (dolist (ci police-centers)
      (cl-incf (aref police-station-raw ci)))
    (list :industry industry-raw
          :road road-raw
          :population population-raw
          :coal coal-raw
          :nuclear nuclear-raw
          :fire-station fire-station-raw
          :police-station police-station-raw
          :park park-raw)))

(defun elcity-quality--raw-to-field (world raw-cells denominator)
  "Return normalized tile field from RAW-CELLS vector for WORLD with DENOMINATOR."
  (cl-check-type world elcity-world-map)
  (cl-check-type raw-cells vector)
  (cl-check-type denominator integer)
  (when (<= denominator 0)
    (error "DENOMINATOR must be positive: %S" denominator))
  (let* ((field (elcity-quality--field-template-for-world world))
         (cells (elcity-tile-field-cells field))
         (limit (length cells)))
    (unless (= (length raw-cells) limit)
      (error "RAW-CELLS length %d does not match world field length %d"
             (length raw-cells)
             limit))
    (dotimes (i limit)
      (aset cells i
            (elcity-quality-clamp-scalar
             (round (* elcity-quality-scalar-max
                       (max 0 (aref raw-cells i)))
                    denominator))))
    field))

(defun elcity-quality-collect-source-bundle (world &optional level-map
                                                   fire-effectiveness
                                                   police-effectiveness
                                                   power-grid
                                                   infra-effectiveness
                                                   congestion-map)
  "Return deterministic normalized S6 source/coverage bundle for WORLD.
LEVEL-MAP is optional S4 level map snapshot.  FIRE-EFFECTIVENESS and
POLICE-EFFECTIVENESS are optional S7 effectiveness permille values and default
to 1000.  POWER-GRID is optional S2 power grid; when non-nil, stations must
be powered and road-adjacent to contribute coverage.
INFRA-EFFECTIVENESS scales road pollution inversely (permille, default 1000).
CONGESTION-MAP is optional S3 congestion tile-field; when non-nil, produces
a congestion-scaled ROAD-POLLUTION-MAP for pollution formulas.
Also derives CRIME-DENSITY-MAP from center-only POPULATION-MAP using a
deterministic 3x3 zone-footprint stamp."
  (cl-check-type world elcity-world-map)
  (when level-map
    (cl-check-type level-map elcity-zone-level-map))
  (when power-grid
    (cl-check-type power-grid elcity-power-grid))
  (let* ((raw (elcity-quality--fused-world-scan world level-map power-grid))
         (industry-map (elcity-quality--raw-to-field
                        world (plist-get raw :industry)
                        elcity-quality-industry-source-normalization-denominator))
         (raw-road-map (elcity-quality--raw-to-field
                        world (plist-get raw :road)
                        elcity-quality-road-source-normalization-denominator))
         (road-map (if (and infra-effectiveness
                            (/= infra-effectiveness 1000))
                       (elcity-quality--scale-road-map-by-infra
                        raw-road-map infra-effectiveness)
                     raw-road-map))
         (population-map (elcity-quality--raw-to-field
                          world (plist-get raw :population)
                          elcity-quality-population-source-normalization-denominator))
         (crime-density-map
          (elcity-quality--crime-density-map-from-population-centers population-map))
         (coal-plant-map (elcity-quality--raw-to-field
                          world (plist-get raw :coal)
                          elcity-quality-plant-source-normalization-denominator))
         (nuclear-plant-map (elcity-quality--raw-to-field
                             world (plist-get raw :nuclear)
                             elcity-quality-plant-source-normalization-denominator))
         (fire-station-map (elcity-quality--raw-to-field
                            world (plist-get raw :fire-station)
                            elcity-quality-station-source-normalization-denominator))
         (police-station-map (elcity-quality--raw-to-field
                              world (plist-get raw :police-station)
                              elcity-quality-station-source-normalization-denominator))
         (center-bias-map (elcity-quality-center-bias-map world))
         (fire-coverage-map (elcity-quality-station-coverage-map
                             fire-station-map
                             (or fire-effectiveness
                                 elcity-quality-scalar-max)))
         (police-coverage-map (elcity-quality-station-coverage-map
                               police-station-map
                               (or police-effectiveness
                                   elcity-quality-scalar-max)))
         (road-pollution-map
          (elcity-quality--scale-road-map-by-congestion road-map congestion-map))
         (park-map (elcity-quality--raw-to-field
                    world (plist-get raw :park)
                    elcity-quality-station-source-normalization-denominator))
         (park-coverage-map (elcity-quality-park-coverage-map park-map))
         (water-bonus-map (elcity-quality--water-bonus-field world)))
    (elcity-quality-source-bundle-create
     :industry-map industry-map
     :road-map road-map
     :population-map population-map
     :crime-density-map crime-density-map
     :coal-plant-map coal-plant-map
     :nuclear-plant-map nuclear-plant-map
     :fire-station-map fire-station-map
     :police-station-map police-station-map
     :center-bias-map center-bias-map
     :fire-coverage-map fire-coverage-map
     :police-coverage-map police-coverage-map
     :road-pollution-map road-pollution-map
     :park-coverage-map park-coverage-map
     :water-bonus-map water-bonus-map)))

(defun elcity-quality--harmonize-map-by-zone-footprints (field-map zone-center-map)
  "Return FIELD-MAP with zone-footprint cells stamped from center values.
ZONE-CENTER-MAP marks zone centers as positive values at center-only cells.
For each center, FIELD-MAP center value is copied to all in-bounds cells in the
3x3 footprint around that center.  Overwrite order is deterministic Y-then-X."
  (cl-check-type field-map elcity-tile-field)
  (cl-check-type zone-center-map elcity-tile-field)
  (elcity-quality--assert-compatible-maps (list field-map zone-center-map))
  (let* ((width (elcity-tile-field-width field-map))
         (height (elcity-tile-field-height field-map))
         (field-cells (elcity-tile-field-cells field-map))
         (center-cells (elcity-tile-field-cells zone-center-map)))
    (dotimes (y height)
      (dotimes (x width)
        (let ((center-index (+ x (* y width))))
          (when (> (aref center-cells center-index) 0)
            (let ((zone-value (elcity-quality-clamp-scalar
                               (aref field-cells center-index))))
              (dotimes (dy 3)
                (dotimes (dx 3)
                  (let ((fx (+ x (1- dx)))
                        (fy (+ y (1- dy))))
                    (when (and (<= 0 fx) (< fx width)
                               (<= 0 fy) (< fy height))
                      (aset field-cells (+ fx (* fy width)) zone-value))))))))))
    field-map))

(defun elcity-quality-pollution-raw-map (sources)
  "Return clamped pollution raw map computed from S6 SOURCES bundle.
Uses ROAD-POLLUTION-MAP (congestion-scaled) for road contribution when
available, falling back to ROAD-MAP.  Park coverage applies as a negative
mitigation term when PARK-COVERAGE-MAP is present."
  (cl-check-type sources elcity-quality-source-bundle)
  (let* ((industry-map (elcity-quality-source-bundle-industry-map sources))
         (road-map (or (elcity-quality-source-bundle-road-pollution-map sources)
                       (elcity-quality-source-bundle-road-map sources)))
         (population-map (elcity-quality-source-bundle-population-map sources))
         (coal-plant-map (elcity-quality-source-bundle-coal-plant-map sources))
         (nuclear-plant-map (elcity-quality-source-bundle-nuclear-plant-map sources))
         (fire-cover-map (elcity-quality-source-bundle-fire-coverage-map sources))
         (park-cover-map (or (elcity-quality-source-bundle-park-coverage-map sources)
                             (elcity-quality--field-zero-like industry-map))))
    (elcity-quality--assert-compatible-maps
     (list industry-map road-map population-map
           coal-plant-map nuclear-plant-map fire-cover-map park-cover-map))
    (let* ((next (elcity-quality--field-zero-like industry-map))
           (next-cells (elcity-tile-field-cells next))
           (industry-cells (elcity-tile-field-cells industry-map))
           (road-cells (elcity-tile-field-cells road-map))
           (population-cells (elcity-tile-field-cells population-map))
           (coal-cells (elcity-tile-field-cells coal-plant-map))
           (nuclear-cells (elcity-tile-field-cells nuclear-plant-map))
           (fire-cover-cells (elcity-tile-field-cells fire-cover-map))
           (park-cover-cells (elcity-tile-field-cells park-cover-map))
           (limit (length next-cells))
           ;; Hoist weight constants for inlined formula.
           (w-ind elcity-quality-pollution-weight-industry)
           (w-road elcity-quality-pollution-weight-roads)
           (w-pop elcity-quality-pollution-weight-population)
           (w-coal elcity-quality-pollution-weight-plant-coal)
           (w-nuke elcity-quality-pollution-weight-plant-nuclear)
           (w-fire elcity-quality-pollution-weight-fire-cover)
           (w-park elcity-quality-pollution-weight-park-cover))
      ;; Clamp to [0,1000] and divide by 1000 after summing weighted
      ;; normalized sources.  Park coverage adds a negative mitigation term.
      (dotimes (index limit)
        (aset next-cells index
              (max 0 (min 1000
                          (round (+ (* w-ind (aref industry-cells index))
                                    (* w-road (aref road-cells index))
                                    (* w-pop (aref population-cells index))
                                    (* w-coal (aref coal-cells index))
                                    (* w-nuke (aref nuclear-cells index))
                                    (* w-fire (aref fire-cover-cells index))
                                    (* w-park (aref park-cover-cells index)))
                                 1000)))))
      next)))

(defun elcity-quality-crime-raw-map (sources pollution-raw-map)
  "Return clamped crime raw map from SOURCES bundle and POLLUTION-RAW-MAP."
  (cl-check-type sources elcity-quality-source-bundle)
  (cl-check-type pollution-raw-map elcity-tile-field)
  (let* ((population-map (elcity-quality-source-bundle-population-map sources))
         ;; Prefer precomputed crime-density when provided by source collection.
         ;; Fallback keeps callers compatible with older bundles.
         (crime-density-map (or (elcity-quality-source-bundle-crime-density-map sources)
                                population-map))
         (road-map (elcity-quality-source-bundle-road-map sources))
         (police-cover-map (elcity-quality-source-bundle-police-coverage-map sources))
         (fire-cover-map (elcity-quality-source-bundle-fire-coverage-map sources)))
    ;; Hard-fail on any dimension mismatch before vector indexing in hot loop.
    (elcity-quality--assert-compatible-maps
     (list population-map
           crime-density-map
           road-map
           pollution-raw-map
           police-cover-map
           fire-cover-map))
    ;; Hoist source cell vectors and weights once; loop stays integer-only.
    (let* ((next (elcity-quality--field-zero-like population-map))
           (next-cells (elcity-tile-field-cells next))
           (crime-density-cells (elcity-tile-field-cells crime-density-map))
           (road-cells (elcity-tile-field-cells road-map))
           (pollution-cells (elcity-tile-field-cells pollution-raw-map))
           (police-cover-cells (elcity-tile-field-cells police-cover-map))
           (fire-cover-cells (elcity-tile-field-cells fire-cover-map))
           (limit (length next-cells))
           (w-pop elcity-quality-crime-weight-population)
           (w-road elcity-quality-crime-weight-roads)
           (w-poll elcity-quality-crime-weight-pollution)
           (w-police elcity-quality-crime-weight-police-cover)
           (w-fire elcity-quality-crime-weight-fire-cover))
      ;; Keep this inlined formula equivalent to historical
      ;; `elcity-quality-crime-raw-value`: clamp to [0,1000] with /1000
      ;; normalization after weighted source summation.
      (dotimes (index limit)
        ;; One row-major tile-cell sample per iteration.
        (aset next-cells index
              (max 0 (min 1000
                          (round (+ (* w-pop (aref crime-density-cells index))
                                    (* w-road (aref road-cells index))
                                    (* w-poll (aref pollution-cells index))
                                    (* w-police (aref police-cover-cells index))
                                    (* w-fire (aref fire-cover-cells index)))
                                 1000)))))
      ;; Harmonize to zone footprints so each developed zone shares one raw value.
      (elcity-quality--harmonize-map-by-zone-footprints next population-map))))

(defun elcity-quality-land-value-raw-map (sources pollution-raw-map crime-raw-map)
  "Return clamped land-value raw map from SOURCES and raw risk maps.
POLLUTION-RAW-MAP and CRIME-RAW-MAP are consumed as direct risk terms.
Water-proximity bonus from SOURCES is added as a positive term."
  (cl-check-type sources elcity-quality-source-bundle)
  (cl-check-type pollution-raw-map elcity-tile-field)
  (cl-check-type crime-raw-map elcity-tile-field)
  (let* ((center-bias-map (elcity-quality-source-bundle-center-bias-map sources))
         (road-map (elcity-quality-source-bundle-road-map sources))
         (fire-cover-map (elcity-quality-source-bundle-fire-coverage-map sources))
         (police-cover-map (elcity-quality-source-bundle-police-coverage-map sources))
         (water-bonus-map (elcity-quality-source-bundle-water-bonus-map sources)))
    (elcity-quality--assert-compatible-maps
     (append
      (list center-bias-map
            road-map
            fire-cover-map
            police-cover-map
            pollution-raw-map
            crime-raw-map)
      (when water-bonus-map
        (list water-bonus-map))))
    (let* ((next (elcity-quality--field-zero-like center-bias-map))
           (next-cells (elcity-tile-field-cells next))
           (center-cells (elcity-tile-field-cells center-bias-map))
           (road-cells (elcity-tile-field-cells road-map))
           (fire-cover-cells (elcity-tile-field-cells fire-cover-map))
           (police-cover-cells (elcity-tile-field-cells police-cover-map))
           (pollution-cells (elcity-tile-field-cells pollution-raw-map))
           (crime-cells (elcity-tile-field-cells crime-raw-map))
           (water-cells (and water-bonus-map
                             (elcity-tile-field-cells water-bonus-map)))
           (limit (length next-cells))
           (w-center elcity-quality-land-value-weight-center-bias)
           (w-road elcity-quality-land-value-weight-road-access)
           (w-fire elcity-quality-land-value-weight-fire-cover)
           (w-police elcity-quality-land-value-weight-police-cover)
           (w-poll-c elcity-quality-land-value-weight-pollution-complement)
           (w-crime-c elcity-quality-land-value-weight-crime-complement)
           (w-water elcity-quality-land-value-weight-water-bonus))
      ;; Formula: weighted sum of base terms plus water-proximity bonus,
      ;; divided by 2000, clamped to [0,1000].  Water bonus is additive
      ;; (can push value above the base formula result near shorelines).
      (dotimes (index limit)
        (let ((pollution (max 0 (min 1000 (aref pollution-cells index))))
              (crime (max 0 (min 1000 (aref crime-cells index))))
              (water (if water-cells (aref water-cells index) 0)))
          (aset next-cells index
                (max 0 (min 1000
                            (round (+ (* w-center (aref center-cells index))
                                       (* w-road (aref road-cells index))
                                       (* w-fire (aref fire-cover-cells index))
                                       (* w-police (aref police-cover-cells index))
                                       (* w-poll-c (- 1000 pollution))
                                       (* w-crime-c (- 1000 crime))
                                       (* w-water water))
                                    2000))))))
      next)))

(defun elcity-quality-smooth-map (source-map)
  "Return SOURCE-MAP smoothed with one double-buffer cardinal pass.
Edge cells use only in-bounds cardinal neighbors (no virtual zero-padding)."
  (cl-check-type source-map elcity-tile-field)
  (let ((alpha-numerator elcity-quality-smoothing-alpha-numerator)
        (alpha-denominator elcity-quality-smoothing-alpha-denominator))
    (cl-check-type alpha-numerator integer)
    (cl-check-type alpha-denominator integer)
    (when (or (< alpha-numerator 0)
              (<= alpha-denominator 0)
              (> alpha-numerator alpha-denominator))
      (error "Invalid smoothing alpha %S/%S"
             alpha-numerator
             alpha-denominator))
    (let* ((width (elcity-tile-field-width source-map))
           (height (elcity-tile-field-height source-map))
           (source-cells (elcity-tile-field-cells source-map))
           (next (copy-elcity-tile-field source-map))
           (next-cells (copy-sequence source-cells))
           (alpha-complement (- alpha-denominator alpha-numerator))
           (last-x (1- width))
           (last-y (1- height)))
      ;; Double-buffered smoothing: read SOURCE-CELLS, write NEXT-CELLS.
      ;; Index arithmetic inlined: north=index-width, south=index+width,
      ;; west=index-1, east=index+1.
      (dotimes (y height)
        (dotimes (x width)
          (let* ((index (+ x (* y width)))
                 (center (aref source-cells index))
                 (neighbor-sum 0)
                 (neighbor-count 0))
            (when (> y 0)
              (setq neighbor-sum (+ neighbor-sum
                                    (aref source-cells (- index width)))
                    neighbor-count (1+ neighbor-count)))
            (when (< y last-y)
              (setq neighbor-sum (+ neighbor-sum
                                    (aref source-cells (+ index width)))
                    neighbor-count (1+ neighbor-count)))
            (when (> x 0)
              (setq neighbor-sum (+ neighbor-sum
                                    (aref source-cells (1- index)))
                    neighbor-count (1+ neighbor-count)))
            (when (< x last-x)
              (setq neighbor-sum (+ neighbor-sum
                                    (aref source-cells (1+ index)))
                    neighbor-count (1+ neighbor-count)))
            (let* ((neighbor-avg (if (> neighbor-count 0)
                                     (round neighbor-sum neighbor-count)
                                   center))
                   (smoothed (round (+ (* alpha-complement center)
                                       (* alpha-numerator neighbor-avg))
                                    alpha-denominator)))
              (aset next-cells index (max 0 (min 1000 smoothed)))))))
      (setf (elcity-tile-field-cells next) next-cells)
      next)))

(defun elcity-quality-smooth-map-pollution (source-map)
  "Return SOURCE-MAP smoothed with Micropolis-like pollution diffusion.
Uses the non-dither Micropolis kernel: next = (center + N + E + S + W) / 4
for interior cells.  Edge cells use available cardinal neighbors only.
This kernel spreads pollution more aggressively than the default EMA
smoothing used for crime and land-value fields."
  (cl-check-type source-map elcity-tile-field)
  (let* ((width (elcity-tile-field-width source-map))
         (height (elcity-tile-field-height source-map))
         (source-cells (elcity-tile-field-cells source-map))
         (next (copy-elcity-tile-field source-map))
         (next-cells (copy-sequence source-cells))
         (last-x (1- width))
         (last-y (1- height)))
    (dotimes (y height)
      (dotimes (x width)
        (let* ((index (+ x (* y width)))
               (center (aref source-cells index))
               (neighbor-sum 0)
               (neighbor-count 0))
          (when (> y 0)
            (setq neighbor-sum (+ neighbor-sum
                                  (aref source-cells (- index width)))
                  neighbor-count (1+ neighbor-count)))
          (when (< y last-y)
            (setq neighbor-sum (+ neighbor-sum
                                  (aref source-cells (+ index width)))
                  neighbor-count (1+ neighbor-count)))
          (when (> x 0)
            (setq neighbor-sum (+ neighbor-sum
                                  (aref source-cells (1- index)))
                  neighbor-count (1+ neighbor-count)))
          (when (< x last-x)
            (setq neighbor-sum (+ neighbor-sum
                                  (aref source-cells (1+ index)))
                  neighbor-count (1+ neighbor-count)))
          ;; Micropolis non-dither kernel: (center + N + E + S + W) / 4.
          ;; Divides by 4 even with 5 terms, so pollution amplifies
          ;; outward from sources until clamped at [0, 1000].
          ;; Edge/corner cells: divisor = max(neighbor-count, 4).
          ;; Isolated cell (0 neighbors) is unchanged.
          (if (= neighbor-count 0)
              (aset next-cells index center)
            (aset next-cells index
                  (max 0 (min 1000
                              (round (+ center neighbor-sum)
                                     (max neighbor-count 4)))))))))
    (setf (elcity-tile-field-cells next) next-cells)
    next))

(defun elcity-quality--smooth-map-pollution-passes (source-map passes)
  "Return SOURCE-MAP after PASSES pollution-specific smoothing iterations."
  (cl-check-type source-map elcity-tile-field)
  (cl-check-type passes integer)
  (when (< passes 0)
    (error "PASSES must be non-negative: %S" passes))
  (let ((next source-map))
    (dotimes (_ passes)
      (setq next (elcity-quality-smooth-map-pollution next)))
    next))

(defun elcity-quality-map-average (field-map)
  "Return clamped integer average scalar of all cells in FIELD-MAP."
  (cl-check-type field-map elcity-tile-field)
  (let* ((cells (elcity-tile-field-cells field-map))
         (count (length cells))
         (sum 0))
    (dotimes (index count)
      (setq sum (+ sum (aref cells index))))
    (elcity-quality-clamp-scalar
     (if (> count 0)
         (round sum count)
       0))))

(defun elcity-quality-crime-population-weighted-average (crime-map population-map)
  "Return population-weighted crime average from CRIME-MAP and POPULATION-MAP.
POPULATION-MAP is the center-only zone population intensity map; only cells
with positive population weight contribute to the aggregate.  Returns 0 when
total population weight is 0."
  (cl-check-type crime-map elcity-tile-field)
  (cl-check-type population-map elcity-tile-field)
  (elcity-quality--assert-compatible-maps (list crime-map population-map))
  (let* ((crime-cells (elcity-tile-field-cells crime-map))
         (population-cells (elcity-tile-field-cells population-map))
         (limit (length crime-cells))
         (weighted-sum 0)
         (total-weight 0))
    (dotimes (index limit)
      (let ((weight (max 0 (aref population-cells index))))
        (when (> weight 0)
          (setq weighted-sum (+ weighted-sum
                                (* (max 0 (min 1000 (aref crime-cells index)))
                                   weight))
                total-weight (+ total-weight weight)))))
    (if (> total-weight 0)
        (elcity-quality-clamp-scalar
         (round weighted-sum total-weight))
      0)))

(defun elcity-quality--smooth-map-passes (source-map passes)
  "Return SOURCE-MAP after PASSES deterministic smoothing iterations."
  (cl-check-type source-map elcity-tile-field)
  (cl-check-type passes integer)
  (when (< passes 0)
    (error "PASSES must be non-negative: %S" passes))
  (let ((next source-map))
    (dotimes (_ passes)
      (setq next (elcity-quality-smooth-map next)))
    next))

(defun elcity-quality-compute-from-sources (sources)
  "Return published S6 snapshot computed from normalized SOURCES bundle."
  (cl-check-type sources elcity-quality-source-bundle)
  (let* ((population-map (elcity-quality-source-bundle-population-map sources))
         (pollution-raw-map (elcity-quality-pollution-raw-map sources))
         (crime-raw-map (elcity-quality-crime-raw-map sources pollution-raw-map))
         (land-value-raw-map (elcity-quality-land-value-raw-map
                              sources
                              pollution-raw-map
                              crime-raw-map))
         (pollution-map (elcity-quality--smooth-map-pollution-passes
                         pollution-raw-map
                         elcity-quality-pollution-smoothing-passes))
         (crime-map (elcity-quality--smooth-map-passes
                     crime-raw-map
                     elcity-quality-smoothing-passes))
         (land-value-map (elcity-quality--smooth-map-passes
                          land-value-raw-map
                          elcity-quality-smoothing-passes)))
    (elcity-quality-snapshot-create
     :pollution-map pollution-map
     :land-value-map land-value-map
     :crime-map crime-map
     :pollution-avg (elcity-quality-map-average pollution-map)
     :land-value-avg (elcity-quality-map-average land-value-map)
     :crime-avg (elcity-quality-crime-population-weighted-average
                 crime-map
                 population-map)
     :fire-coverage-map (elcity-quality-source-bundle-fire-coverage-map sources)
     :police-coverage-map (elcity-quality-source-bundle-police-coverage-map
                           sources))))

(defun elcity-quality-compute (world &optional level-map fire-effectiveness
                                     police-effectiveness power-grid
                                     infra-effectiveness congestion-map)
  "Return published S6 snapshot computed from WORLD and upstream snapshots.
LEVEL-MAP is optional S4 level map.  FIRE-EFFECTIVENESS and
POLICE-EFFECTIVENESS are optional S7 permille values.  POWER-GRID is
optional S2 power grid for station connectivity gating.
INFRA-EFFECTIVENESS scales road pollution inversely (permille, default 1000).
CONGESTION-MAP is optional S3 congestion tile-field for pollution scaling."
  (elcity-quality-compute-from-sources
   (elcity-quality-collect-source-bundle world
                                         level-map
                                         fire-effectiveness
                                         police-effectiveness
                                         power-grid
                                         infra-effectiveness
                                         congestion-map)))

(defun elcity-quality-default-snapshot (world)
  "Return deterministic default S6 snapshot sized for WORLD dimensions.
All field maps and aggregate averages are initialized to 0."
  (cl-check-type world elcity-world-map)
  (let ((width (elcity-world-map-width world))
        (height (elcity-world-map-height world)))
    (elcity-quality-snapshot-create
     :pollution-map (elcity-tile-field-make width
                                             height
                                             0)
     :land-value-map (elcity-tile-field-make width
                                              height
                                              0)
     :crime-map (elcity-tile-field-make width
                                         height
                                         0)
     :pollution-avg 0
     :land-value-avg 0
     :crime-avg 0)))

(defun elcity-quality-local-quality-at (snapshot world x y kind)
  "Return local-quality projection for tile X,Y and zone KIND.
Uses SNAPSHOT for S6 quality field data and WORLD for bounds checking.
Projection contract:
  residential: clamp(land-value - pollution - crime, -1000, 1000)
  commercial: clamp(land-value - crime, -1000, 1000)
  industrial: 0
KIND is validated against supported S4 zone kinds."
  (cl-check-type snapshot elcity-quality-snapshot)
  (cl-check-type world elcity-world-map)
  (cl-check-type x integer)
  (cl-check-type y integer)
  (elcity-zones-check-kind kind)
  (if (not (elcity-world-in-bounds-p world x y))
      (elcity-zones-clamp-local-quality 0)
    (pcase kind
      ('industrial
       (elcity-zones-clamp-local-quality 0))
      ('commercial
       (let* ((land-value-map (elcity-quality-snapshot-land-value-map snapshot))
              (crime-map (elcity-quality-snapshot-crime-map snapshot))
              (land-value (elcity-tile-field-ref land-value-map x y 0))
              (crime (elcity-tile-field-ref crime-map x y 0)))
         (elcity-zones-clamp-local-quality (- land-value crime))))
      ('residential
       (let* ((pollution-map (elcity-quality-snapshot-pollution-map snapshot))
              (land-value-map (elcity-quality-snapshot-land-value-map snapshot))
              (crime-map (elcity-quality-snapshot-crime-map snapshot))
              (pollution (elcity-tile-field-ref pollution-map x y 0))
              (land-value (elcity-tile-field-ref land-value-map x y 0))
              (crime (elcity-tile-field-ref crime-map x y 0)))
         (elcity-zones-clamp-local-quality (- land-value pollution crime)))))))

(provide 'elcity-quality)

;;; elcity-quality.el ends here
