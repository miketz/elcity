;;; elcity-terrain.el --- Terrain generation helpers -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; Pure terrain generation helpers for S1 world initialization.
;;
;; This module owns:
;; - quarter-circle water body generation for new-game maps,
;; - water-proximity distance computation for S6 land-value input,
;; - terrain generation constants (fraction target, tolerance, shoreline params).
;;
;; Water is placed as a quarter-circle in one map corner.  The corner is
;; chosen deterministically from the seed.  Target water fraction is ~10%
;; of total tiles, with a configurable tolerance band.

;;; Code:

(require 'cl-lib)
(require 'elcity-world)

;;; Constants — terrain generation

(defconst elcity-terrain-water-fraction-target 10
  "Target water fraction as integer percent of total tiles.")

(defconst elcity-terrain-water-fraction-tolerance 3
  "Tolerance band in percent points around target fraction.
Acceptable water fraction is TARGET +/- TOLERANCE, i.e. 7%–13%.")

(defconst elcity-terrain-water-tile 1
  "Tile index used for water cells in generated terrain.
Must match `elcity-building-water-index'.")

;;; Constants — shoreline land-value bonus

(defconst elcity-terrain-shoreline-bonus-max 300
  "Maximum shoreline land-value bonus (S6 scalar units, [0,1000]).")

(defconst elcity-terrain-shoreline-falloff-radius 8
  "Manhattan distance at which shoreline bonus decays to zero.")

;;; Terrain generation

(defun elcity-terrain-quarter-circle-radius (width height fraction-percent)
  "Return quarter-circle radius for target FRACTION-PERCENT of WIDTH x HEIGHT.
The radius is chosen so that a quarter-circle occupying one corner covers
approximately FRACTION-PERCENT of the total tile area."
  (cl-check-type width integer)
  (cl-check-type height integer)
  (cl-check-type fraction-percent integer)
  (let* ((total (* width height))
         (target-area (* total fraction-percent))
         ;; Quarter-circle area = pi*r^2/4
         ;; target-area / 100 = pi*r^2/4
         ;; r = sqrt(4 * target-area / (100 * pi))
         (r-squared (/ (* 4.0 target-area) (* 100.0 float-pi))))
    (round (sqrt r-squared))))

(defun elcity-terrain-corner-for-seed (seed)
  "Return corner index 0..3 for deterministic SEED.
Corners: 0=top-left, 1=top-right, 2=bottom-left, 3=bottom-right."
  (cl-check-type seed integer)
  (mod (abs seed) 4))

(defun elcity-terrain-stamp-water (world seed)
  "Return WORLD with quarter-circle water body stamped in one corner.
Corner is chosen deterministically from SEED.  Water fraction targets
`elcity-terrain-water-fraction-target'."
  (cl-check-type world elcity-world-map)
  (cl-check-type seed integer)
  (let* ((width (elcity-world-map-width world))
         (height (elcity-world-map-height world))
         (radius (elcity-terrain-quarter-circle-radius
                  width height elcity-terrain-water-fraction-target))
         (corner (elcity-terrain-corner-for-seed seed))
         (r2 (* radius radius))
         (tiles (copy-sequence (elcity-world-map-tiles world)))
         (cx (if (memq corner '(0 2)) 0 (1- width)))
         (cy (if (memq corner '(0 1)) 0 (1- height))))
    (dotimes (y height)
      (dotimes (x width)
        (let* ((dx (- x cx))
               (dy (- y cy))
               (dist2 (+ (* dx dx) (* dy dy))))
          (when (<= dist2 r2)
            (aset tiles (+ x (* y width)) elcity-terrain-water-tile)))))
    (elcity-world-map-create :width width
                             :height height
                             :tiles tiles)))

;;; Water-proximity distance map

(defun elcity-terrain-water-distance-map (world)
  "Return Manhattan distance map from each tile to nearest water in WORLD.
Non-water tiles get their Manhattan distance to nearest water tile.
Water tiles get distance 0.  Tiles with no reachable water within
`elcity-terrain-shoreline-falloff-radius' get the falloff radius value.
Returns a vector of integers with same dimensions as WORLD tiles."
  (cl-check-type world elcity-world-map)
  (let* ((width (elcity-world-map-width world))
         (height (elcity-world-map-height world))
         (tiles (elcity-world-map-tiles world))
         (total (* width height))
         (max-dist elcity-terrain-shoreline-falloff-radius)
         (dist (make-vector total max-dist))
         queue)
    ;; Initialize: water tiles get distance 0 and are BFS seeds.
    (dotimes (i total)
      (when (= (logand (aref tiles i) #x03ff) elcity-terrain-water-tile)
        (aset dist i 0)
        (push i queue)))
    ;; Multi-source BFS with Manhattan distance.
    (setq queue (nreverse queue))
    (while queue
      (let* ((idx (pop queue))
             (d (aref dist idx))
             (x (mod idx width))
             (y (/ idx width))
             (nd (1+ d)))
        (when (< nd max-dist)
          (dolist (neighbor (list (when (> x 0) (1- idx))
                                 (when (< x (1- width)) (1+ idx))
                                 (when (> y 0) (- idx width))
                                 (when (< y (1- height)) (+ idx width))))
            (when (and neighbor (> (aref dist neighbor) nd))
              (aset dist neighbor nd)
              (push neighbor queue))))))
    dist))

(defun elcity-terrain-shoreline-bonus-map (world)
  "Return bonus vector for water proximity in WORLD.
Each cell contains a land-value bonus in
[0, `elcity-terrain-shoreline-bonus-max'].
Bonus decays linearly from max at distance 1 to 0 at falloff
radius.  Water tiles (distance 0) get bonus 0 (non-developable)."
  (cl-check-type world elcity-world-map)
  (let* ((dist (elcity-terrain-water-distance-map world))
         (total (length dist))
         (bonus (make-vector total 0))
         (max-bonus elcity-terrain-shoreline-bonus-max)
         (radius elcity-terrain-shoreline-falloff-radius))
    (dotimes (i total)
      (let ((d (aref dist i)))
        (when (and (> d 0) (< d radius))
          ;; Linear decay: bonus = max * (radius - d) / (radius - 1)
          (aset bonus i (round (* max-bonus
                                  (/ (float (- radius d))
                                     (1- radius))))))))
    bonus))

(provide 'elcity-terrain)

;;; elcity-terrain.el ends here
