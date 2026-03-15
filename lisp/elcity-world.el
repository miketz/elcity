;;; elcity-world.el --- S1 world map container and coordinate accessors -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; Authoritative S1 world map container.
;;
;; The world map stores full tile-resolution data (width, height, flat vector)
;; and is the source of truth for map state.  Derived tile fields are computed
;; from this data by downstream S2/S3/S6 collectors.
;;
;; Access/update helpers are pure and deterministic, with explicit behavior for
;; out-of-bounds coordinates.

;;; Code:

(require 'cl-lib)

(defconst elcity-world-default-width 120
  "Default world-map width in tiles.")

(defconst elcity-world-default-height 100
  "Default world-map height in tiles.")

(cl-defstruct (elcity-world-map
               (:constructor elcity-world-map-create))
  "Authoritative world tile map.
WIDTH and HEIGHT are tile dimensions.  TILES is a row-major flat vector where
the tile at X,Y is stored at index (+ X (* Y WIDTH))."
  (width 0 :type integer)
  (height 0 :type integer)
  (tiles [] :type vector))

(defun elcity-world-map-make (width height &optional initial-tile)
  "Return a new world map with WIDTH and HEIGHT.
INITIAL-TILE fills all tiles and defaults to 0.
Signals an error when WIDTH or HEIGHT are non-positive."
  (cl-check-type width integer)
  (cl-check-type height integer)
  (when (or (<= width 0)
            (<= height 0))
    (error "WIDTH and HEIGHT must be positive"))
  (let ((tile (or initial-tile 0)))
    (cl-check-type tile integer)
    (elcity-world-map-create
     :width width
     :height height
     :tiles (make-vector (* width height) tile))))

(defun elcity-world-in-bounds-p (world x y)
  "Return non-nil when X,Y are valid tile coordinates in WORLD.
Bounds are inclusive on the lower edge and exclusive on the upper edge:
0 <= X < WIDTH and 0 <= Y < HEIGHT."
  (cl-check-type world elcity-world-map)
  (cl-check-type x integer)
  (cl-check-type y integer)
  (and (<= 0 x)
       (< x (elcity-world-map-width world))
       (<= 0 y)
       (< y (elcity-world-map-height world))))

(defun elcity-world--tile-index (world x y)
  "Return linear tile index in WORLD for X,Y or nil when out of bounds."
  (when (elcity-world-in-bounds-p world x y)
    ;; Flatten 2D tile coordinates into row-major vector storage.
    (+ x
       (* y (elcity-world-map-width world)))))

(defun elcity-world-tile-at (world x y &optional default)
  "Return tile at X,Y in WORLD, or DEFAULT when out of bounds.
When DEFAULT is nil and X,Y are out of bounds, return nil."
  (cl-check-type world elcity-world-map)
  (cl-check-type x integer)
  (cl-check-type y integer)
  (let ((index (elcity-world--tile-index world x y)))
    (if index
        (aref (elcity-world-map-tiles world) index)
      default)))

(defun elcity-world-with-tiles (world tile-entries)
  "Return a new WORLD with every tile in TILE-ENTRIES written at once.
Each element has the form ((X . Y) . TILE).  The tiles vector is
copied once; all in-bounds elements are applied in list order.
Out-of-bounds elements are silently skipped.  When no element is
in-bounds, return WORLD unchanged (same object identity)."
  (cl-check-type world elcity-world-map)
  (cl-check-type tile-entries list)
  (let ((tiles (elcity-world-map-tiles world))
        (applied nil))
    (dolist (entry tile-entries)
      (let* ((coord (car entry))
             (index (elcity-world--tile-index world (car coord) (cdr coord))))
        (when index
          (unless applied
            (setq tiles (copy-sequence tiles))
            (setq applied t))
          (aset tiles index (cdr entry)))))
    (if (not applied)
        world
      (let ((next (copy-elcity-world-map world)))
        (setf (elcity-world-map-tiles next) tiles)
        next))))

(defun elcity-world-with-tile (world x y tile)
  "Return a new WORLD with TILE written at X,Y.
When X,Y are out of bounds, return WORLD unchanged (same object identity).
When X,Y are in bounds, return a copied world with only that tile changed."
  (elcity-world-with-tiles world (list (cons (cons x y) tile))))

(provide 'elcity-world)

;;; elcity-world.el ends here
