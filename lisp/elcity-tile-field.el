;;; elcity-tile-field.el --- Tile-resolution derived field container -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; Tile-resolution field container for derived simulation data.
;;
;; A tile-field stores one integer value per world tile in a flat row-major
;; vector.  It shares the same coordinate space as the world map but holds
;; derived data (pollution, congestion, land-value, etc.) rather than
;; authoritative tile state.
;;
;; This module provides creation, access, and batch-update primitives
;; consumed by S3, S6, and downstream field systems.

;;; Code:

(require 'cl-lib)

(cl-defstruct (elcity-tile-field
               (:constructor elcity-tile-field-create))
  "Tile-resolution derived field with flat row-major storage.
WIDTH and HEIGHT match world tile dimensions.  CELLS is indexed by
\(+ X (* Y WIDTH))."
  (width 0 :type integer)
  (height 0 :type integer)
  (cells [] :type vector))

(defun elcity-tile-field-make (width height &optional initial-value)
  "Return a new tile field with WIDTH and HEIGHT.
INITIAL-VALUE fills all cells and defaults to 0."
  (cl-check-type width integer)
  (cl-check-type height integer)
  (when (or (<= width 0) (<= height 0))
    (error "WIDTH and HEIGHT must be positive"))
  (elcity-tile-field-create
   :width width
   :height height
   :cells (make-vector (* width height) (or initial-value 0))))

(defun elcity-tile-field-in-bounds-p (field x y)
  "Return non-nil when X,Y are valid coordinates in FIELD."
  (cl-check-type field elcity-tile-field)
  (cl-check-type x integer)
  (cl-check-type y integer)
  (and (<= 0 x)
       (< x (elcity-tile-field-width field))
       (<= 0 y)
       (< y (elcity-tile-field-height field))))

(defun elcity-tile-field--cell-index (field x y)
  "Return linear index in FIELD for X,Y, or nil when out of bounds."
  (when (elcity-tile-field-in-bounds-p field x y)
    (+ x (* y (elcity-tile-field-width field)))))

(defun elcity-tile-field-ref (field x y &optional default)
  "Return value at X,Y in FIELD, or DEFAULT when out of bounds."
  (cl-check-type field elcity-tile-field)
  (let ((index (elcity-tile-field--cell-index field x y)))
    (if index
        (aref (elcity-tile-field-cells field) index)
      default)))

(defun elcity-tile-field-set (field x y value)
  "Return new FIELD with VALUE at X,Y.
When out of bounds, return FIELD unchanged."
  (cl-check-type field elcity-tile-field)
  (let ((index (elcity-tile-field--cell-index field x y)))
    (if (not index)
        field
      (let* ((cells (copy-sequence (elcity-tile-field-cells field)))
             (next (copy-elcity-tile-field field)))
        (aset cells index value)
        (setf (elcity-tile-field-cells next) cells)
        next))))

(provide 'elcity-tile-field)

;;; elcity-tile-field.el ends here
