;;; elcity-tile.el --- S1 tile encoding and neighborhood helpers -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; Tile-level S1 primitives.
;;
;; A tile is an integer with:
;; - index payload bits (what base tile it is),
;; - status flag bits (powered/conductive/flammable/bulldozable/zone center).
;;
;; This module provides pure bit helpers and cardinal neighbor utilities only.
;; It does not own world storage.

;;; Code:

(require 'cl-lib)

(defconst elcity-tile-index-mask #x03ff
  "Bit mask for the low 10-bit tile index payload.")

(defconst elcity-tile-pwrbit #x8000
  "Bit flag indicating that a tile is currently powered.")

(defconst elcity-tile-condbit #x4000
  "Bit flag indicating that a tile conducts electricity.")

(defconst elcity-tile-burnbit #x2000
  "Bit flag indicating that a tile is flammable.")

(defconst elcity-tile-bullbit #x1000
  "Bit flag indicating that a tile is bulldozable.")

(defconst elcity-tile-zonebit #x0400
  "Bit flag indicating that a tile is a zone center.")

(defconst elcity-tile-status-mask
  (logior elcity-tile-pwrbit
          elcity-tile-condbit
          elcity-tile-burnbit
          elcity-tile-bullbit
          elcity-tile-zonebit)
  "Bit mask for all tile status flags.")

(defconst elcity-tile-residential-index-min 240
  "Lowest tile index classified as a residential zone center kind.")

(defconst elcity-tile-commercial-index-min 423
  "Lowest tile index classified as a commercial zone center kind.")

(defconst elcity-tile-industrial-index-min 612
  "Lowest tile index classified as an industrial zone center kind.")

(defconst elcity-tile-industrial-index-max 692
  "Highest tile index classified as an industrial zone center kind.")

(defconst elcity-cardinal-neighbor-deltas
  '((0 . -1) (1 . 0) (0 . 1) (-1 . 0))
  "Cardinal neighbor deltas in stable N-E-S-W order.")

(defun elcity--tile-check-flag (flag)
  "Signal an error when FLAG is not one known single status bit."
  (cl-check-type flag integer)
  (unless (and (/= 0 flag)
               (= flag (logand flag elcity-tile-status-mask)))
    (signal 'wrong-type-argument (list 'elcity-tile-flag flag))))

(defun elcity-tile-index (tile)
  "Return the low index payload bits from TILE."
  (cl-check-type tile integer)
  (logand tile elcity-tile-index-mask))

(defun elcity-tile-status-bits (tile)
  "Return only status-flag bits from TILE (no index payload bits)."
  (cl-check-type tile integer)
  (logand tile elcity-tile-status-mask))

(defun elcity-tile-with-index (tile index)
  "Return TILE with low index bits replaced by INDEX.
INDEX must be in [0, `elcity-tile-index-mask`]."
  (cl-check-type tile integer)
  (cl-check-type index integer)
  (when (or (< index 0)
            (> index elcity-tile-index-mask))
    (signal 'args-out-of-range
            (list 'index index 0 elcity-tile-index-mask)))
  (logior (elcity-tile-status-bits tile)
          index))

(defun elcity-tile-flag-set-p (tile flag)
  "Return non-nil when FLAG is set in TILE.
FLAG must be one known status bit."
  (cl-check-type tile integer)
  (elcity--tile-check-flag flag)
  (/= 0 (logand tile flag)))

(defun elcity-tile-with-flag (tile flag enabled)
  "Return TILE with FLAG set when ENABLED is non-nil, else cleared.
FLAG must be one known status bit."
  (cl-check-type tile integer)
  (elcity--tile-check-flag flag)
  (if enabled
      (logior tile flag)
    (logand tile (lognot flag))))

(defun elcity-tile-powered-p (tile)
  "Return non-nil when TILE has `elcity-tile-pwrbit`."
  (elcity-tile-flag-set-p tile elcity-tile-pwrbit))

(defun elcity-tile-with-powered (tile enabled)
  "Return TILE with `elcity-tile-pwrbit` toggled by ENABLED."
  (elcity-tile-with-flag tile elcity-tile-pwrbit enabled))

(defun elcity-tile-conductive-p (tile)
  "Return non-nil when TILE has `elcity-tile-condbit`."
  (elcity-tile-flag-set-p tile elcity-tile-condbit))

(defun elcity-tile-with-conductive (tile enabled)
  "Return TILE with `elcity-tile-condbit` toggled by ENABLED."
  (elcity-tile-with-flag tile elcity-tile-condbit enabled))

(defun elcity-tile-flammable-p (tile)
  "Return non-nil when TILE has `elcity-tile-burnbit`."
  (elcity-tile-flag-set-p tile elcity-tile-burnbit))

(defun elcity-tile-with-flammable (tile enabled)
  "Return TILE with `elcity-tile-burnbit` toggled by ENABLED."
  (elcity-tile-with-flag tile elcity-tile-burnbit enabled))

(defun elcity-tile-bulldozable-p (tile)
  "Return non-nil when TILE has `elcity-tile-bullbit`."
  (elcity-tile-flag-set-p tile elcity-tile-bullbit))

(defun elcity-tile-with-bulldozable (tile enabled)
  "Return TILE with `elcity-tile-bullbit` toggled by ENABLED."
  (elcity-tile-with-flag tile elcity-tile-bullbit enabled))

(defun elcity-tile-zone-center-p (tile)
  "Return non-nil when TILE has `elcity-tile-zonebit`."
  (elcity-tile-flag-set-p tile elcity-tile-zonebit))

(defun elcity-tile-with-zone-center (tile enabled)
  "Return TILE with `elcity-tile-zonebit` toggled by ENABLED."
  (elcity-tile-with-flag tile elcity-tile-zonebit enabled))

(defun elcity-cardinal-neighbors (x y &optional width height)
  "Return cardinal neighbor coordinates around X and Y.
Neighbors are returned as cons cells in N-E-S-W order.  If WIDTH and HEIGHT
are provided, out-of-bounds neighbors are filtered out using [0, WIDTH) and
[0, HEIGHT) bounds.
Diagonal neighbors are intentionally excluded."
  (cl-check-type x integer)
  (cl-check-type y integer)
  (when (xor (null width)
             (null height))
    (error "WIDTH and HEIGHT must both be provided, or both be nil"))
  (when width
    (cl-check-type width integer)
    (cl-check-type height integer)
    (when (or (<= width 0)
              (<= height 0))
      (error "WIDTH and HEIGHT must be positive")))
  (let ((neighbors (mapcar (lambda (delta)
                             (cons (+ x (car delta))
                                   (+ y (cdr delta))))
                           elcity-cardinal-neighbor-deltas)))
    (if (null width)
        neighbors
      (cl-remove-if-not (lambda (coord)
                          (and (<= 0 (car coord))
                               (< (car coord) width)
                               (<= 0 (cdr coord))
                               (< (cdr coord) height)))
                        neighbors))))

(provide 'elcity-tile)

;;; elcity-tile.el ends here
