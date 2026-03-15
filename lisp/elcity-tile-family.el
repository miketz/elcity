;;; elcity-tile-family.el --- Multi-tile building family registry -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; Canonical registry for multi-tile building families.
;;
;; Each family occupies a contiguous row-major index range:
;;   index = start + dy * width + dx
;;
;; The center (anchor) tile is derived from footprint metadata:
;;   center = start + anchor-dy * width + anchor-dx
;;
;; Public API:
;;   (elcity-tile-family-get FAMILY-ID)             -> family plist or nil
;;   (elcity-tile-family-center-index FAMILY-ID)    -> integer
;;   (elcity-tile-family-position-index FAMILY-ID DX DY) -> integer
;;   (elcity-tile-family-local-coord FAMILY-ID INDEX)    -> (DX . DY) or nil
;;   (elcity-tile-family-size FAMILY-ID)             -> (WIDTH . HEIGHT)
;;   (elcity-tile-family-start-index FAMILY-ID)      -> integer
;;   (elcity-tile-family-for-index INDEX)            -> FAMILY-ID or nil
;;   (elcity-tile-family-member-p FAMILY-ID INDEX)   -> non-nil or nil
;;   (elcity-tile-family-ids)                        -> list of family-id symbols

;;; Code:

(require 'cl-lib)

;;; Family table

(defconst elcity-tile-family--table
  '((coal-plant     . (:start 850 :width 3 :height 3 :anchor-dx 1 :anchor-dy 1))
    (nuclear-plant  . (:start 860 :width 4 :height 4 :anchor-dx 2 :anchor-dy 2))
    (fire-station   . (:start 878 :width 3 :height 3 :anchor-dx 1 :anchor-dy 1))
    (police-station . (:start 888 :width 3 :height 3 :anchor-dx 1 :anchor-dy 1))
    (park           . (:start 844 :width 1 :height 1 :anchor-dx 0 :anchor-dy 0)))
  "Declarative family metadata.
Each entry is (FAMILY-ID . PLIST) where PLIST has:
  :start      - first tile index in the contiguous range
  :width      - footprint width in tiles
  :height     - footprint height in tiles
  :anchor-dx  - anchor column offset within footprint
  :anchor-dy  - anchor row offset within footprint")

;;; Lookup cache (built at load time)

(defconst elcity-tile-family--index-lookup
  (let ((tbl (make-hash-table :test 'eql)))
    (dolist (entry elcity-tile-family--table)
      (let* ((fam-id (car entry))
             (props (cdr entry))
             (start (plist-get props :start))
             (w (plist-get props :width))
             (h (plist-get props :height))
             (count (* w h)))
        (dotimes (i count)
          (puthash (+ start i) fam-id tbl))))
    tbl)
  "Hash table mapping tile index -> family-id for O(1) reverse lookup.")

;;; Public API

(defun elcity-tile-family-get (family-id)
  "Return the property list for FAMILY-ID, or nil if not registered."
  (cdr (assq family-id elcity-tile-family--table)))

(defun elcity-tile-family-ids ()
  "Return list of all registered family-id symbols."
  (mapcar #'car elcity-tile-family--table))

(defun elcity-tile-family-start-index (family-id)
  "Return the start tile index for FAMILY-ID."
  (let ((props (elcity-tile-family-get family-id)))
    (unless props (error "Unknown family: %S" family-id))
    (plist-get props :start)))

(defun elcity-tile-family-size (family-id)
  "Return (WIDTH . HEIGHT) for FAMILY-ID."
  (let ((props (elcity-tile-family-get family-id)))
    (unless props (error "Unknown family: %S" family-id))
    (cons (plist-get props :width) (plist-get props :height))))

(defun elcity-tile-family-center-index (family-id)
  "Return the center (anchor) tile index for FAMILY-ID."
  (let ((props (elcity-tile-family-get family-id)))
    (unless props (error "Unknown family: %S" family-id))
    (let ((start (plist-get props :start))
          (w (plist-get props :width))
          (adx (plist-get props :anchor-dx))
          (ady (plist-get props :anchor-dy)))
      (+ start (* ady w) adx))))

(defun elcity-tile-family-position-index (family-id dx dy)
  "Return tile index for FAMILY-ID at footprint offset DX, DY.
DX and DY are 0-based from the top-left corner."
  (let ((props (elcity-tile-family-get family-id)))
    (unless props (error "Unknown family: %S" family-id))
    (let ((start (plist-get props :start))
          (w (plist-get props :width))
          (h (plist-get props :height)))
      (when (or (< dx 0) (>= dx w) (< dy 0) (>= dy h))
        (error "Position (%d,%d) out of bounds for %S (%dx%d)"
               dx dy family-id w h))
      (+ start (* dy w) dx))))

(defun elcity-tile-family-local-coord (family-id index)
  "Return (DX . DY) within FAMILY-ID for tile INDEX, or nil."
  (let ((props (elcity-tile-family-get family-id)))
    (unless props (error "Unknown family: %S" family-id))
    (let* ((start (plist-get props :start))
           (w (plist-get props :width))
           (h (plist-get props :height))
           (offset (- index start)))
      (when (and (>= offset 0) (< offset (* w h)))
        (cons (% offset w) (/ offset w))))))

(defun elcity-tile-family-member-p (family-id index)
  "Return non-nil when INDEX belongs to FAMILY-ID's contiguous range."
  (let ((props (elcity-tile-family-get family-id)))
    (when props
      (let ((start (plist-get props :start))
            (count (* (plist-get props :width)
                      (plist-get props :height))))
        (and (>= index start)
             (< index (+ start count)))))))

(defun elcity-tile-family-for-index (index)
  "Return the family-id that owns tile INDEX, or nil."
  (gethash index elcity-tile-family--index-lookup))

(provide 'elcity-tile-family)

;;; elcity-tile-family.el ends here
