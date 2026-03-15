;;; elcity-overlay.el --- Map overlay heatmap support -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; Pure data module for map overlay heatmaps.
;;
;; Provides a 7-step red-yellow-green gradient palette and XPM border
;; tinting for per-tile heatmap visualization.  Overlay kinds define
;; which simulation field map to read and how to map values to colors.
;;
;; This module has no buffer, rendering, or simulation dependencies.
;; Consumed by `elcity-player-render' and `elcity-player'.
;;
;; Public API:
;;   `elcity-overlay-kinds'           - ordered list of overlay kind symbols
;;   `elcity-overlay-kind-config'     - config plist for a kind
;;   `elcity-overlay-value-to-color'  - map value to gradient hex color
;;   `elcity-overlay-border-xpm'     - apply colored border to XPM data
;;   `elcity-overlay-clear-cache'    - clear the XPM border cache

;;; Code:

(require 'cl-lib)

;;; ---------- Gradient palette ----------

(defconst elcity-overlay-gradient
  ["#44AA44"   ; step 0 — green (best/lowest)
   "#88CC44"   ; step 1 — yellow-green
   "#BBCC44"   ; step 2 — yellow
   "#CCAA44"   ; step 3 — yellow-orange
   "#CC7744"   ; step 4 — orange
   "#CC4444"   ; step 5 — red-orange
   "#AA2222"]  ; step 6 — red (worst/highest)
  "7-step gradient from green (good) to red (bad).
Index 0 is best, index 6 is worst.")

(defconst elcity-overlay-gradient-steps
  (length elcity-overlay-gradient)
  "Number of discrete gradient steps.")

(defun elcity-overlay-value-to-step (value value-max)
  "Map VALUE in [0..VALUE-MAX] to a gradient step index.
Returns an integer in [0..`elcity-overlay-gradient-steps'-1].
VALUE is clamped to [0..VALUE-MAX]."
  (let* ((clamped (max 0 (min value-max value)))
         (steps elcity-overlay-gradient-steps)
         (step (if (zerop value-max)
                   0
                 (min (1- steps)
                      (/ (* clamped (1- steps)) value-max)))))
    step))

(defun elcity-overlay-value-to-color (value value-max direction)
  "Map VALUE in [0..VALUE-MAX] to a gradient hex color string.
DIRECTION is `bad-is-high' or `good-is-high'.
  - `bad-is-high': 0 is green (step 0), VALUE-MAX is red (step 6).
  - `good-is-high': 0 is red (step 6), VALUE-MAX is green (step 0)."
  (let ((step (elcity-overlay-value-to-step value value-max)))
    (aref elcity-overlay-gradient
          (pcase direction
            ('bad-is-high step)
            ('good-is-high (- (1- elcity-overlay-gradient-steps) step))
            (_ (error "Unknown overlay direction: %S" direction))))))

;;; ---------- Overlay kind registry ----------

(defconst elcity-overlay-kind-configs
  '((pollution
     :label "Pollution"
     :value-max 1000
     :direction bad-is-high)
    (crime
     :label "Crime"
     :value-max 1000
     :direction bad-is-high)
    (land-value
     :label "Land Value"
     :value-max 1000
     :direction good-is-high)
    (congestion
     :label "Congestion"
     :value-max 240
     :direction bad-is-high)
    (fire-coverage
     :label "Fire Coverage"
     :value-max 1000
     :direction good-is-high)
    (police-coverage
     :label "Police Coverage"
     :value-max 1000
     :direction good-is-high))
  "Overlay kind configurations.
Each entry is (KIND . PLIST) where PLIST contains:
  `:label'     — player-facing display name,
  `:value-max' — maximum raw value from the field map,
  `:direction' — `bad-is-high' or `good-is-high'.")

(defconst elcity-overlay-kinds
  (mapcar #'car elcity-overlay-kind-configs)
  "Ordered list of overlay kind symbols for cycling.")

(defun elcity-overlay-kind-config (kind)
  "Return config plist for overlay KIND, or nil if unknown."
  (cdr (assq kind elcity-overlay-kind-configs)))

(defun elcity-overlay-kind-label (kind)
  "Return player-facing display label for overlay KIND."
  (plist-get (elcity-overlay-kind-config kind) :label))

(defun elcity-overlay-kind-value-max (kind)
  "Return maximum raw value for overlay KIND."
  (plist-get (elcity-overlay-kind-config kind) :value-max))

(defun elcity-overlay-kind-direction (kind)
  "Return gradient direction for overlay KIND."
  (plist-get (elcity-overlay-kind-config kind) :direction))

;;; ---------- XPM border rendering ----------

(defconst elcity-overlay-border-width 2
  "Border width in pixels for overlay heatmap tinting.")

(defconst elcity-overlay-border-char ?O
  "XPM palette character used for the overlay border color.")

(defun elcity-overlay-border-xpm (xpm-data color &optional border-width)
  "Return XPM-DATA with a colored border drawn into it.
COLOR is a hex color string (e.g. \"#CC4444\").
Optional BORDER-WIDTH overrides `elcity-overlay-border-width'.
Adds a color entry for `elcity-overlay-border-char' and replaces
border pixels.  Tile pixel size is parsed from the XPM header."
  (let* ((bw (or border-width elcity-overlay-border-width))
         (lines (split-string xpm-data "\n"))
         (header-re "^\"\\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\) 1\"")
         (tile-size nil)
         (result nil)
         (in-pixels nil)
         (row-count 0)
         (color-added nil))
    (dolist (line lines)
      (cond
       ;; Header: parse tile size, bump color count
       ((and (not tile-size)
             (string-match header-re line))
        (setq tile-size (string-to-number (match-string 1 line)))
        (let ((h (match-string 2 line))
              (n (string-to-number (match-string 3 line))))
          (push (format "\"%d %s %d 1\"," tile-size h (1+ n)) result)))
       ;; First pixel row: insert color def before it
       ((and tile-size (not color-added)
             (not in-pixels)
             (string-match (format "^\"\\(.\\{%d\\}\\)\"" tile-size) line))
        (push (format "\"%c c %s\"," elcity-overlay-border-char color)
              result)
        (setq color-added t
              in-pixels t)
        (push (elcity-overlay--border-row line row-count tile-size bw)
              result)
        (setq row-count (1+ row-count)))
       ;; Subsequent pixel rows
       (in-pixels
        (if (string-match (format "^\"\\(.\\{%d\\}\\)\"" tile-size) line)
            (progn
              (push (elcity-overlay--border-row line row-count tile-size bw)
                    result)
              (setq row-count (1+ row-count)))
          (push line result)))
       (t (push line result))))
    (mapconcat #'identity (nreverse result) "\n")))

(defun elcity-overlay--border-row (line row-index tile-size border-width)
  "Return LINE with border pixels replaced for ROW-INDEX.
TILE-SIZE is the pixel dimension.  BORDER-WIDTH is the border thickness.
Top and bottom BORDER-WIDTH rows get all pixels replaced; middle rows
get only the leftmost and rightmost BORDER-WIDTH pixels replaced."
  (let ((pixel-re (format "^\"\\(.\\{%d\\}\\)\"\\(.*\\)" tile-size)))
    (if (string-match pixel-re line)
        (let ((pixels (match-string 1 line))
              (suffix (match-string 2 line)))
          (if (or (< row-index border-width)
                  (>= row-index (- tile-size border-width)))
              ;; Full border row
              (format "\"%s\"%s"
                      (make-string tile-size elcity-overlay-border-char)
                      suffix)
            ;; Middle row: replace left and right edges
            (let ((left (make-string border-width elcity-overlay-border-char))
                  (interior (substring pixels border-width
                                       (- tile-size border-width)))
                  (right (make-string border-width elcity-overlay-border-char)))
              (format "\"%s%s%s\"%s" left interior right suffix))))
      line)))

;;; ---------- XPM border cache ----------

(defvar elcity-overlay--border-cache (make-hash-table :test #'equal)
  "Cache mapping (XPM-DATA . COLOR-HEX) to bordered XPM data string.
Stores data strings, not image descriptors.")

(defun elcity-overlay-border-xpm-cached (xpm-data color)
  "Return bordered XPM for XPM-DATA with COLOR, using cache.
Always uses `elcity-overlay-border-width'.  Cache key is (XPM-DATA . COLOR)."
  (let ((key (cons xpm-data color)))
    (or (gethash key elcity-overlay--border-cache)
        (let ((result (elcity-overlay-border-xpm xpm-data color)))
          (puthash key result elcity-overlay--border-cache)
          result))))

(defun elcity-overlay-clear-cache ()
  "Clear the overlay XPM border cache."
  (clrhash elcity-overlay--border-cache))

(provide 'elcity-overlay)

;;; elcity-overlay.el ends here
