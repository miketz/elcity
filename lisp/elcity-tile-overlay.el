;;; elcity-tile-overlay.el --- Icon overlay composition for tile images -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; Pure XPM string transformation module for compositing status icon
;; overlays into tile images at render time.
;;
;; Four overlay channels:
;; - Power icon: lightning bolt in bottom-right corner, shown on
;;   conductive tiles that are unpowered or disconnected.
;; - Traffic icon: red X in bottom-left corner, shown on zone centers
;;   failing the S3 accessibility gate (blocked or no-road).
;; - Congestion stripe: horizontal center bar on road tiles with
;;   critical (>=200) or blocked (=240) congestion severity.
;; - Burning icon: red/yellow flame motif covering tile center,
;;   shown on tiles that are currently on fire.
;;
;; Public API:
;;   (elcity-tile-overlay-compose-xpm XPM-DATA POWER-STATUS TRAFFIC-STATUS &optional CONGESTION-SEVERITY BURNING-P)
;;   (elcity-tile-overlay-needs-overlay-p POWER-STATUS TRAFFIC-STATUS &optional CONGESTION-SEVERITY BURNING-P)

;;; Code:

(require 'cl-lib)

;;; ---------- Icon palette ----------

(defconst elcity-tile-overlay-power-char ?!
  "XPM palette character for the power icon.")

(defconst elcity-tile-overlay-power-color "#FFE000"
  "Hex color for the power icon (bright yellow).")

(defconst elcity-tile-overlay-traffic-char ?@
  "XPM palette character for the traffic icon.")

(defconst elcity-tile-overlay-traffic-color "#FF0000"
  "Hex color for the traffic icon (bright red).")

(defconst elcity-tile-overlay-congestion-char ?$
  "XPM palette character for the congestion stripe.")

(defconst elcity-tile-overlay-congestion-color-critical "#FF8C00"
  "Hex color for the congestion stripe at critical level (dark orange).")

(defconst elcity-tile-overlay-congestion-color-blocked "#FF0000"
  "Hex color for the congestion stripe at blocked level (bright red).")

(defconst elcity-tile-overlay-burning-char-r ?&
  "XPM palette character for the burning icon red pixels.")

(defconst elcity-tile-overlay-burning-color-r "#FF2000"
  "Hex color for the burning icon red flame pixels.")

(defconst elcity-tile-overlay-burning-char-y ?^
  "XPM palette character for the burning icon yellow pixels.")

(defconst elcity-tile-overlay-burning-color-y "#FFD000"
  "Hex color for the burning icon yellow flame pixels.")

;;; ---------- Icon pixel patterns ----------

;; Each pattern is a list of (DX . DY) offsets relative to the icon anchor.
;; Patterns are 5x5 pixels within the 16x16 tile.

(defconst elcity-tile-overlay-power-anchor '(11 . 11)
  "Top-left anchor position for the power icon (bottom-right corner).")

(defconst elcity-tile-overlay-power-offsets
  '((2 . 0)
    (1 . 1) (2 . 1)
    (2 . 2)
    (2 . 3)
    (2 . 4))
  "Pixel offsets for power icon (lightning bolt shape).")

(defconst elcity-tile-overlay-traffic-anchor '(0 . 11)
  "Top-left anchor position for the traffic icon (bottom-left corner).")

(defconst elcity-tile-overlay-traffic-offsets
  '((0 . 0) (4 . 0)
    (1 . 1) (3 . 1)
    (2 . 2)
    (1 . 3) (3 . 3)
    (0 . 4) (4 . 4))
  "Pixel offsets for traffic icon (X mark shape).")

(defconst elcity-tile-overlay-burning-anchor '(4 . 2)
  "Top-left anchor position for the burning icon (center area).")

(defconst elcity-tile-overlay-burning-offsets-r
  '((3 . 3)
    (2 . 4) (3 . 4) (5 . 4)
    (1 . 5) (2 . 5) (3 . 5) (4 . 5) (5 . 5) (6 . 5)
    (1 . 6) (2 . 6) (5 . 6) (6 . 6)
    (0 . 7) (1 . 7) (2 . 7) (5 . 7) (6 . 7) (7 . 7)
    (0 . 8) (1 . 8) (6 . 8) (7 . 8)
    (0 . 9) (7 . 9)
    (0 . 10) (1 . 10) (6 . 10) (7 . 10)
    (1 . 11) (2 . 11) (5 . 11) (6 . 11))
  "Red pixel offsets for burning icon (flame outline).")

(defconst elcity-tile-overlay-burning-offsets-y
  '((3 . 2) (4 . 2)
    (4 . 3) (5 . 3)
    (4 . 4)
    (3 . 6) (4 . 6)
    (3 . 7) (4 . 7)
    (2 . 8) (3 . 8) (4 . 8) (5 . 8)
    (1 . 9) (2 . 9) (3 . 9) (4 . 9) (5 . 9) (6 . 9)
    (2 . 10) (3 . 10) (4 . 10) (5 . 10)
    (3 . 11) (4 . 11))
  "Yellow pixel offsets for burning icon (flame core).")

(defconst elcity-tile-overlay-congestion-anchor '(5 . 7)
  "Top-left anchor position for the congestion stripe (tile center).")

(defconst elcity-tile-overlay-congestion-offsets
  '((0 . 0) (1 . 0) (2 . 0) (3 . 0) (4 . 0) (5 . 0)
    (0 . 1) (1 . 1) (2 . 1) (3 . 1) (4 . 1) (5 . 1))
  "Pixel offsets for congestion stripe (6x2 horizontal center bar).")

;;; ---------- Status predicates ----------

(defun elcity-tile-overlay-needs-power-icon-p (power-status)
  "Return non-nil when POWER-STATUS requires a power icon overlay.
POWER-STATUS is nil, `unpowered', or `disconnected'."
  (memq power-status '(unpowered disconnected)))

(defun elcity-tile-overlay-needs-traffic-icon-p (traffic-status)
  "Return non-nil when TRAFFIC-STATUS requires a traffic icon overlay.
TRAFFIC-STATUS is nil, `blocked', or `no-road'."
  (and traffic-status (memq traffic-status '(blocked no-road))))

(defun elcity-tile-overlay-needs-congestion-stripe-p (congestion-severity)
  "Return non-nil when CONGESTION-SEVERITY requires a road stripe.
CONGESTION-SEVERITY is nil, `critical', or `blocked'."
  (memq congestion-severity '(critical blocked)))

(defun elcity-tile-overlay-needs-burning-icon-p (burning-p)
  "Return non-nil when BURNING-P requires a burning icon overlay."
  (and burning-p t))

(defun elcity-tile-overlay-needs-overlay-p (power-status traffic-status
                                            &optional congestion-severity
                                            burning-p)
  "Return non-nil when any status needs an overlay.
POWER-STATUS, TRAFFIC-STATUS, CONGESTION-SEVERITY, and BURNING-P are checked."
  (or (elcity-tile-overlay-needs-power-icon-p power-status)
      (elcity-tile-overlay-needs-traffic-icon-p traffic-status)
      (elcity-tile-overlay-needs-congestion-stripe-p congestion-severity)
      (elcity-tile-overlay-needs-burning-icon-p burning-p)))

;;; ---------- XPM composition ----------

(defun elcity-tile-overlay--pixel-set (power-status traffic-status
                                      &optional congestion-severity
                                      burning-p)
  "Return (X Y CHAR) pixel replacements for statuses.
POWER-STATUS, TRAFFIC-STATUS, CONGESTION-SEVERITY, and BURNING-P
control which icons and stripes are included.  Burning pixels are
pushed first, then congestion, then icons.  Last-write-wins
ordering in `--apply-pixels' means icons override burning pixels."
  (let (pixels)
    (when (elcity-tile-overlay-needs-power-icon-p power-status)
      (let ((ax (car elcity-tile-overlay-power-anchor))
            (ay (cdr elcity-tile-overlay-power-anchor)))
        (dolist (off elcity-tile-overlay-power-offsets)
          (push (list (+ ax (car off)) (+ ay (cdr off))
                      elcity-tile-overlay-power-char)
                pixels))))
    (when (elcity-tile-overlay-needs-traffic-icon-p traffic-status)
      (let ((ax (car elcity-tile-overlay-traffic-anchor))
            (ay (cdr elcity-tile-overlay-traffic-anchor)))
        (dolist (off elcity-tile-overlay-traffic-offsets)
          (push (list (+ ax (car off)) (+ ay (cdr off))
                      elcity-tile-overlay-traffic-char)
                pixels))))
    (when (elcity-tile-overlay-needs-congestion-stripe-p congestion-severity)
      (let ((ax (car elcity-tile-overlay-congestion-anchor))
            (ay (cdr elcity-tile-overlay-congestion-anchor)))
        (dolist (off elcity-tile-overlay-congestion-offsets)
          (push (list (+ ax (car off)) (+ ay (cdr off))
                      elcity-tile-overlay-congestion-char)
                pixels))))
    (when (elcity-tile-overlay-needs-burning-icon-p burning-p)
      (let ((ax (car elcity-tile-overlay-burning-anchor))
            (ay (cdr elcity-tile-overlay-burning-anchor)))
        ;; Yellow core first (lower priority), then red outline on top.
        (dolist (off elcity-tile-overlay-burning-offsets-y)
          (push (list (+ ax (car off)) (+ ay (cdr off))
                      elcity-tile-overlay-burning-char-y)
                pixels))
        (dolist (off elcity-tile-overlay-burning-offsets-r)
          (push (list (+ ax (car off)) (+ ay (cdr off))
                      elcity-tile-overlay-burning-char-r)
                pixels))))
    pixels))

(defun elcity-tile-overlay--extra-colors (power-status traffic-status
                                         &optional congestion-severity
                                         burning-p)
  "Return list of XPM color definition strings for overlay statuses.
POWER-STATUS, TRAFFIC-STATUS, CONGESTION-SEVERITY, and BURNING-P
control which palette entries are included."
  (let (colors)
    (when (elcity-tile-overlay-needs-power-icon-p power-status)
      (push (format "\"%c c %s\","
                    elcity-tile-overlay-power-char
                    elcity-tile-overlay-power-color)
            colors))
    (when (elcity-tile-overlay-needs-traffic-icon-p traffic-status)
      (push (format "\"%c c %s\","
                    elcity-tile-overlay-traffic-char
                    elcity-tile-overlay-traffic-color)
            colors))
    (when (elcity-tile-overlay-needs-congestion-stripe-p congestion-severity)
      (push (format "\"%c c %s\","
                    elcity-tile-overlay-congestion-char
                    (if (eq congestion-severity 'blocked)
                        elcity-tile-overlay-congestion-color-blocked
                      elcity-tile-overlay-congestion-color-critical))
            colors))
    (when (elcity-tile-overlay-needs-burning-icon-p burning-p)
      (push (format "\"%c c %s\","
                    elcity-tile-overlay-burning-char-r
                    elcity-tile-overlay-burning-color-r)
            colors)
      (push (format "\"%c c %s\","
                    elcity-tile-overlay-burning-char-y
                    elcity-tile-overlay-burning-color-y)
            colors))
    (nreverse colors)))

(defun elcity-tile-overlay-compose-xpm (xpm-data power-status traffic-status
                                        &optional congestion-severity
                                        burning-p)
  "Return XPM-DATA with status icon overlays baked in.
POWER-STATUS is nil, `unpowered', or `disconnected'.
TRAFFIC-STATUS is nil, `blocked', or `no-road'.
CONGESTION-SEVERITY is nil, `critical', or `blocked'.
BURNING-P is non-nil when the tile is on fire.
When no status requires an overlay, return XPM-DATA unchanged."
  (if (not (elcity-tile-overlay-needs-overlay-p
            power-status traffic-status congestion-severity burning-p))
      xpm-data
    (let* ((pixels (elcity-tile-overlay--pixel-set
                    power-status traffic-status congestion-severity burning-p))
           (extra-colors (elcity-tile-overlay--extra-colors
                          power-status traffic-status congestion-severity
                          burning-p))
           (num-extra (length extra-colors))
           (lines (split-string xpm-data "\n"))
           (result nil)
           (color-count nil)
           (colors-remaining 0)
           (row-y 0)
           (header-re "^\"\\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\) 1\""))
      (dolist (line lines)
        (cond
         ;; Header line: extract color count and bump it
         ((and (not color-count)
               (string-match header-re line))
          (let ((w (match-string 1 line))
                (h (match-string 2 line))
                (n (string-to-number (match-string 3 line))))
            (setq color-count n
                  colors-remaining n)
            (push (format "\"%s %s %d 1\"," w h (+ n num-extra)) result)))
         ;; Color definition lines: count down from header's color count
         ((and color-count (> colors-remaining 0))
          (cl-decf colors-remaining)
          (push line result)
          ;; After last original color, inject extra colors
          (when (= colors-remaining 0)
            (dolist (c extra-colors)
              (push c result))))
         ;; Pixel rows
         ((and color-count (= colors-remaining 0)
               (string-match "^\"" line))
          (push (elcity-tile-overlay--apply-pixels line row-y pixels) result)
          (setq row-y (1+ row-y)))
         ;; Other lines (comments, close brace, etc.)
         (t (push line result))))
      (mapconcat #'identity (nreverse result) "\n"))))

(defun elcity-tile-overlay--apply-pixels (line row-y pixels)
  "Return LINE with overlay PIXELS applied for ROW-Y.
PIXELS is a list of (X Y CHAR) where Y matches row index."
  (let ((row-pixels (cl-remove-if-not (lambda (p) (= (nth 1 p) row-y)) pixels)))
    (if (null row-pixels)
        line
      ;; Extract pixel content between quotes
      (if (string-match "^\"\\([^\"]*\\)\"\\(.*\\)" line)
          (let ((content (match-string 1 line))
                (suffix (match-string 2 line)))
            (dolist (p row-pixels)
              (let ((x (nth 0 p))
                    (ch (nth 2 p)))
                (when (< x (length content))
                  (setq content (concat (substring content 0 x)
                                        (char-to-string ch)
                                        (substring content (1+ x)))))))
            (format "\"%s\"%s" content suffix))
        line))))

(provide 'elcity-tile-overlay)

;;; elcity-tile-overlay.el ends here
