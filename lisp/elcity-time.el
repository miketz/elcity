;;; elcity-time.el --- Simulation calendar formatting -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; Calendar mapping from simulation ticks to human-readable time.
;;
;; Calibration (days-per-month):
;;   30 ticks = 1 month (1 tick = 1 day)
;;  360 ticks = 1 year (12 months)
;;
;; Game starts at "Jan Year 1" (tick 0).
;;
;; Public API:
;;   (elcity-time-format TICK) -> "Jan Year 1"

;;; Code:

(defconst elcity-time-ticks-per-month 30
  "Number of simulation ticks per calendar month (1 tick = 1 day).")

(defconst elcity-time-ticks-per-year (* elcity-time-ticks-per-month 12)
  "Number of simulation ticks per calendar year (360).")

(defconst elcity-time-month-names
  ["Jan" "Feb" "Mar" "Apr" "May" "Jun"
   "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"]
  "Abbreviated month names indexed 0..11.")

(defun elcity-time-format (tick)
  "Return calendar string for simulation TICK.
Format is \"Mon Year N\", e.g. \"Jan Year 1\" for tick 0."
  (let* ((month-index (mod (/ tick elcity-time-ticks-per-month) 12))
         (year (1+ (/ tick elcity-time-ticks-per-year)))
         (month-name (aref elcity-time-month-names month-index)))
    (format "%s Year %d" month-name year)))

(provide 'elcity-time)

;;; elcity-time.el ends here
