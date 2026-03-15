;;; elcity-util.el --- Shared utility functions -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; Generic utility functions shared across ElCity subsystems.

;;; Code:

(require 'cl-lib)

(defun elcity-util-clamp (value minimum maximum)
  "Return VALUE clamped to inclusive MINIMUM..MAXIMUM range."
  (cl-check-type value integer)
  (cl-check-type minimum integer)
  (cl-check-type maximum integer)
  (max minimum
       (min maximum value)))

(provide 'elcity-util)

;;; elcity-util.el ends here
