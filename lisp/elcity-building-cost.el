;;; elcity-building-cost.el --- S12 construction economy -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; Construction cost gating for S11 building tools.
;;
;; S12 is a gate between S11 (tile computation) and S10 (tile application).
;; Each placement via S11 is checked against the current treasury and charged
;; a flat cost on success.  Insufficient funds reject the entire placement
;; without side effects.  Raw S10 `tile-update' actions (from scenarios,
;; replay, debug) remain cost-free.
;;
;; Public API:
;;   (elcity-building-cost-for-tool TOOL-KIND FOOTPRINT-SIZE)
;;     -> cost (integer)
;;   (elcity-building-check-cost TOOL-KIND FOOTPRINT-SIZE TREASURY)
;;     -> cost (integer); signals error when funds insufficient
;;   (elcity-building-place TOOL-KIND ANCHOR-X ANCHOR-Y STATE)
;;     -> new state with tiles applied and treasury deducted

;;; Code:

(require 'cl-lib)
(require 'elcity-building)
(require 'elcity-budget)
(require 'elcity-state-api)

;;; Cost table

(defconst elcity-building-cost-table
  '((road            .    10)
    (powerline       .     5)
    (zone-residential .  100)
    (zone-commercial  .  100)
    (zone-industrial  .  100)
    (coal-plant      .  3000)
    (nuclear-plant   .  5000)
    (fire-station    .   300)
    (police-station  .   300)
    (park            .    10)
    (bulldoze        .     1))
  "Flat construction cost per placement.
Bulldoze cost is per tile cleared; all others are per placement.")

;;; Cost lookup

(defun elcity-building-cost-for-tool (tool-kind footprint-size)
  "Return construction cost for TOOL-KIND with FOOTPRINT-SIZE tiles.
For bulldoze, cost scales with FOOTPRINT-SIZE (1 per tile cleared).
For all other tools, cost is a flat lookup regardless of footprint."
  (cl-check-type tool-kind symbol)
  (cl-check-type footprint-size integer)
  (let ((entry (assq tool-kind elcity-building-cost-table)))
    (unless entry
      (error "Unknown building tool kind for cost lookup: %S" tool-kind))
    (let ((base-cost (cdr entry)))
      (if (eq tool-kind 'bulldoze)
          (* base-cost footprint-size)
        base-cost))))

;;; Treasury check

(defun elcity-building-check-cost (tool-kind footprint-size treasury)
  "Validate that TREASURY can afford TOOL-KIND with FOOTPRINT-SIZE.
Return the cost on success.  Signal error on insufficient funds."
  (cl-check-type treasury integer)
  (let ((cost (elcity-building-cost-for-tool tool-kind footprint-size)))
    (when (> cost treasury)
      (error "Insufficient funds: need %d, have %d" cost treasury))
    cost))

;;; Treasury deduction

(defun elcity-building-deduct-cost (state cost)
  "Return STATE with COST deducted from treasury."
  (cl-check-type state elcity-state)
  (cl-check-type cost integer)
  (elcity-state-api-with-treasury state (- (elcity-state-api-treasury state) cost)))

;;; Pipeline coordinator

(defun elcity-building-place (tool-kind anchor-x anchor-y state)
  "Place TOOL-KIND at ANCHOR-X, ANCHOR-Y in STATE with S12 cost check.
Return new state with tiles applied, treasury deducted, and
construction-spent incremented.  Signal error on rejection."
  (cl-check-type state elcity-state)
  (let* ((result (elcity-building-apply tool-kind anchor-x anchor-y state))
         (actions (plist-get result :actions))
         (footprint-size (plist-get result :footprint-size))
         (treasury (elcity-state-api-treasury state))
         (cost (elcity-building-check-cost tool-kind footprint-size treasury))
         (tile-updates (mapcar (lambda (a)
                                 (cons (cons (plist-get a :x) (plist-get a :y))
                                       (plist-get a :tile)))
                               actions))
         (state (if tile-updates
                    (elcity-state-api-apply-tile-updates state tile-updates)
                  state))
         (state (elcity-building-deduct-cost state cost))
         (snapshot (copy-elcity-budget-snapshot
                    (elcity-state-api-budget-snapshot state))))
    (setf (elcity-budget-snapshot-construction-spent snapshot)
          (+ (elcity-budget-snapshot-construction-spent snapshot) cost))
    (elcity-state-api-with-budget-snapshot state snapshot)))

(provide 'elcity-building-cost)

;;; elcity-building-cost.el ends here
