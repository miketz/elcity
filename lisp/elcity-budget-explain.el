;;; elcity-budget-explain.el --- Budget explainability payload model -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; Pure data module for budget explainability.
;;
;; Provides structured breakdown records for income, spend, and treasury,
;; plus a builder that derives all fields from published map/state accessors.
;;
;; This module is payload-only and must not depend on player/debug modules
;; or own string rendering policy.
;;
;; Design note:
;; - Previous treasury is intentionally not reconstructed or shown.
;;   Explainability reports current published flow only.
;; - Construction spending (S12) is intentionally excluded from this payload,
;;   since it is direct user-initiated action rather than opaque simulation
;;   spending.
;;
;; Public API:
;;   (elcity-budget-explain-at state) -> elcity-budget-explanation

;;; Code:

(require 'cl-lib)
(require 'elcity-budget)
(require 'elcity-power)
(require 'elcity-state-api)
(require 'elcity-state)
(require 'elcity-zones)

;;; ---------- Data contracts ----------

(cl-defstruct (elcity-budget-income-explanation
               (:constructor elcity-budget-income-explanation--create)
               (:copier nil))
  "Structured breakdown of S7 tax income derivation.
GROSS-TAX-INCOME is pre-collectability tax income.
COLLECTABILITY is the permille ratio applied to gross tax.
TAX-INCOME is the collected amount after collectability."
  (tax-rate 0 :type integer)
  (residential-pop 0 :type integer)
  (commercial-pop 0 :type integer)
  (industrial-pop 0 :type integer)
  (taxable-pop 0 :type integer)
  (income-per-capita 0 :type integer)
  (gross-tax-income 0 :type integer)
  (collectability 1000 :type integer)
  (tax-income 0 :type integer))

(cl-defstruct (elcity-budget-spend-explanation
               (:constructor elcity-budget-spend-explanation--create)
               (:copier nil))
  "Structured breakdown of S7 service spending.
Requested fields are absolute demand from city state.
Percent fields are funding policy (0..100).
Target fields are demand * percent / 100.
Spent fields are allocated amounts.
Effectiveness fields are permille values (0..1000).
COAL-COUNT and NUCLEAR-COUNT are power plant counts contributing
to infrastructure demand via operational costs.
POWER-CAPACITY-PERMILLE is the S2 funding-scaled capacity ratio."
  (coal-count 0 :type integer)
  (nuclear-count 0 :type integer)
  (power-capacity-permille 1000 :type integer)
  (infrastructure-requested 0 :type integer)
  (infrastructure-percent 0 :type integer)
  (infrastructure-target 0 :type integer)
  (infrastructure-spent 0 :type integer)
  (infrastructure-eff 0 :type integer)
  (fire-requested 0 :type integer)
  (fire-percent 0 :type integer)
  (fire-target 0 :type integer)
  (fire-spent 0 :type integer)
  (fire-eff 0 :type integer)
  (police-requested 0 :type integer)
  (police-percent 0 :type integer)
  (police-target 0 :type integer)
  (police-spent 0 :type integer)
  (police-eff 0 :type integer)
  (total-requested 0 :type integer)
  (total-target 0 :type integer)
  (total-spent 0 :type integer))

(cl-defstruct
    (elcity-budget-treasury-explanation
     (:constructor elcity-budget-treasury-explanation--create)
     (:copier nil))
  "Structured breakdown of current treasury and per-tick flow.
TREASURY is authoritative current city cash (`elcity-state-api-treasury').
This record intentionally does not infer or expose a previous-treasury value.
TREASURY-MIN and TREASURY-MAX are the S7 clamp bounds."
  (treasury 0 :type integer)
  (tax-income 0 :type integer)
  (total-spent 0 :type integer)
  (treasury-min 0 :type integer)
  (treasury-max 0 :type integer))

(cl-defstruct (elcity-budget-explanation
               (:constructor elcity-budget-explanation--create)
               (:copier nil))
  "Top-level budget explainability record.
Contains income, spend, and treasury sub-explanations."
  (income nil :type (or null elcity-budget-income-explanation))
  (spend nil :type (or null elcity-budget-spend-explanation))
  (treasury nil :type (or null elcity-budget-treasury-explanation)))

;;; ---------- Public builder ----------

(defun elcity-budget-explain-at (state)
  "Return `elcity-budget-explanation' derived from STATE.
All fields are computed from published snapshots and policy inputs
available in STATE.  This function is pure: it reads but never mutates."
  (cl-check-type state elcity-state)
  (let* ((snapshot (elcity-state-api-budget-snapshot state))
         (zones-summary (elcity-state-api-zones-summary state))
         ;; Income explanation
         (tax-rate (elcity-state-api-tax-rate state))
         (residential-pop
          (elcity-zone-dynamics-summary-residential-pop zones-summary))
         (commercial-pop
          (elcity-zone-dynamics-summary-commercial-pop zones-summary))
         (industrial-pop
          (elcity-zone-dynamics-summary-industrial-pop zones-summary))
         (taxable-pop
          (elcity-budget-taxable-population-from-zone-summary zones-summary))
         (income-per-capita elcity-budget-income-per-capita)
         (gross-tax-income
          (elcity-budget-snapshot-gross-tax-income snapshot))
         (collectability
          (elcity-budget-snapshot-collectability snapshot))
         (tax-income (elcity-budget-snapshot-tax-income snapshot))
         ;; Infrastructure breakdown: plant counts and power capacity.
         (world (elcity-state-world state))
         (power-sources (elcity-power-collect-sources world))
         (coal-count (cl-count 'coal power-sources
                               :key #'elcity-power-source-kind))
         (nuclear-count (cl-count 'nuclear power-sources
                                  :key #'elcity-power-source-kind))
         (power-rated (elcity-power-total-capacity power-sources))
         (power-capacity (elcity-state-api-power-capacity state))
         (power-capacity-permille
          (if (> power-rated 0)
              (min 1000 (/ (* power-capacity 1000) power-rated))
            1000))
         ;; Spend explanation (from snapshot which carries demand/percent/target/spent)
         (infrastructure-requested
          (elcity-budget-snapshot-infrastructure-requested snapshot))
         (infrastructure-percent
          (elcity-budget-snapshot-infrastructure-percent snapshot))
         (infrastructure-target
          (elcity-budget-snapshot-infrastructure-target snapshot))
         (infrastructure-spent
          (elcity-budget-snapshot-infrastructure-spent snapshot))
         (fire-requested
          (elcity-budget-snapshot-fire-requested snapshot))
         (fire-percent
          (elcity-budget-snapshot-fire-percent snapshot))
         (fire-target
          (elcity-budget-snapshot-fire-target snapshot))
         (fire-spent (elcity-budget-snapshot-fire-spent snapshot))
         (police-requested
          (elcity-budget-snapshot-police-requested snapshot))
         (police-percent
          (elcity-budget-snapshot-police-percent snapshot))
         (police-target
          (elcity-budget-snapshot-police-target snapshot))
         (police-spent (elcity-budget-snapshot-police-spent snapshot))
         ;; Effectiveness
         (infrastructure-eff
          (elcity-budget-snapshot-infrastructure-eff snapshot))
         (fire-eff (elcity-budget-snapshot-fire-eff snapshot))
         (police-eff (elcity-budget-snapshot-police-eff snapshot))
         ;; Aggregates
         (total-requested (elcity-budget-snapshot-total-requested snapshot))
         (total-target (elcity-budget-snapshot-total-target snapshot))
         (total-spent (elcity-budget-snapshot-total-spent snapshot))
         ;; Treasury explanation
         (treasury (elcity-state-api-treasury state)))
    (elcity-budget-explanation--create
     :income (elcity-budget-income-explanation--create
              :tax-rate tax-rate
              :residential-pop residential-pop
              :commercial-pop commercial-pop
              :industrial-pop industrial-pop
              :taxable-pop taxable-pop
              :income-per-capita income-per-capita
              :gross-tax-income gross-tax-income
              :collectability collectability
              :tax-income tax-income)
     :spend (elcity-budget-spend-explanation--create
             :coal-count coal-count
             :nuclear-count nuclear-count
             :power-capacity-permille power-capacity-permille
             :infrastructure-requested infrastructure-requested
             :infrastructure-percent infrastructure-percent
             :infrastructure-target infrastructure-target
             :infrastructure-spent infrastructure-spent
             :infrastructure-eff infrastructure-eff
             :fire-requested fire-requested
             :fire-percent fire-percent
             :fire-target fire-target
             :fire-spent fire-spent
             :fire-eff fire-eff
             :police-requested police-requested
             :police-percent police-percent
             :police-target police-target
             :police-spent police-spent
             :police-eff police-eff
             :total-requested total-requested
             :total-target total-target
             :total-spent total-spent)
     :treasury (elcity-budget-treasury-explanation--create
                :treasury treasury
                :tax-income tax-income
                :total-spent total-spent
                :treasury-min elcity-budget-treasury-min
                :treasury-max elcity-budget-treasury-max))))

(provide 'elcity-budget-explain)

;;; elcity-budget-explain.el ends here
