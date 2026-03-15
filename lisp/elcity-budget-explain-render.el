;;; elcity-budget-explain-render.el --- Budget explainability text adapters -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; Rendering adapters for `elcity-budget-explain' payloads.
;;
;; This module owns deterministic string formatting for UI/debug views and
;; keeps presentation policy out of the payload builder.
;;
;; Design note:
;; - Treasury text intentionally reports current treasury only and does not
;;   infer previous treasury.
;; - Spending text intentionally reports S7 service spending only and excludes
;;   user-initiated S12 construction spending.

;;; Code:

(require 'cl-lib)
(require 'elcity-budget-explain)

(defun elcity-budget-explain-format-income (explanation)
  "Return deterministic income breakdown string from EXPLANATION."
  (cl-check-type explanation elcity-budget-explanation)
  (let* ((inc (elcity-budget-explanation-income explanation))
         (tax-rate (elcity-budget-income-explanation-tax-rate inc))
         (r-pop (elcity-budget-income-explanation-residential-pop inc))
         (c-pop (elcity-budget-income-explanation-commercial-pop inc))
         (i-pop (elcity-budget-income-explanation-industrial-pop inc))
         (taxable (elcity-budget-income-explanation-taxable-pop inc))
         (per-cap
          (elcity-budget-income-explanation-income-per-capita inc))
         (gross
          (elcity-budget-income-explanation-gross-tax-income inc))
         (coll
          (elcity-budget-income-explanation-collectability inc))
         (income (elcity-budget-income-explanation-tax-income inc)))
    (format (concat
             "Income: $%d\n"
             "Tax Rate: %d%%\n"
             "Taxable Pop: %d (R:%d C:%d I:%d)\n"
             "Per Capita: %d\n"
             "Gross Tax: $%d\n"
             "Collectability: %d%% (50%% base + %d%% crime)\n"
             "Collected Tax: $%d")
            income tax-rate taxable r-pop c-pop i-pop per-cap
            gross
            (/ coll 10) (/ (- coll 500) 10)
            income)))

(defun elcity-budget-explain--plant-opex-line (sp)
  "Return plant opex detail string from spend explanation SP, or nil."
  (let ((coal (elcity-budget-spend-explanation-coal-count sp))
        (nuclear (elcity-budget-spend-explanation-nuclear-count sp)))
    (cond
     ((and (> coal 0) (> nuclear 0))
      (format "  Plant opex: %d coal ($%d) + %d nuclear ($%d)"
              coal (* coal elcity-budget-coal-operational-cost)
              nuclear (* nuclear elcity-budget-nuclear-operational-cost)))
     ((> coal 0)
      (format "  Plant opex: %d coal ($%d)"
              coal (* coal elcity-budget-coal-operational-cost)))
     ((> nuclear 0)
      (format "  Plant opex: %d nuclear ($%d)"
              nuclear
              (* nuclear elcity-budget-nuclear-operational-cost))))))

(defun elcity-budget-explain-format-spend (explanation)
  "Return deterministic service-spending breakdown string from EXPLANATION."
  (cl-check-type explanation elcity-budget-explanation)
  (let* ((sp (elcity-budget-explanation-spend explanation))
         (infra-req
          (elcity-budget-spend-explanation-infrastructure-requested sp))
         (infra-pct
          (elcity-budget-spend-explanation-infrastructure-percent sp))
         (infra-tgt
          (elcity-budget-spend-explanation-infrastructure-target sp))
         (infra-spent
          (elcity-budget-spend-explanation-infrastructure-spent sp))
         (infra-eff
          (elcity-budget-spend-explanation-infrastructure-eff sp))
         (cap-pml
          (elcity-budget-spend-explanation-power-capacity-permille sp))
         (fire-req (elcity-budget-spend-explanation-fire-requested sp))
         (fire-pct (elcity-budget-spend-explanation-fire-percent sp))
         (fire-tgt (elcity-budget-spend-explanation-fire-target sp))
         (fire-spent (elcity-budget-spend-explanation-fire-spent sp))
         (fire-eff (elcity-budget-spend-explanation-fire-eff sp))
         (police-req
          (elcity-budget-spend-explanation-police-requested sp))
         (police-pct
          (elcity-budget-spend-explanation-police-percent sp))
         (police-tgt
          (elcity-budget-spend-explanation-police-target sp))
         (police-spent
          (elcity-budget-spend-explanation-police-spent sp))
         (police-eff
          (elcity-budget-spend-explanation-police-eff sp))
         (total-spent
          (elcity-budget-spend-explanation-total-spent sp))
         (plant-line (elcity-budget-explain--plant-opex-line sp))
         (base
          (format (concat
                   "Spending: $%d\n"
                   "Infrastructure: $%d/$%d"
                   " (demand $%d @ %d%%)"
                   " eff %d%% -> power %d%%\n"
                   "%s"
                   "Fire: $%d/$%d (demand $%d @ %d%%) eff %d%%\n"
                   "Police: $%d/$%d (demand $%d @ %d%%) eff %d%%")
                  total-spent
                  infra-spent infra-tgt infra-req infra-pct
                  (/ infra-eff 10) (/ cap-pml 10)
                  (if plant-line (concat plant-line "\n") "")
                  fire-spent fire-tgt fire-req fire-pct
                  (/ fire-eff 10)
                  police-spent police-tgt police-req police-pct
                  (/ police-eff 10))))
    base))

(defun elcity-budget-explain-format-treasury (explanation)
  "Return deterministic current-treasury breakdown string from EXPLANATION."
  (cl-check-type explanation elcity-budget-explanation)
  (let* ((tr (elcity-budget-explanation-treasury explanation))
         (treasury (elcity-budget-treasury-explanation-treasury tr))
         (income (elcity-budget-treasury-explanation-tax-income tr))
         (spent (elcity-budget-treasury-explanation-total-spent tr)))
    (format "Treasury: $%d\nIncome: $%d\nSpending: $%d"
            treasury income spent)))

(defun elcity-budget-explain-format-full (explanation)
  "Return complete multi-line budget summary from EXPLANATION."
  (cl-check-type explanation elcity-budget-explanation)
  (concat (elcity-budget-explain-format-treasury explanation)
          "\n\n"
          (elcity-budget-explain-format-income explanation)
          "\n\n"
          (elcity-budget-explain-format-spend explanation)))

(provide 'elcity-budget-explain-render)

;;; elcity-budget-explain-render.el ends here
