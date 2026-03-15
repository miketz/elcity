;;; elcity-budget.el --- S7 budget and services contracts -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; Pure S7 helpers for budget demand, funding policy, and published service
;; snapshot state.
;;
;; This module owns:
;; - treasury/tax-income/request/effectiveness/percent bounds,
;; - S7 demand/policy/snapshot structs,
;; - deterministic default demand and policy builders,
;; - service-kind access helpers for demand, policy, and snapshot values,
;; - deterministic proportional allocation and effectiveness mapping helpers.
;;
;; The S7 model uses a two-layer approach:
;; - Demand: absolute service cost derived from city state each economy tick.
;; - Policy percent: player-controlled 0..100% funding target per service.
;; - Target: demand * percent / 100, used as basis for proportional allocation.

;;; Code:

(require 'cl-lib)
(require 'elcity-util)
(require 'elcity-zones)

(defconst elcity-budget-treasury-min 0
  "Minimum treasury value accepted by S7.")

(defconst elcity-budget-treasury-max 2000000000
  "Maximum treasury value accepted by S7.")

(defconst elcity-budget-tax-income-min 0
  "Minimum tax-income value accepted by S7.")

(defconst elcity-budget-tax-income-max 2000000000
  "Maximum tax-income value accepted by S7.")

(defconst elcity-budget-tax-rate-min 0
  "Minimum tax-rate percent accepted by S7 tax-income provider.")

(defconst elcity-budget-tax-rate-max 20
  "Maximum tax-rate percent accepted by S7 tax-income provider.")

(defconst elcity-budget-income-per-capita 5
  "Baseline income-per-capita multiplier used by S7 tax-income provider.")

(defconst elcity-budget-requested-min 0
  "Minimum requested budget accepted per service in S7.")

(defconst elcity-budget-requested-max 2000000000
  "Maximum requested budget accepted per service in S7.")

(defconst elcity-budget-effectiveness-min 0
  "Minimum service effectiveness permille accepted by S7.")

(defconst elcity-budget-effectiveness-max 1000
  "Maximum service effectiveness permille accepted by S7.")

(defconst elcity-budget-percent-min 0
  "Minimum funding policy percent accepted by S7.")

(defconst elcity-budget-percent-max 100
  "Maximum funding policy percent accepted by S7.")

(defconst elcity-budget-collectability-min 500
  "Minimum collectability permille accepted by S7 (50% floor).")

(defconst elcity-budget-collectability-max 1000
  "Maximum collectability permille accepted by S7 (100% ceiling).")

(defconst elcity-budget-default-treasury 10000
  "Deterministic tick-0 treasury value for S7.")

(defconst elcity-budget-infrastructure-cost-per-tile 1
  "Annual maintenance cost per infrastructure tile (road, powerline, crossing).")

(defconst elcity-budget-infrastructure-base-cost 10
  "Minimum infrastructure service demand regardless of tile count.")

(defconst elcity-budget-fire-station-cost 5
  "Annual maintenance cost per fire station.")

(defconst elcity-budget-fire-base-cost 10
  "Minimum fire service demand regardless of station count.")

(defconst elcity-budget-police-station-cost 5
  "Annual maintenance cost per police station.")

(defconst elcity-budget-police-base-cost 10
  "Minimum police service demand regardless of station count.")

(defconst elcity-budget-park-cost 1
  "Annual maintenance cost per park tile.")

(defconst elcity-budget-coal-operational-cost 40
  "Annual operational cost per coal power plant.")

(defconst elcity-budget-nuclear-operational-cost 80
  "Annual operational cost per nuclear power plant.")

(defconst elcity-budget-default-infrastructure-percent 100
  "Deterministic default funding policy percent for infrastructure.")

(defconst elcity-budget-default-fire-percent 100
  "Deterministic default funding policy percent for fire.")

(defconst elcity-budget-default-police-percent 100
  "Deterministic default funding policy percent for police.")

(defconst elcity-budget-service-kinds
  '(infrastructure fire police)
  "Canonical ordered list of supported S7 service kind symbols.")

(defconst elcity-budget-allocation-priority
  elcity-budget-service-kinds
  "Deterministic remainder-distribution order for proportional allocation.")

;;; --- Demand struct ---

(cl-defstruct (elcity-budget-demand
               (:constructor elcity-budget-demand-create))
  "S7 service demand: absolute requested cost derived from city state."
  (infrastructure-requested 0 :type integer)
  (fire-requested 0 :type integer)
  (police-requested 0 :type integer))

;;; --- Policy struct (percent-based) ---

(cl-defstruct (elcity-budget-policy
               (:constructor elcity-budget-policy-create))
  "S7 funding policy: per-service percent (0..100) controlling target spend."
  (infrastructure-percent elcity-budget-default-infrastructure-percent
                          :type integer)
  (fire-percent elcity-budget-default-fire-percent :type integer)
  (police-percent elcity-budget-default-police-percent :type integer))

;;; --- Snapshot struct ---

(cl-defstruct (elcity-budget-snapshot
               (:constructor elcity-budget-snapshot-create))
  "S7 published snapshot for budget arithmetic and service effectiveness.
TREASURY is available city cash after S7 update.
TAX-INCOME and CASH-DELTA are current-tick flow values.
Requested fields are per-service demand (absolute cost from city state).
Percent fields are per-service funding policy (0..100).
Target fields are demand * percent / 100 (desired spend before allocation).
Spent fields are per-service allocated amounts.
Total-requested, total-target, and total-spent are aggregate sums.
Effectiveness fields are per-service permille values in [0,1000].
CONSTRUCTION-SPENT accumulates S12 building costs within a tick and
resets to 0 when `elcity-budget-advance' creates a fresh snapshot."
  (treasury elcity-budget-default-treasury :type integer)
  (tax-income 0 :type integer)
  (cash-delta 0 :type integer)
  ;; Demand (absolute requested cost from city state)
  (infrastructure-requested 0 :type integer)
  (fire-requested 0 :type integer)
  (police-requested 0 :type integer)
  ;; Policy percent (0..100)
  (infrastructure-percent elcity-budget-default-infrastructure-percent
                          :type integer)
  (fire-percent elcity-budget-default-fire-percent :type integer)
  (police-percent elcity-budget-default-police-percent :type integer)
  ;; Target (demand * percent / 100)
  (infrastructure-target 0 :type integer)
  (fire-target 0 :type integer)
  (police-target 0 :type integer)
  ;; Spent (allocated from available funds)
  (infrastructure-spent 0 :type integer)
  (fire-spent 0 :type integer)
  (police-spent 0 :type integer)
  ;; Aggregates
  (total-requested 0 :type integer)
  (total-target 0 :type integer)
  (total-spent 0 :type integer)
  ;; Effectiveness (permille 0..1000)
  (infrastructure-eff 0 :type integer)
  (fire-eff 0 :type integer)
  (police-eff 0 :type integer)
  (construction-spent 0 :type integer)
  ;; Collectability (permille 500..1000): fraction of gross tax actually collected.
  ;; 1000 = 100% collected (no crime penalty), 500 = 50% floor at max crime.
  (collectability 1000 :type integer)
  ;; Gross tax income before collectability attenuation.
  (gross-tax-income 0 :type integer))

;;; --- Internal helpers ---

(defun elcity-budget--check-kind (kind)
  "Signal error when KIND is not one supported S7 service kind symbol."
  (unless (memq kind elcity-budget-service-kinds)
    (error "Unknown budget service kind: %S" kind)))

;;; --- Public kind/clamp helpers ---

(defun elcity-budget-check-kind (kind)
  "Signal error when KIND is not one supported S7 service symbol."
  (elcity-budget--check-kind kind))

(defun elcity-budget-clamp-treasury (value)
  "Return treasury VALUE clamped to S7 treasury range."
  (elcity-util-clamp value
                        elcity-budget-treasury-min
                        elcity-budget-treasury-max))

(defun elcity-budget-clamp-tax-income (value)
  "Return tax-income VALUE clamped to S7 tax-income range."
  (elcity-util-clamp value
                        elcity-budget-tax-income-min
                        elcity-budget-tax-income-max))

(defun elcity-budget-clamp-tax-rate (value)
  "Return tax-rate VALUE clamped to S7 tax-income provider range."
  (elcity-util-clamp value
                        elcity-budget-tax-rate-min
                        elcity-budget-tax-rate-max))

(defun elcity-budget-clamp-requested (value)
  "Return requested service budget VALUE clamped to S7 request range."
  (elcity-util-clamp value
                        elcity-budget-requested-min
                        elcity-budget-requested-max))

(defun elcity-budget-clamp-effectiveness (value)
  "Return effectiveness permille VALUE clamped to S7 range."
  (elcity-util-clamp value
                        elcity-budget-effectiveness-min
                        elcity-budget-effectiveness-max))

(defun elcity-budget-clamp-percent (value)
  "Return funding policy percent VALUE clamped to S7 range."
  (elcity-util-clamp value
                        elcity-budget-percent-min
                        elcity-budget-percent-max))

(defun elcity-budget-clamp-collectability (value)
  "Return collectability permille VALUE clamped to S7 range [500,1000]."
  (elcity-util-clamp value
                        elcity-budget-collectability-min
                        elcity-budget-collectability-max))

(defun elcity-budget-compute-collectability (crime-avg)
  "Return collectability permille from CRIME-AVG in [0,1000].
50% of gross tax is always collected; 50% scales linearly with crime.
Formula: clamp(500 + 500*(1000 - crime-avg)/1000, 500, 1000)."
  (cl-check-type crime-avg integer)
  (let ((clamped (elcity-util-clamp crime-avg 0 1000)))
    (elcity-budget-clamp-collectability
     (+ 500 (/ (* 500 (- 1000 clamped)) 1000)))))

;;; --- Demand helpers ---

(defun elcity-budget-default-demand ()
  "Return deterministic default S7 service demand with zero requested values.
Dynamic demand is computed from city state each economy tick; this
default represents the absence of any demand before the first tick."
  (elcity-budget-demand-create))

(defun elcity-budget-demand-requested-at (demand kind)
  "Return requested budget from DEMAND for S7 service KIND."
  (cl-check-type demand elcity-budget-demand)
  (elcity-budget--check-kind kind)
  (pcase kind
    ('infrastructure
     (elcity-budget-demand-infrastructure-requested demand))
    ('fire
     (elcity-budget-demand-fire-requested demand))
    ('police
     (elcity-budget-demand-police-requested demand))))

(defun elcity-budget-demand-with-requested (demand kind requested)
  "Return DEMAND copied with KIND requested budget set to REQUESTED."
  (cl-check-type demand elcity-budget-demand)
  (elcity-budget--check-kind kind)
  (cl-check-type requested integer)
  (let ((next (copy-elcity-budget-demand demand))
        (value (elcity-budget-clamp-requested requested)))
    (pcase kind
      ('infrastructure
       (setf (elcity-budget-demand-infrastructure-requested next) value))
      ('fire
       (setf (elcity-budget-demand-fire-requested next) value))
      ('police
       (setf (elcity-budget-demand-police-requested next) value)))
    next))

(defun elcity-budget-demand-total-requested (demand)
  "Return total requested budget across all services in DEMAND."
  (cl-check-type demand elcity-budget-demand)
  (+ (elcity-budget-demand-infrastructure-requested demand)
     (elcity-budget-demand-fire-requested demand)
     (elcity-budget-demand-police-requested demand)))

(defun elcity-budget-demand-normalize (demand)
  "Return DEMAND copied with requested values clamped to S7 request bounds."
  (cl-check-type demand elcity-budget-demand)
  (elcity-budget-demand-create
   :infrastructure-requested
   (elcity-budget-clamp-requested
    (elcity-budget-demand-infrastructure-requested demand))
   :fire-requested
   (elcity-budget-clamp-requested
    (elcity-budget-demand-fire-requested demand))
   :police-requested
   (elcity-budget-clamp-requested
    (elcity-budget-demand-police-requested demand))))

;;; --- Policy helpers ---

(defun elcity-budget-default-policy ()
  "Return deterministic default S7 funding percent policy."
  (elcity-budget-policy-create
   :infrastructure-percent elcity-budget-default-infrastructure-percent
   :fire-percent elcity-budget-default-fire-percent
   :police-percent elcity-budget-default-police-percent))

(defun elcity-budget-policy-percent-at (policy kind)
  "Return funding percent from POLICY for S7 service KIND."
  (cl-check-type policy elcity-budget-policy)
  (elcity-budget--check-kind kind)
  (pcase kind
    ('infrastructure
     (elcity-budget-policy-infrastructure-percent policy))
    ('fire
     (elcity-budget-policy-fire-percent policy))
    ('police
     (elcity-budget-policy-police-percent policy))))

(defun elcity-budget-policy-with-percent (policy kind percent)
  "Return POLICY copied with KIND funding percent set to PERCENT."
  (cl-check-type policy elcity-budget-policy)
  (elcity-budget--check-kind kind)
  (cl-check-type percent integer)
  (let ((next (copy-elcity-budget-policy policy))
        (value (elcity-budget-clamp-percent percent)))
    (pcase kind
      ('infrastructure
       (setf (elcity-budget-policy-infrastructure-percent next) value))
      ('fire
       (setf (elcity-budget-policy-fire-percent next) value))
      ('police
       (setf (elcity-budget-policy-police-percent next) value)))
    next))

(defun elcity-budget-policy-normalize (policy)
  "Return POLICY copied with percent values clamped to S7 percent bounds."
  (cl-check-type policy elcity-budget-policy)
  (elcity-budget-policy-create
   :infrastructure-percent
   (elcity-budget-clamp-percent
    (elcity-budget-policy-infrastructure-percent policy))
   :fire-percent
   (elcity-budget-clamp-percent
    (elcity-budget-policy-fire-percent policy))
   :police-percent
   (elcity-budget-clamp-percent
    (elcity-budget-policy-police-percent policy))))

;;; --- Target computation ---

(defun elcity-budget-compute-targets (demand policy)
  "Return demand struct with per-service target = DEMAND * POLICY percent / 100.
DEMAND is an `elcity-budget-demand' with absolute requested costs.
POLICY is an `elcity-budget-policy' with per-service funding percents.
Returned values are clamped to S7 request bounds."
  (cl-check-type demand elcity-budget-demand)
  (cl-check-type policy elcity-budget-policy)
  (let* ((nd (elcity-budget-demand-normalize demand))
         (np (elcity-budget-policy-normalize policy)))
    (elcity-budget-demand-create
     :infrastructure-requested
     (elcity-budget-clamp-requested
      (/ (* (elcity-budget-demand-infrastructure-requested nd)
            (elcity-budget-policy-infrastructure-percent np))
         100))
     :fire-requested
     (elcity-budget-clamp-requested
      (/ (* (elcity-budget-demand-fire-requested nd)
            (elcity-budget-policy-fire-percent np))
         100))
     :police-requested
     (elcity-budget-clamp-requested
      (/ (* (elcity-budget-demand-police-requested nd)
            (elcity-budget-policy-police-percent np))
         100)))))

;;; --- Tax income helpers ---

(defun elcity-budget-available-funds (treasury tax-income)
  "Return deterministic non-negative available funds from TREASURY and TAX-INCOME."
  (cl-check-type treasury integer)
  (cl-check-type tax-income integer)
  (+ (elcity-budget-clamp-treasury treasury)
     (elcity-budget-clamp-tax-income tax-income)))

(defun elcity-budget-taxable-population-from-zone-summary (summary)
  "Return taxable population aggregate derived from S4 zone SUMMARY."
  (cl-check-type summary elcity-zone-dynamics-summary)
  (+ (max 0 (elcity-zone-dynamics-summary-residential-pop summary))
     (max 0 (elcity-zone-dynamics-summary-commercial-pop summary))
     (max 0 (elcity-zone-dynamics-summary-industrial-pop summary))))

(defun elcity-budget-tax-income-from-totals (taxable-pop tax-rate
                                                        &optional income-per-capita)
  "Return clamped S7 tax-income from TAXABLE-POP and TAX-RATE percent.
When INCOME-PER-CAPITA is nil, use `elcity-budget-income-per-capita'."
  (cl-check-type taxable-pop integer)
  (cl-check-type tax-rate integer)
  (let* ((population (max 0 taxable-pop))
         (rate (elcity-budget-clamp-tax-rate tax-rate))
         (income-scale (max 0 (or income-per-capita
                                 elcity-budget-income-per-capita)))
         (raw (/ (* population rate income-scale) 100)))
    (elcity-budget-clamp-tax-income raw)))

(defun elcity-budget-tax-income-from-zone-summary (summary tax-rate
                                                           &optional income-per-capita)
  "Return clamped S7 tax-income from zone SUMMARY and TAX-RATE.
Optional INCOME-PER-CAPITA overrides provider default scaling."
  (cl-check-type summary elcity-zone-dynamics-summary)
  (cl-check-type tax-rate integer)
  (elcity-budget-tax-income-from-totals
   (elcity-budget-taxable-population-from-zone-summary summary)
   tax-rate
   income-per-capita))

;;; --- Effectiveness ---

(defun elcity-budget-effectiveness-for-service (spent requested)
  "Return effectiveness permille from SPENT versus REQUESTED service budget."
  (cl-check-type spent integer)
  (cl-check-type requested integer)
  (if (<= requested 0)
      0
    (elcity-budget-clamp-effectiveness
     (/ (* spent elcity-budget-effectiveness-max)
        requested))))

;;; --- Allocation ---

(defun elcity-budget-allocate-spending (targets available)
  "Return spent-budget demand for TARGETS under AVAILABLE funds.
TARGETS is an `elcity-budget-demand' representing per-service target spend.
Allocation uses proportional floor scaling, then deterministic remainder
distribution by `elcity-budget-allocation-priority'."
  (cl-check-type targets elcity-budget-demand)
  (cl-check-type available integer)
  (let* ((normalized (elcity-budget-demand-normalize targets))
         (available-funds (max 0 available))
         (infrastructure-target
          (elcity-budget-demand-infrastructure-requested normalized))
         (fire-target (elcity-budget-demand-fire-requested normalized))
         (police-target (elcity-budget-demand-police-requested normalized))
         (total-target (elcity-budget-demand-total-requested normalized)))
    (cond
     ((<= total-target 0)
      (elcity-budget-demand-create
       :infrastructure-requested 0
       :fire-requested 0
       :police-requested 0))
     ((>= available-funds total-target)
      (copy-elcity-budget-demand normalized))
     (t
      (let* ((infrastructure-spent (/ (* infrastructure-target available-funds)
                                      total-target))
             (fire-spent (/ (* fire-target available-funds)
                            total-target))
             (police-spent (/ (* police-target available-funds)
                              total-target))
             (remainder (- available-funds
                           (+ infrastructure-spent fire-spent police-spent))))
        (while (> remainder 0)
          (dolist (kind elcity-budget-allocation-priority)
            (when (> remainder 0)
              (pcase kind
                ('infrastructure
                 (when (< infrastructure-spent infrastructure-target)
                   (setq infrastructure-spent (1+ infrastructure-spent)
                         remainder (1- remainder))))
                ('fire
                 (when (< fire-spent fire-target)
                   (setq fire-spent (1+ fire-spent)
                         remainder (1- remainder))))
                ('police
                 (when (< police-spent police-target)
                   (setq police-spent (1+ police-spent)
                         remainder (1- remainder))))))))
        (elcity-budget-demand-create
         :infrastructure-requested infrastructure-spent
         :fire-requested fire-spent
         :police-requested police-spent))))))

;;; --- Budget advance ---

(defun elcity-budget-advance (treasury tax-income demand policy
                                        &optional crime-avg)
  "Return S7 budget snapshot for TREASURY, TAX-INCOME, DEMAND, POLICY.
DEMAND is an `elcity-budget-demand' with absolute requested costs.
POLICY is an `elcity-budget-policy' with per-service funding percents.
Optional CRIME-AVG (integer 0..1000) drives collectability attenuation
of TAX-INCOME.  When nil, full collectability (1000) is used."
  (cl-check-type treasury integer)
  (cl-check-type tax-income integer)
  (cl-check-type demand elcity-budget-demand)
  (cl-check-type policy elcity-budget-policy)
  (let* ((current-treasury (elcity-budget-clamp-treasury treasury))
         (gross-tax (elcity-budget-clamp-tax-income tax-income))
         (collectability (elcity-budget-compute-collectability
                          (or crime-avg 0)))
         (income (elcity-budget-clamp-tax-income
                  (/ (* gross-tax collectability) 1000)))
         (norm-demand (elcity-budget-demand-normalize demand))
         (norm-policy (elcity-budget-policy-normalize policy))
         (targets (elcity-budget-compute-targets norm-demand norm-policy))
         (available (elcity-budget-available-funds current-treasury income))
         (spent (elcity-budget-allocate-spending targets available))
         (total-requested (elcity-budget-demand-total-requested norm-demand))
         (total-target (elcity-budget-demand-total-requested targets))
         (total-spent (elcity-budget-demand-total-requested spent))
         (cash-delta (- income total-spent))
         (treasury-next (elcity-budget-clamp-treasury
                         (+ current-treasury cash-delta)))
         ;; Per-service demand
         (infrastructure-requested
          (elcity-budget-demand-requested-at norm-demand 'infrastructure))
         (fire-requested (elcity-budget-demand-requested-at norm-demand 'fire))
         (police-requested (elcity-budget-demand-requested-at norm-demand 'police))
         ;; Per-service percent
         (infrastructure-percent
          (elcity-budget-policy-percent-at norm-policy 'infrastructure))
         (fire-percent (elcity-budget-policy-percent-at norm-policy 'fire))
         (police-percent (elcity-budget-policy-percent-at norm-policy 'police))
         ;; Per-service target
         (infrastructure-target
          (elcity-budget-demand-requested-at targets 'infrastructure))
         (fire-target (elcity-budget-demand-requested-at targets 'fire))
         (police-target (elcity-budget-demand-requested-at targets 'police))
         ;; Per-service spent
         (infrastructure-spent
          (elcity-budget-demand-requested-at spent 'infrastructure))
         (fire-spent (elcity-budget-demand-requested-at spent 'fire))
         (police-spent (elcity-budget-demand-requested-at spent 'police)))
    (elcity-budget-snapshot-create
     :treasury treasury-next
     :tax-income income
     :cash-delta cash-delta
     :infrastructure-requested infrastructure-requested
     :fire-requested fire-requested
     :police-requested police-requested
     :infrastructure-percent infrastructure-percent
     :fire-percent fire-percent
     :police-percent police-percent
     :infrastructure-target infrastructure-target
     :fire-target fire-target
     :police-target police-target
     :infrastructure-spent infrastructure-spent
     :fire-spent fire-spent
     :police-spent police-spent
     :total-requested total-requested
     :total-target total-target
     :total-spent total-spent
     :infrastructure-eff (elcity-budget-effectiveness-for-service
                          infrastructure-spent
                          infrastructure-target)
     :fire-eff (elcity-budget-effectiveness-for-service
                fire-spent
                fire-target)
     :police-eff (elcity-budget-effectiveness-for-service
                  police-spent
                  police-target)
     :collectability collectability
     :gross-tax-income gross-tax)))

;;; --- Validation ---

(defun elcity-budget-affordable-p (treasury tax-income snapshot)
  "Return non-nil when SNAPSHOT spending is affordable for TREASURY and TAX-INCOME."
  (cl-check-type treasury integer)
  (cl-check-type tax-income integer)
  (cl-check-type snapshot elcity-budget-snapshot)
  (<= (elcity-budget-snapshot-total-spent snapshot)
      (elcity-budget-available-funds treasury tax-income)))

(defun elcity-budget-snapshot-consistent-p (snapshot)
  "Return non-nil when SNAPSHOT satisfies S7 arithmetic and bounds invariants."
  (cl-check-type snapshot elcity-budget-snapshot)
  (let* ((infrastructure-requested
          (elcity-budget-snapshot-infrastructure-requested snapshot))
         (fire-requested (elcity-budget-snapshot-fire-requested snapshot))
         (police-requested (elcity-budget-snapshot-police-requested snapshot))
         (infrastructure-percent
          (elcity-budget-snapshot-infrastructure-percent snapshot))
         (fire-percent (elcity-budget-snapshot-fire-percent snapshot))
         (police-percent (elcity-budget-snapshot-police-percent snapshot))
         (infrastructure-target
          (elcity-budget-snapshot-infrastructure-target snapshot))
         (fire-target (elcity-budget-snapshot-fire-target snapshot))
         (police-target (elcity-budget-snapshot-police-target snapshot))
         (infrastructure-spent
          (elcity-budget-snapshot-infrastructure-spent snapshot))
         (fire-spent (elcity-budget-snapshot-fire-spent snapshot))
         (police-spent (elcity-budget-snapshot-police-spent snapshot))
         (requested-sum (+ infrastructure-requested fire-requested
                           police-requested))
         (target-sum (+ infrastructure-target fire-target police-target))
         (spent-sum (+ infrastructure-spent fire-spent police-spent))
         (infrastructure-eff (elcity-budget-snapshot-infrastructure-eff snapshot))
         (fire-eff (elcity-budget-snapshot-fire-eff snapshot))
         (police-eff (elcity-budget-snapshot-police-eff snapshot)))
    (and (<= elcity-budget-treasury-min
             (elcity-budget-snapshot-treasury snapshot))
         (>= elcity-budget-treasury-max
             (elcity-budget-snapshot-treasury snapshot))
         (<= elcity-budget-tax-income-min
             (elcity-budget-snapshot-tax-income snapshot))
         (>= elcity-budget-tax-income-max
             (elcity-budget-snapshot-tax-income snapshot))
         ;; Demand aggregates
         (= requested-sum
            (elcity-budget-snapshot-total-requested snapshot))
         ;; Target aggregates
         (= target-sum
            (elcity-budget-snapshot-total-target snapshot))
         ;; Spent aggregates
         (= spent-sum
            (elcity-budget-snapshot-total-spent snapshot))
         ;; Non-negative demand
         (<= 0 infrastructure-requested)
         (<= 0 fire-requested)
         (<= 0 police-requested)
         ;; Percent bounds
         (<= elcity-budget-percent-min infrastructure-percent)
         (>= elcity-budget-percent-max infrastructure-percent)
         (<= elcity-budget-percent-min fire-percent)
         (>= elcity-budget-percent-max fire-percent)
         (<= elcity-budget-percent-min police-percent)
         (>= elcity-budget-percent-max police-percent)
         ;; Non-negative targets
         (<= 0 infrastructure-target)
         (<= 0 fire-target)
         (<= 0 police-target)
         ;; Non-negative spent
         (<= 0 infrastructure-spent)
         (<= 0 fire-spent)
         (<= 0 police-spent)
         ;; Spent never exceeds target
         (<= infrastructure-spent infrastructure-target)
         (<= fire-spent fire-target)
         (<= police-spent police-target)
         ;; Effectiveness bounds
         (<= elcity-budget-effectiveness-min infrastructure-eff)
         (>= elcity-budget-effectiveness-max infrastructure-eff)
         (<= elcity-budget-effectiveness-min fire-eff)
         (>= elcity-budget-effectiveness-max fire-eff)
         (<= elcity-budget-effectiveness-min police-eff)
         (>= elcity-budget-effectiveness-max police-eff)
         ;; Effectiveness matches arithmetic
         (= infrastructure-eff
            (elcity-budget-effectiveness-for-service
             infrastructure-spent
             infrastructure-target))
         (= fire-eff
            (elcity-budget-effectiveness-for-service
             fire-spent
             fire-target))
         (= police-eff
            (elcity-budget-effectiveness-for-service
             police-spent
             police-target))
         ;; Construction spent non-negative
         (<= 0 (elcity-budget-snapshot-construction-spent snapshot))
         ;; Collectability bounds
         (<= elcity-budget-collectability-min
             (elcity-budget-snapshot-collectability snapshot))
         (>= elcity-budget-collectability-max
             (elcity-budget-snapshot-collectability snapshot))
         ;; Gross tax income non-negative
         (<= 0 (elcity-budget-snapshot-gross-tax-income snapshot))
         ;; Collectability arithmetic: tax = floor(gross * coll / 1000)
         (= (elcity-budget-snapshot-tax-income snapshot)
            (elcity-budget-clamp-tax-income
             (/ (* (elcity-budget-snapshot-gross-tax-income snapshot)
                   (elcity-budget-snapshot-collectability snapshot))
                1000))))))

;;; --- Default snapshot ---

(defun elcity-budget-default-snapshot (&optional treasury demand policy)
  "Return default S7 published snapshot for TREASURY, DEMAND, and POLICY.
When DEMAND is nil, use deterministic default demand.
When POLICY is nil, use deterministic default funding percent policy."
  (let* ((next-demand (elcity-budget-demand-normalize
                       (or demand (elcity-budget-default-demand))))
         (next-policy (elcity-budget-policy-normalize
                       (or policy (elcity-budget-default-policy))))
         (targets (elcity-budget-compute-targets next-demand next-policy))
         (infrastructure-requested
          (elcity-budget-demand-requested-at next-demand 'infrastructure))
         (fire-requested
          (elcity-budget-demand-requested-at next-demand 'fire))
         (police-requested
          (elcity-budget-demand-requested-at next-demand 'police))
         (total-requested (elcity-budget-demand-total-requested next-demand))
         (infrastructure-percent
          (elcity-budget-policy-percent-at next-policy 'infrastructure))
         (fire-percent
          (elcity-budget-policy-percent-at next-policy 'fire))
         (police-percent
          (elcity-budget-policy-percent-at next-policy 'police))
         (infrastructure-target
          (elcity-budget-demand-requested-at targets 'infrastructure))
         (fire-target
          (elcity-budget-demand-requested-at targets 'fire))
         (police-target
          (elcity-budget-demand-requested-at targets 'police))
         (total-target (elcity-budget-demand-total-requested targets)))
    (elcity-budget-snapshot-create
     :treasury (elcity-budget-clamp-treasury
                (or treasury elcity-budget-default-treasury))
     :tax-income 0
     :cash-delta 0
     :infrastructure-requested infrastructure-requested
     :fire-requested fire-requested
     :police-requested police-requested
     :infrastructure-percent infrastructure-percent
     :fire-percent fire-percent
     :police-percent police-percent
     :infrastructure-target infrastructure-target
     :fire-target fire-target
     :police-target police-target
     :infrastructure-spent infrastructure-target
     :fire-spent fire-target
     :police-spent police-target
     :total-requested total-requested
     :total-target total-target
     :total-spent total-target
     :infrastructure-eff (elcity-budget-effectiveness-for-service
                          infrastructure-target
                          infrastructure-target)
     :fire-eff (elcity-budget-effectiveness-for-service
                fire-target
                fire-target)
     :police-eff (elcity-budget-effectiveness-for-service
                  police-target
                  police-target)
     :collectability elcity-budget-collectability-max
     :gross-tax-income 0)))

;;; --- Snapshot accessors ---

(defun elcity-budget-snapshot-requested-at (snapshot kind)
  "Return requested demand from S7 SNAPSHOT for service KIND."
  (cl-check-type snapshot elcity-budget-snapshot)
  (elcity-budget--check-kind kind)
  (pcase kind
    ('infrastructure
     (elcity-budget-snapshot-infrastructure-requested snapshot))
    ('fire
     (elcity-budget-snapshot-fire-requested snapshot))
    ('police
     (elcity-budget-snapshot-police-requested snapshot))))

(defun elcity-budget-snapshot-percent-at (snapshot kind)
  "Return funding policy percent from S7 SNAPSHOT for service KIND."
  (cl-check-type snapshot elcity-budget-snapshot)
  (elcity-budget--check-kind kind)
  (pcase kind
    ('infrastructure
     (elcity-budget-snapshot-infrastructure-percent snapshot))
    ('fire
     (elcity-budget-snapshot-fire-percent snapshot))
    ('police
     (elcity-budget-snapshot-police-percent snapshot))))

(defun elcity-budget-snapshot-target-at (snapshot kind)
  "Return target spend from S7 SNAPSHOT for service KIND."
  (cl-check-type snapshot elcity-budget-snapshot)
  (elcity-budget--check-kind kind)
  (pcase kind
    ('infrastructure
     (elcity-budget-snapshot-infrastructure-target snapshot))
    ('fire
     (elcity-budget-snapshot-fire-target snapshot))
    ('police
     (elcity-budget-snapshot-police-target snapshot))))

(defun elcity-budget-snapshot-spent-at (snapshot kind)
  "Return spent budget from S7 SNAPSHOT for service KIND."
  (cl-check-type snapshot elcity-budget-snapshot)
  (elcity-budget--check-kind kind)
  (pcase kind
    ('infrastructure
     (elcity-budget-snapshot-infrastructure-spent snapshot))
    ('fire
     (elcity-budget-snapshot-fire-spent snapshot))
    ('police
     (elcity-budget-snapshot-police-spent snapshot))))

(defun elcity-budget-snapshot-effectiveness-at (snapshot kind)
  "Return service effectiveness permille from S7 SNAPSHOT for KIND."
  (cl-check-type snapshot elcity-budget-snapshot)
  (elcity-budget--check-kind kind)
  (pcase kind
    ('infrastructure
     (elcity-budget-snapshot-infrastructure-eff snapshot))
    ('fire
     (elcity-budget-snapshot-fire-eff snapshot))
    ('police
     (elcity-budget-snapshot-police-eff snapshot))))

(provide 'elcity-budget)

;;; elcity-budget.el ends here
