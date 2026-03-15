;;; elcity-evaluation.el --- S8 evaluation and complaints contracts -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; Pure S8 helpers for city score, approval proxy, and complaint ranking.
;;
;; This module owns:
;; - S8 snapshot/memory/signal structs,
;; - deterministic complaint formulas and ranking,
;; - score/approval formulas with smoothing and per-tick caps.

;;; Code:

(require 'cl-lib)
(require 'elcity-util)

(defconst elcity-evaluation-min 0
  "Minimum S8 score/approval/severity value.")

(defconst elcity-evaluation-max 1000
  "Maximum S8 score/approval/severity value.")

(defconst elcity-evaluation-default-score 500
  "Deterministic tick-0 default city score.")

(defconst elcity-evaluation-default-approval 500
  "Deterministic tick-0 default approval proxy.")

(defconst elcity-evaluation-complaint-kinds
  '(power traffic housing jobs taxes budget-services)
  "Deterministic S8 complaint kind ordering and tie-break priority.")

(defconst elcity-evaluation-top-complaints-count 3
  "Number of complaints exposed in S8 top complaint ranking.")

(defconst elcity-evaluation-demand-bound 2000
  "Absolute demand bound used by negative-demand normalization.")

(defconst elcity-evaluation-complaint-ema-denominator 4
  "EMA denominator used by complaint severity smoothing.")

(defconst elcity-evaluation-score-ema-denominator 8
  "EMA denominator used by city score smoothing.")

(defconst elcity-evaluation-approval-ema-denominator 8
  "EMA denominator used by approval smoothing.")

(defconst elcity-evaluation-score-delta-cap 40
  "Maximum absolute score change per tick after smoothing.")

(defconst elcity-evaluation-approval-delta-cap 50
  "Maximum absolute approval change per tick after smoothing.")

;; Weight vectors use permille-style composition and intentionally sum to 1000.
;; Base-health: 230+180+140+150+220+80 = 1000.
;; Approval: 700+150+150 = 1000.
(defconst elcity-evaluation-weight-base-power 230
  "Weight for power complaint in base-health scoring.")

(defconst elcity-evaluation-weight-base-traffic 180
  "Weight for traffic complaint in base-health scoring.")

(defconst elcity-evaluation-weight-base-housing 140
  "Weight for housing complaint in base-health scoring.")

(defconst elcity-evaluation-weight-base-jobs 150
  "Weight for jobs complaint in base-health scoring.")

(defconst elcity-evaluation-weight-base-budget-services 220
  "Weight for budget-services complaint in base-health scoring.")

(defconst elcity-evaluation-weight-base-taxes 80
  "Weight for taxes complaint in base-health scoring.")

(defconst elcity-evaluation-weight-approval-score 700
  "Raw approval formula weight for raw city score.")

(defconst elcity-evaluation-weight-approval-taxes 150
  "Raw approval formula weight for tax complaint complement.")

(defconst elcity-evaluation-weight-approval-budget-services 150
  "Raw approval formula weight for budget-services complaint complement.")

(cl-defstruct (elcity-evaluation-snapshot
               (:constructor elcity-evaluation-snapshot-create))
  "Published S8 snapshot for downstream readers."
  (city-score elcity-evaluation-default-score :type integer)
  (approval elcity-evaluation-default-approval :type integer)
  (top-complaints '(power traffic housing) :type list)
  (complaint-severity-alist nil :type list))

(cl-defstruct (elcity-evaluation-memory
               (:constructor elcity-evaluation-memory-create))
  "S8 lag memory used for next evaluation update."
  (score elcity-evaluation-default-score :type integer)
  (approval elcity-evaluation-default-approval :type integer)
  (complaint-severity-alist nil :type list))

(cl-defstruct (elcity-evaluation-signals
               (:constructor elcity-evaluation-signals-create))
  "Normalized aggregated S8 inputs consumed by complaint formulas."
  (power-unserved 0 :type integer)
  (traffic-shortfall 0 :type integer)
  (traffic-hard-fail 0 :type integer)
  (gates-blocked-ratio 0 :type integer)
  (res-demand-neg 0 :type integer)
  (com-demand-neg 0 :type integer)
  (ind-demand-neg 0 :type integer)
  (tax-pressure 0 :type integer)
  (budget-eff-shortfall 0 :type integer)
  (budget-deficit 0 :type integer)
  (budget-runway 0 :type integer)
  (s6-landvalue-pressure 0 :type integer)
  (s6-pollution-pressure 0 :type integer)
  (s6-crime-pressure 0 :type integer))

(defun elcity-evaluation-clamp-score (value)
  "Return S8 score VALUE clamped to accepted range."
  (elcity-util-clamp value
                            elcity-evaluation-min
                            elcity-evaluation-max))

(defun elcity-evaluation-clamp-approval (value)
  "Return S8 approval VALUE clamped to accepted range."
  (elcity-util-clamp value
                            elcity-evaluation-min
                            elcity-evaluation-max))

(defun elcity-evaluation-clamp-severity (value)
  "Return S8 complaint severity VALUE clamped to accepted range."
  (elcity-util-clamp value
                            elcity-evaluation-min
                            elcity-evaluation-max))

(defun elcity-evaluation-check-complaint-kind (kind)
  "Signal error when KIND is not one supported S8 complaint symbol."
  (unless (memq kind elcity-evaluation-complaint-kinds)
    (error "Unknown evaluation complaint kind: %S" kind)))

(defun elcity-evaluation-normalize-01 (value maxv)
  "Return VALUE normalized to [0,1000] relative to MAXV."
  (cl-check-type value integer)
  (cl-check-type maxv integer)
  (elcity-evaluation-clamp-severity
   (round (* 1000 value)
          (max 1 maxv))))

(defun elcity-evaluation-normalize-negative-demand (demand)
  "Return normalized negative-demand pressure from DEMAND."
  (cl-check-type demand integer)
  (elcity-evaluation-clamp-severity
   (round (* 1000 (max 0 (- demand)))
          elcity-evaluation-demand-bound)))

(defun elcity-evaluation-normalize-shortfall-ratio (ratio)
  "Return normalized shortfall pressure for RATIO in 0..1 range.
RATIO intentionally accepts floating-point input from map ratio accessors."
  (cl-check-type ratio number)
  (elcity-evaluation-clamp-severity
   (round (* 1000 (- 1.0 ratio)))))

(defun elcity-evaluation-default-complaint-severity-alist ()
  "Return default deterministic complaint severity map."
  (mapcar (lambda (kind)
            (cons kind 0))
          elcity-evaluation-complaint-kinds))

(defun elcity-evaluation-normalize-complaint-severity-alist (severity-alist)
  "Return SEVERITY-ALIST normalized to ordered complete S8 complaint map."
  (cl-check-type severity-alist list)
  (mapcar (lambda (kind)
            (let ((entry (assq kind severity-alist)))
              (cons kind
                    (elcity-evaluation-clamp-severity (if entry
                                                          (cdr entry)
                                                        0)))))
          elcity-evaluation-complaint-kinds))

(defun elcity-evaluation-default-top-complaints ()
  "Return deterministic default top complaint list used at tick 0."
  (cl-subseq elcity-evaluation-complaint-kinds
             0
             elcity-evaluation-top-complaints-count))

(defun elcity-evaluation-default-memory ()
  "Return deterministic S8 default lag memory."
  (elcity-evaluation-memory-create
   :score elcity-evaluation-default-score
   :approval elcity-evaluation-default-approval
   :complaint-severity-alist (elcity-evaluation-default-complaint-severity-alist)))

(defun elcity-evaluation-default-snapshot ()
  "Return deterministic S8 default published snapshot."
  (elcity-evaluation-snapshot-create
   :city-score elcity-evaluation-default-score
   :approval elcity-evaluation-default-approval
   :top-complaints (elcity-evaluation-default-top-complaints)
   :complaint-severity-alist (elcity-evaluation-default-complaint-severity-alist)))

(defun elcity-evaluation-memory-complaint-severity-at (memory kind)
  "Return complaint severity from MEMORY for KIND."
  (cl-check-type memory elcity-evaluation-memory)
  (elcity-evaluation-check-complaint-kind kind)
  (let ((entry (assq kind
                     (elcity-evaluation-memory-complaint-severity-alist memory))))
    (if entry
        (cdr entry)
      0)))

(defun elcity-evaluation-snapshot-complaint-severity-at (snapshot kind)
  "Return complaint severity from SNAPSHOT for KIND."
  (cl-check-type snapshot elcity-evaluation-snapshot)
  (elcity-evaluation-check-complaint-kind kind)
  (let ((entry (assq kind
                     (elcity-evaluation-snapshot-complaint-severity-alist snapshot))))
    (if entry
        (cdr entry)
      0)))

(defun elcity-evaluation-signals-normalize (signals)
  "Return SIGNALS copied with all fields clamped to S8 range."
  (cl-check-type signals elcity-evaluation-signals)
  (elcity-evaluation-signals-create
   :power-unserved (elcity-evaluation-clamp-severity
                    (elcity-evaluation-signals-power-unserved signals))
   :traffic-shortfall (elcity-evaluation-clamp-severity
                       (elcity-evaluation-signals-traffic-shortfall signals))
   :traffic-hard-fail (elcity-evaluation-clamp-severity
                       (elcity-evaluation-signals-traffic-hard-fail signals))
   :gates-blocked-ratio (elcity-evaluation-clamp-severity
                         (elcity-evaluation-signals-gates-blocked-ratio signals))
   :res-demand-neg (elcity-evaluation-clamp-severity
                    (elcity-evaluation-signals-res-demand-neg signals))
   :com-demand-neg (elcity-evaluation-clamp-severity
                    (elcity-evaluation-signals-com-demand-neg signals))
   :ind-demand-neg (elcity-evaluation-clamp-severity
                    (elcity-evaluation-signals-ind-demand-neg signals))
   :tax-pressure (elcity-evaluation-clamp-severity
                  (elcity-evaluation-signals-tax-pressure signals))
   :budget-eff-shortfall (elcity-evaluation-clamp-severity
                          (elcity-evaluation-signals-budget-eff-shortfall signals))
   :budget-deficit (elcity-evaluation-clamp-severity
                    (elcity-evaluation-signals-budget-deficit signals))
   :budget-runway (elcity-evaluation-clamp-severity
                   (elcity-evaluation-signals-budget-runway signals))
   :s6-landvalue-pressure (elcity-evaluation-clamp-severity
                           (elcity-evaluation-signals-s6-landvalue-pressure signals))
   :s6-pollution-pressure (elcity-evaluation-clamp-severity
                           (elcity-evaluation-signals-s6-pollution-pressure signals))
   :s6-crime-pressure (elcity-evaluation-clamp-severity
                       (elcity-evaluation-signals-s6-crime-pressure signals))))

(defun elcity-evaluation--raw-complaint-severity (signals kind)
  "Return raw complaint severity from SIGNALS for KIND."
  (cl-check-type signals elcity-evaluation-signals)
  (elcity-evaluation-check-complaint-kind kind)
  (pcase kind
    ('power
     (elcity-evaluation-signals-power-unserved signals))
    ('traffic
     (elcity-evaluation-clamp-severity
      (round (+ (* 700 (elcity-evaluation-signals-traffic-shortfall signals))
                (* 300 (elcity-evaluation-signals-traffic-hard-fail signals)))
             1000)))
    ('housing
     (elcity-evaluation-clamp-severity
      (round (+ (* 550 (elcity-evaluation-signals-res-demand-neg signals))
                (* 200 (elcity-evaluation-signals-gates-blocked-ratio signals))
                (* 100 (elcity-evaluation-signals-s6-landvalue-pressure signals))
                (* 150 (elcity-evaluation-signals-s6-pollution-pressure signals)))
             1000)))
    ('jobs
     (elcity-evaluation-clamp-severity
      (round (+ (* 450 (elcity-evaluation-signals-com-demand-neg signals))
                (* 450 (elcity-evaluation-signals-ind-demand-neg signals))
                (* 100 (elcity-evaluation-signals-s6-crime-pressure signals)))
             1000)))
    ('taxes
     (elcity-evaluation-signals-tax-pressure signals))
    ('budget-services
     (elcity-evaluation-clamp-severity
      (round (+ (* 600 (elcity-evaluation-signals-budget-eff-shortfall signals))
                (* 250 (elcity-evaluation-signals-budget-deficit signals))
                (* 150 (elcity-evaluation-signals-budget-runway signals)))
             1000)))))

(defun elcity-evaluation-raw-complaint-severity-alist (signals)
  "Return deterministic raw complaint severity alist from SIGNALS."
  (cl-check-type signals elcity-evaluation-signals)
  (mapcar (lambda (kind)
            (cons kind
                  (elcity-evaluation--raw-complaint-severity signals kind)))
          elcity-evaluation-complaint-kinds))

(defun elcity-evaluation--ema-next (previous raw denominator)
  "Return EMA-updated value from PREVIOUS toward RAW by DENOMINATOR."
  (cl-check-type previous integer)
  (cl-check-type raw integer)
  (cl-check-type denominator integer)
  (when (<= denominator 0)
    (error "DENOMINATOR must be positive: %S" denominator))
  (+ previous
     (round (- raw previous)
            denominator)))

(defun elcity-evaluation-smooth-complaint-severity-alist (previous raw)
  "Return complaint severity alist after S8 complaint EMA smoothing.
PREVIOUS and RAW are complaint-severity alists."
  (cl-check-type previous list)
  (cl-check-type raw list)
  (let ((prev-normalized (elcity-evaluation-normalize-complaint-severity-alist
                          previous))
        (raw-normalized (elcity-evaluation-normalize-complaint-severity-alist
                         raw)))
    (mapcar (lambda (kind)
              (let ((previous-value (cdr (assq kind prev-normalized)))
                    (raw-value (cdr (assq kind raw-normalized))))
                (cons kind
                      (elcity-evaluation-clamp-severity
                       (elcity-evaluation--ema-next
                        previous-value
                        raw-value
                        elcity-evaluation-complaint-ema-denominator)))))
            elcity-evaluation-complaint-kinds)))

(defun elcity-evaluation--kind-priority (kind)
  "Return deterministic tie-break priority index for complaint KIND."
  (or (cl-position kind elcity-evaluation-complaint-kinds)
      (length elcity-evaluation-complaint-kinds)))

(defun elcity-evaluation-ranked-complaints (severity-alist)
  "Return complaint kinds ranked from SEVERITY-ALIST by deterministic tie-break."
  (cl-check-type severity-alist list)
  (let ((entries (copy-sequence
                  (elcity-evaluation-normalize-complaint-severity-alist
                   severity-alist))))
    (mapcar #'car
            (sort entries
                  (lambda (a b)
                    (let ((left (cdr a))
                          (right (cdr b)))
                      (if (/= left right)
                          (> left right)
                        (< (elcity-evaluation--kind-priority (car a))
                           (elcity-evaluation--kind-priority (car b))))))))))

(defun elcity-evaluation-top-complaints (severity-alist)
  "Return deterministic top complaint list from SEVERITY-ALIST."
  (cl-subseq (elcity-evaluation-ranked-complaints severity-alist)
             0
             elcity-evaluation-top-complaints-count))

(defun elcity-evaluation-base-health (severity-alist)
  "Return raw base-health score from smoothed complaint SEVERITY-ALIST."
  (cl-check-type severity-alist list)
  (let ((power (cdr (assq 'power severity-alist)))
        (traffic (cdr (assq 'traffic severity-alist)))
        (housing (cdr (assq 'housing severity-alist)))
        (jobs (cdr (assq 'jobs severity-alist)))
        (budget-services (cdr (assq 'budget-services severity-alist)))
        (taxes (cdr (assq 'taxes severity-alist))))
    (elcity-evaluation-clamp-score
     (round (+ (* elcity-evaluation-weight-base-power
                  (- elcity-evaluation-max power))
               (* elcity-evaluation-weight-base-traffic
                  (- elcity-evaluation-max traffic))
               (* elcity-evaluation-weight-base-housing
                  (- elcity-evaluation-max housing))
               (* elcity-evaluation-weight-base-jobs
                  (- elcity-evaluation-max jobs))
               (* elcity-evaluation-weight-base-budget-services
                  (- elcity-evaluation-max budget-services))
               (* elcity-evaluation-weight-base-taxes
                  (- elcity-evaluation-max taxes)))
            1000))))

(defun elcity-evaluation-complaint-penalty (severity-alist top-complaints)
  "Return complaint penalty from SEVERITY-ALIST using TOP-COMPLAINTS."
  (cl-check-type severity-alist list)
  (cl-check-type top-complaints list)
  (let ((top1 (cdr (assq (nth 0 top-complaints) severity-alist)))
        (top2 (cdr (assq (nth 1 top-complaints) severity-alist)))
        (top3 (cdr (assq (nth 2 top-complaints) severity-alist))))
    (floor (+ top1 top2 top3)
           6)))

(defun elcity-evaluation-raw-score (severity-alist top-complaints)
  "Return raw score from SEVERITY-ALIST and TOP-COMPLAINTS."
  (elcity-evaluation-clamp-score
   (- (elcity-evaluation-base-health severity-alist)
      (elcity-evaluation-complaint-penalty severity-alist top-complaints))))

(defun elcity-evaluation-raw-approval (raw-score severity-alist)
  "Return raw approval from RAW-SCORE and complaint SEVERITY-ALIST."
  (cl-check-type raw-score integer)
  (cl-check-type severity-alist list)
  (let ((taxes (cdr (assq 'taxes severity-alist)))
        (budget-services (cdr (assq 'budget-services severity-alist))))
    (elcity-evaluation-clamp-approval
     (round (+ (* elcity-evaluation-weight-approval-score raw-score)
               (* elcity-evaluation-weight-approval-taxes
                  (- elcity-evaluation-max taxes))
               (* elcity-evaluation-weight-approval-budget-services
                  (- elcity-evaluation-max budget-services)))
            1000))))

(defun elcity-evaluation--smooth-with-cap (previous raw denominator cap)
  "Return smoothed value from PREVIOUS toward RAW using DENOMINATOR and CAP."
  (cl-check-type previous integer)
  (cl-check-type raw integer)
  (cl-check-type denominator integer)
  (cl-check-type cap integer)
  (let* ((ema (elcity-evaluation--ema-next previous raw denominator))
         (minimum (- previous cap))
         (maximum (+ previous cap)))
    (elcity-util-clamp ema minimum maximum)))

(defun elcity-evaluation-advance (memory signals)
  "Return cons cell (NEXT-MEMORY . NEXT-SNAPSHOT) from MEMORY and SIGNALS."
  (cl-check-type memory elcity-evaluation-memory)
  (cl-check-type signals elcity-evaluation-signals)
  (let* ((normalized-signals (elcity-evaluation-signals-normalize signals))
         (raw-severity (elcity-evaluation-raw-complaint-severity-alist
                        normalized-signals))
         (next-severity (elcity-evaluation-smooth-complaint-severity-alist
                         (elcity-evaluation-memory-complaint-severity-alist memory)
                         raw-severity))
         (top-complaints (elcity-evaluation-top-complaints next-severity))
         (raw-score (elcity-evaluation-raw-score next-severity top-complaints))
         (raw-approval (elcity-evaluation-raw-approval raw-score next-severity))
         (score-next (elcity-evaluation-clamp-score
                      (elcity-evaluation--smooth-with-cap
                       (elcity-evaluation-memory-score memory)
                       raw-score
                       elcity-evaluation-score-ema-denominator
                       elcity-evaluation-score-delta-cap)))
         (approval-next (elcity-evaluation-clamp-approval
                         (elcity-evaluation--smooth-with-cap
                          (elcity-evaluation-memory-approval memory)
                          raw-approval
                          elcity-evaluation-approval-ema-denominator
                          elcity-evaluation-approval-delta-cap)))
         (next-memory (elcity-evaluation-memory-create
                       :score score-next
                       :approval approval-next
                       :complaint-severity-alist (copy-tree next-severity)))
         (next-snapshot (elcity-evaluation-snapshot-create
                         :city-score score-next
                         :approval approval-next
                         :top-complaints (copy-sequence top-complaints)
                         :complaint-severity-alist (copy-tree next-severity))))
    (cons next-memory next-snapshot)))

(provide 'elcity-evaluation)

;;; elcity-evaluation.el ends here
