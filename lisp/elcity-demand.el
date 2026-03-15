;;; elcity-demand.el --- S5 demand model primitives -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; Pure S5 helpers for lagged R/C/I demand.
;;
;; This module owns:
;; - demand snapshot/input contracts,
;; - tax-rate contract and clamping,
;; - raw imbalance signal formulas with congestion drag,
;; - deterministic EMA + slew-limited demand updates.

;;; Code:

(require 'cl-lib)
(require 'elcity-util)
(require 'elcity-zones)

(defconst elcity-demand-default-value 150
  "Default initial demand value for each R/C/I kind.")

(defconst elcity-demand-default-tax-rate 7
  "Default tax-rate percent used before full S7 budget integration.")

(defconst elcity-demand-tax-rate-min 0
  "Minimum tax-rate percent accepted by S5.")

(defconst elcity-demand-tax-rate-max 20
  "Maximum tax-rate percent accepted by S5.")

(defconst elcity-demand-ema-alpha-numerator 1
  "EMA alpha numerator used by S5 lag update.")

(defconst elcity-demand-ema-alpha-denominator 8
  "EMA alpha denominator used by S5 lag update.")

(defconst elcity-demand-delta-max 500
  "Maximum absolute per-tick demand change after EMA update.")

(defconst elcity-demand-bias-residential 230
  "Raw-signal baseline bias for residential demand.")

(defconst elcity-demand-bias-commercial 210
  "Raw-signal baseline bias for commercial demand.")

(defconst elcity-demand-bias-industrial 210
  "Raw-signal baseline bias for industrial demand.")

(defconst elcity-demand-imbalance-scale 5
  "Divisor applied to population/jobs imbalance terms in raw signals.")

(defconst elcity-demand-imbalance-weight-residential 40
  "Jobs-minus-pop imbalance multiplier for residential raw signal.")

(defconst elcity-demand-imbalance-weight-commercial 36
  "Population-minus-commercial imbalance multiplier for commercial raw signal.")

(defconst elcity-demand-imbalance-weight-industrial 30
  "Population-minus-industrial imbalance multiplier for industrial raw signal.")

(defconst elcity-demand-tax-weight-residential 16
  "Tax drag multiplier for residential raw signal.")

(defconst elcity-demand-tax-weight-commercial 16
  "Tax drag multiplier for commercial raw signal.")

(defconst elcity-demand-tax-weight-industrial 10
  "Tax drag multiplier for industrial raw signal.")

(defconst elcity-demand-congestion-scale 24
  "Divisor applied to congestion-pressure in drag term.
Normalizes the [0, 240] congestion range so that drag
weight units are comparable to tax weight units.")

(defconst elcity-demand-congestion-weight-residential 12
  "Congestion drag multiplier for residential raw signal.")

(defconst elcity-demand-congestion-weight-commercial 12
  "Congestion drag multiplier for commercial raw signal.")

(defconst elcity-demand-congestion-weight-industrial 8
  "Congestion drag multiplier for industrial raw signal.")

(cl-defstruct (elcity-demand-snapshot
               (:constructor elcity-demand-snapshot-create))
  "S5 demand values for residential/commercial/industrial kinds."
  (residential elcity-demand-default-value :type integer)
  (commercial elcity-demand-default-value :type integer)
  (industrial elcity-demand-default-value :type integer))

(cl-defstruct (elcity-demand-inputs
               (:constructor elcity-demand-inputs-create))
  "S5 macro input bundle sampled by economy stage.
RESIDENTIAL-POP is R population total.
COMMERCIAL-POP and INDUSTRIAL-POP are job-proxy totals.
TAX-RATE is clamped percent used by S5 formulas.
CONGESTION-PRESSURE is city-wide average congestion [0, 240]."
  (residential-pop 0 :type integer)
  (commercial-pop 0 :type integer)
  (industrial-pop 0 :type integer)
  (tax-rate elcity-demand-default-tax-rate :type integer)
  (congestion-pressure 0 :type integer))

(defun elcity-demand-clamp-tax-rate (tax-rate)
  "Return TAX-RATE clamped to accepted S5 percent range."
  (elcity-util-clamp tax-rate
                     elcity-demand-tax-rate-min
                     elcity-demand-tax-rate-max))

(defun elcity-demand-default-snapshot ()
  "Return S5 default demand snapshot used at tick 0."
  (elcity-demand-snapshot-create
   :residential elcity-demand-default-value
   :commercial elcity-demand-default-value
   :industrial elcity-demand-default-value))

(defun elcity-demand-snapshot-at (snapshot kind)
  "Return demand value from SNAPSHOT for KIND."
  (cl-check-type snapshot elcity-demand-snapshot)
  (elcity-zones-check-kind kind)
  (pcase kind
    ('residential (elcity-demand-snapshot-residential snapshot))
    ('commercial (elcity-demand-snapshot-commercial snapshot))
    ('industrial (elcity-demand-snapshot-industrial snapshot))))

(defun elcity-demand-inputs-from-zone-summary (summary tax-rate
                                                       &optional congestion-pressure)
  "Return S5 inputs from S4 zone SUMMARY, TAX-RATE, and CONGESTION-PRESSURE.
CONGESTION-PRESSURE defaults to 0 when omitted."
  (cl-check-type summary elcity-zone-dynamics-summary)
  (cl-check-type tax-rate integer)
  (let* ((residential-pop (elcity-zone-dynamics-summary-residential-pop summary))
         (commercial-pop (elcity-zone-dynamics-summary-commercial-pop summary))
         (industrial-pop (elcity-zone-dynamics-summary-industrial-pop summary)))
    (elcity-demand-inputs-create
     :residential-pop residential-pop
     :commercial-pop commercial-pop
     :industrial-pop industrial-pop
     :tax-rate (elcity-demand-clamp-tax-rate tax-rate)
     :congestion-pressure (or congestion-pressure 0))))

(defun elcity-demand--scaled-round (value numerator denominator)
  "Return rounded VALUE * NUMERATOR / DENOMINATOR."
  (cl-check-type value integer)
  (cl-check-type numerator integer)
  (cl-check-type denominator integer)
  (when (<= denominator 0)
    (error "DENOMINATOR must be positive: %S" denominator))
  (round (* value numerator) denominator))

(defun elcity-demand--imbalance-term (weight numerator)
  "Return rounded imbalance term from WEIGHT * NUMERATOR / scale."
  (cl-check-type weight integer)
  (cl-check-type numerator integer)
  (elcity-demand--scaled-round (* weight numerator)
                               1
                               elcity-demand-imbalance-scale))

(defun elcity-demand--ema-next (previous raw)
  "Return EMA-updated value from PREVIOUS toward RAW."
  (cl-check-type previous integer)
  (cl-check-type raw integer)
  (+ previous
     (elcity-demand--scaled-round (- raw previous)
                                  elcity-demand-ema-alpha-numerator
                                  elcity-demand-ema-alpha-denominator)))

(defun elcity-demand--limit-delta (previous candidate)
  "Return CANDIDATE limited to `elcity-demand-delta-max' from PREVIOUS."
  (cl-check-type previous integer)
  (cl-check-type candidate integer)
  (let ((minimum (- previous elcity-demand-delta-max))
        (maximum (+ previous elcity-demand-delta-max)))
    (elcity-util-clamp candidate minimum maximum)))

(defun elcity-demand--congestion-drag (weight congestion-pressure)
  "Return congestion drag from WEIGHT and CONGESTION-PRESSURE.
Result is non-negative integer; zero when CONGESTION-PRESSURE is zero."
  (cl-check-type weight integer)
  (cl-check-type congestion-pressure integer)
  (/ (* weight (max 0 congestion-pressure))
     elcity-demand-congestion-scale))

(defun elcity-demand-raw-signal (inputs kind)
  "Return unbounded raw signal for KIND from INPUTS."
  (cl-check-type inputs elcity-demand-inputs)
  (elcity-zones-check-kind kind)
  (let* ((residential-pop (elcity-demand-inputs-residential-pop inputs))
         (commercial-pop (elcity-demand-inputs-commercial-pop inputs))
         (industrial-pop (elcity-demand-inputs-industrial-pop inputs))
         (jobs-total (+ commercial-pop industrial-pop))
         (tax-rate (elcity-demand-inputs-tax-rate inputs))
         (congestion (elcity-demand-inputs-congestion-pressure inputs)))
    (pcase kind
      ('residential
       (+ elcity-demand-bias-residential
          (elcity-demand--imbalance-term elcity-demand-imbalance-weight-residential
                                         (- jobs-total residential-pop))
          (- (* elcity-demand-tax-weight-residential tax-rate))
          (- (elcity-demand--congestion-drag
              elcity-demand-congestion-weight-residential congestion))))
      ('commercial
       (+ elcity-demand-bias-commercial
          (elcity-demand--imbalance-term elcity-demand-imbalance-weight-commercial
                                         (- residential-pop commercial-pop))
          (- (* elcity-demand-tax-weight-commercial tax-rate))
          (- (elcity-demand--congestion-drag
              elcity-demand-congestion-weight-commercial congestion))))
      ('industrial
       (+ elcity-demand-bias-industrial
          (elcity-demand--imbalance-term elcity-demand-imbalance-weight-industrial
                                         (- residential-pop industrial-pop))
          (- (* elcity-demand-tax-weight-industrial tax-rate))
          (- (elcity-demand--congestion-drag
              elcity-demand-congestion-weight-industrial congestion)))))))

(defun elcity-demand-next-value (previous-memory raw-signal)
  "Return next bounded demand value from PREVIOUS-MEMORY and RAW-SIGNAL."
  (cl-check-type previous-memory integer)
  (cl-check-type raw-signal integer)
  (let* ((ema (elcity-demand--ema-next previous-memory raw-signal))
         (rate-limited (elcity-demand--limit-delta previous-memory ema)))
    (elcity-zones-clamp-demand rate-limited)))

(defun elcity-demand-advance (memory inputs)
  "Return cons cell (NEXT-MEMORY . NEXT-SNAPSHOT) for current tick.
MEMORY stores previous lagged values.  INPUTS is current macro S5 sample."
  (cl-check-type memory elcity-demand-snapshot)
  (cl-check-type inputs elcity-demand-inputs)
  (let* ((next-residential
          (elcity-demand-next-value
           (elcity-demand-snapshot-residential memory)
           (elcity-demand-raw-signal inputs 'residential)))
         (next-commercial
          (elcity-demand-next-value
           (elcity-demand-snapshot-commercial memory)
           (elcity-demand-raw-signal inputs 'commercial)))
         (next-industrial
          (elcity-demand-next-value
           (elcity-demand-snapshot-industrial memory)
           (elcity-demand-raw-signal inputs 'industrial)))
         (next-memory (elcity-demand-snapshot-create
                       :residential next-residential
                       :commercial next-commercial
                       :industrial next-industrial))
         (next-snapshot (copy-elcity-demand-snapshot next-memory)))
    (cons next-memory next-snapshot)))

(provide 'elcity-demand)

;;; elcity-demand.el ends here
