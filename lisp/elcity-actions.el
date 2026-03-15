;;; elcity-actions.el --- S10 action batch contracts -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; Deterministic S10 action schema, validation, and ordering helpers.
;;
;; This module defines canonical per-tick action values consumed by the S0
;; dispatcher.  Validation is strict and intended to fail fast for malformed
;; client-boundary inputs before any state mutation occurs.
;;
;; Value-range clamping is intentionally delegated to downstream map/domain
;; mutators (for example tax-rate and budget-percent clamps).  This module
;; enforces action kind, required keys, and value types for trusted internal
;; test/replay/gameplay producers.

;;; Code:

(require 'cl-lib)
(require 'elcity-budget)

(defconst elcity-actions-kind-priority-order
  '(tile-update tax-rate budget-percent noop)
  "Canonical S10 action kind execution order within one tick.")

(defun elcity-action-tile-update (x y tile)
  "Return canonical tile-update action for X,Y and TILE."
  (cl-check-type x integer)
  (cl-check-type y integer)
  (cl-check-type tile integer)
  (list :kind 'tile-update
        :x x
        :y y
        :tile tile))

(defun elcity-action-tax-rate (value)
  "Return canonical tax-rate action for VALUE percent.
VALUE type is validated here; range handling is delegated downstream."
  (cl-check-type value integer)
  (list :kind 'tax-rate
        :value value))

(defun elcity-action-budget-percent (service percent)
  "Return canonical budget-percent action for SERVICE and PERCENT.
PERCENT type is validated here; range clamping is delegated downstream."
  (elcity-budget-check-kind service)
  (cl-check-type percent integer)
  (list :kind 'budget-percent
        :service service
        :percent percent))

(defun elcity-action-noop ()
  "Return canonical no-op action."
  (list :kind 'noop))

(defun elcity-actions-kind-priority (kind)
  "Return deterministic priority index for action KIND."
  (let ((position (cl-position kind elcity-actions-kind-priority-order)))
    (unless position
      (error "Unknown action kind: %S" kind))
    position))

(defun elcity-actions--require-key (action key index)
  "Return KEY value from ACTION at INDEX or signal malformed-action error."
  (if (plist-member action key)
      (plist-get action key)
    (error "Malformed action at index %d: missing %S in %S"
           index
           key
           action)))

(defun elcity-actions--normalize-one (action index)
  "Return canonical validated ACTION for INDEX in current tick batch."
  (unless (listp action)
    (error "Malformed action at index %d: expected plist, got %S"
           index
           action))
  (let ((kind (elcity-actions--require-key action :kind index)))
    (unless (symbolp kind)
      (error "Malformed action at index %d: :kind must be symbol, got %S"
             index
             kind))
    (pcase kind
      ('tile-update
       (let ((x (elcity-actions--require-key action :x index))
             (y (elcity-actions--require-key action :y index))
             (tile (elcity-actions--require-key action :tile index)))
         (cl-check-type x integer)
         (cl-check-type y integer)
         (cl-check-type tile integer)
         (elcity-action-tile-update x y tile)))
      ('tax-rate
       (let ((value (elcity-actions--require-key action :value index)))
         (cl-check-type value integer)
         (elcity-action-tax-rate value)))
      ('budget-percent
       (let ((service (elcity-actions--require-key action :service index))
             (percent (elcity-actions--require-key action :percent index)))
         (elcity-budget-check-kind service)
         (cl-check-type percent integer)
         (elcity-action-budget-percent service percent)))
      ('noop
       (elcity-action-noop))
      (_
       (error "Malformed action at index %d: unknown :kind %S in %S"
              index
              kind
              action)))))

(defun elcity-actions-normalize-batch (actions)
  "Return canonical validated S10 action batch from ACTIONS.
When ACTIONS is nil, return nil."
  (if (null actions)
      nil
    (cl-check-type actions list)
    (let (result
          (index 0))
      (dolist (action actions (nreverse result))
        (push (elcity-actions--normalize-one action index) result)
        (setq index (1+ index))))))

(defun elcity-actions-order-batch (actions)
  "Return ACTIONS ordered deterministically by S10 kind priority.
Relative order for equal-priority kinds is preserved."
  (cl-check-type actions list)
  (let (tile-updates
        tax-rates
        budget-percents
        noops)
    (dolist (action actions)
      (pcase (plist-get action :kind)
        ('tile-update
         (push action tile-updates))
        ('tax-rate
         (push action tax-rates))
        ('budget-percent
         (push action budget-percents))
        ('noop
         (push action noops))
        (_
         (error "Malformed canonical action batch: unknown :kind in %S" action))))
    (append (nreverse tile-updates)
            (nreverse tax-rates)
            (nreverse budget-percents)
            (nreverse noops))))

(defun elcity-actions-normalize-and-order-batch (actions)
  "Return canonical deterministic action batch from raw ACTIONS."
  (elcity-actions-order-batch
   (elcity-actions-normalize-batch actions)))

(defmacro elcity-action-timeline (&rest entries)
  "Build an action-timeline alist from compact ENTRIES.
Each entry is (TICK (KIND ARGS...) ...) where KIND is a short name
that expands to `elcity-action-KIND'.  Example:

  (elcity-action-timeline
   (20 (tax-rate 0)
       (budget-percent \\='infrastructure 50))
   (45 (tax-rate 20)))

expands to:

  (list (cons 20 (list (elcity-action-tax-rate 0)
                       (elcity-action-budget-percent \\='infrastructure 50)))
        (cons 45 (list (elcity-action-tax-rate 20))))"
  `(list
    ,@(mapcar
       (lambda (entry)
         (let ((tick (car entry))
               (actions (cdr entry)))
           `(cons ,tick
                  (list
                   ,@(mapcar
                      (lambda (action)
                        (let ((kind (car action))
                              (args (cdr action)))
                          `(,(intern (format "elcity-action-%s" kind))
                            ,@args)))
                      actions)))))
       entries)))

(provide 'elcity-actions)

;;; elcity-actions.el ends here
