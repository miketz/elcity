;;; elcity-fire.el --- Fire incident simulation -*- lexical-binding: t; -*-
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: games, simulation

;; Copyright (C) 2026

;;; Commentary:

;; Deterministic fire simulation: ignition, burn lifecycle, and demolition.
;;
;; Each tick the fire stage:
;; 1. Decrements active burn timers.
;; 2. Demolishes expired burns (full footprint + neighbor retile).
;; 3. On cadence ticks (every 3 months), rolls new ignitions using
;;    coverage-modulated probability.
;; 4. Increments fire-cycle.

;;; Code:

(require 'cl-lib)
(require 'elcity-building)
(require 'elcity-state)
(require 'elcity-state-api)
(require 'elcity-tile)
(require 'elcity-tile-field)
(require 'elcity-time)
(require 'elcity-world)

;;; Constants

(defconst elcity-fire-base-rate 5
  "Base ignition probability per check per eligible tile (out of 1000).
At zero coverage, each eligible tile has a 5/1000 = 0.5% chance of
igniting per ignition check.")

(defconst elcity-fire-burn-duration 8
  "Number of ticks a tile burns before expiring to empty ground.")

(defconst elcity-fire-salt 314159265
  "Deterministic salt for fire roll hash, distinct from zones salt.")

(defconst elcity-fire-ignition-cadence (* 3 elcity-time-ticks-per-month)
  "Fire-cycle interval between ignition checks (every 3 in-game months).
Ignition rolls occur when fire-cycle > 0 and fire-cycle is a multiple
of this value; the first check is at fire-cycle = cadence.
Burn timer processing runs every tick regardless.")

;;; Deterministic roll

(defun elcity-fire-deterministic-roll (seed fire-cycle x y)
  "Return deterministic integer roll in [0, 1000) for fire ignition.
Uses the same LCG-XOR hash pattern as `elcity-zones-deterministic-roll'
but with a distinct fire salt to avoid correlation.
SEED is the world seed, FIRE-CYCLE is the fire stage counter,
X and Y are tile coordinates."
  (let* ((n (+ (* 1103515245 (logand seed #xffffffff))
               (* 12345 (1+ fire-cycle))
               (* 2654435761 (1+ x))
               (* 2246822519 (1+ y))
               elcity-fire-salt)))
    (setq n (logand n #xffffffff))
    (setq n (logxor n (ash n -16)))
    (mod n 1000)))

;;; Ignition probability

(defun elcity-fire-ignition-probability (coverage)
  "Return ignition probability in [0, 1000] from COVERAGE value [0, 1000].
Linear formula: base-rate * (1000 - coverage) / 1000.
At zero coverage returns `elcity-fire-base-rate'.  At full coverage (1000)
returns 0."
  (/ (* elcity-fire-base-rate (- 1000 (min 1000 (max 0 coverage)))) 1000))

;;; Tile eligibility

(defun elcity-fire-tile-eligible-p (world x y)
  "Return non-nil when tile at X,Y in WORLD is eligible for fire ignition.
Eligible means: in-bounds and has burnbit set."
  (and (elcity-world-in-bounds-p world x y)
       (elcity-tile-flammable-p (elcity-world-tile-at world x y 0))))

;;; Internal helpers

(defun elcity-fire--decrement-timers (timers)
  "Decrement all burn TIMERS by 1 and return (ACTIVE . EXPIRED).
ACTIVE is a new hash table with decremented entries (timer > 0 after
decrement).  EXPIRED is a list of (X . Y) coordinates that reached 0."
  (let ((active (make-hash-table :test 'equal))
        expired)
    (when timers
      (maphash (lambda (coord remaining)
                 (let ((new-val (1- remaining)))
                   (if (<= new-val 0)
                       (push coord expired)
                     (puthash coord new-val active))))
               timers))
    (cons active expired)))

(defun elcity-fire--demolish-expired (state expired-coords active-timers)
  "Return (STATE . ACTIVE-TIMERS) after demolishing EXPIRED-COORDS.
Detects full building footprints, clears to empty ground, retiles
cardinal neighbors, and removes footprint entries from ACTIVE-TIMERS."
  (if (null expired-coords)
      (cons state active-timers)
    (let ((world (elcity-state-world state))
          (demolished (make-hash-table :test 'equal))
          clear-updates)
      ;; Collect all footprint tiles to clear.
      (dolist (coord expired-coords)
        (unless (gethash coord demolished)
          (let ((footprint (elcity-building--detect-footprint
                            world (car coord) (cdr coord))))
            (dolist (fc footprint)
              (unless (gethash fc demolished)
                (puthash fc t demolished)
                (push (cons fc elcity-building-empty-index) clear-updates)
                ;; Remove from active timers too.
                (remhash fc active-timers))))))
      ;; Apply clear updates to get intermediate state.
      (let ((mid-state (if clear-updates
                           (elcity-state-api-apply-tile-updates
                            state (nreverse clear-updates))
                         state)))
        ;; Compute retile actions for cardinal neighbors of demolished tiles.
        (let (retile-updates
              (visited (make-hash-table :test 'equal)))
          (maphash (lambda (coord _)
                     (dolist (delta elcity-cardinal-neighbor-deltas)
                       (let ((nc (cons (+ (car coord) (car delta))
                                       (+ (cdr coord) (cdr delta)))))
                         (unless (or (gethash nc demolished)
                                     (gethash nc visited))
                           (puthash nc t visited)
                           (let ((action (elcity-building--retile-road-neighbor
                                          mid-state (car nc) (cdr nc))))
                             (when action
                               (push (cons (cons (plist-get action :x)
                                                 (plist-get action :y))
                                           (plist-get action :tile))
                                     retile-updates)))
                           (let ((action (elcity-building--retile-powerline-neighbor
                                          mid-state (car nc) (cdr nc))))
                             (when action
                               (push (cons (cons (plist-get action :x)
                                                 (plist-get action :y))
                                           (plist-get action :tile))
                                     retile-updates)))))))
                   demolished)
          (let ((final-state (if retile-updates
                                 (elcity-state-api-apply-tile-updates
                                  mid-state retile-updates)
                               mid-state)))
            (cons final-state active-timers)))))))

(defun elcity-fire--roll-ignitions (state active-timers coverage-map)
  "Return updated ACTIVE-TIMERS after rolling ignitions on STATE.
Scans all world tiles in row-major order.  For eligible non-burning
tiles, rolls deterministic ignition and expands to full footprint.
COVERAGE-MAP is the S6 fire-coverage tile field captured before
any demolition mutations in this tick."
  (let* ((world (elcity-state-world state))
         (width (elcity-world-map-width world))
         (height (elcity-world-map-height world))
         (seed (elcity-state-seed state))
         (fire-cycle (elcity-state-fire-cycle state)))
    (let ((y 0))
      (while (< y height)
        (let ((x 0))
          (while (< x width)
            (when (and (elcity-fire-tile-eligible-p world x y)
                       (not (gethash (cons x y) active-timers)))
              (let* ((coverage (if coverage-map
                                   (elcity-tile-field-ref coverage-map x y 0)
                                 0))
                     (prob (elcity-fire-ignition-probability coverage))
                     (roll (elcity-fire-deterministic-roll
                            seed fire-cycle x y)))
                (when (< roll prob)
                  ;; Ignite: expand to full footprint.
                  (let ((footprint (elcity-building--detect-footprint
                                    world x y)))
                    (dolist (fc footprint)
                      (unless (gethash fc active-timers)
                        (puthash fc elcity-fire-burn-duration
                                 active-timers)))))))
            (setq x (1+ x))))
        (setq y (1+ y)))))
  active-timers)

;;; Main fire step

(defun elcity-fire-step (state)
  "Run one fire simulation tick on STATE and return updated state.
Lifecycle order:
1. Decrement all active burn timers.
2. Collect expired entries (timer reached 0).
3. For each expired entry, detect full building footprint and demolish
   (clear to empty ground index 0) with cardinal neighbor retiling.
4. On cadence ticks (fire-cycle > 0 and fire-cycle mod
   ignition-cadence = 0), roll ignition for all eligible
   non-burning tiles using S6
   fire-coverage-map and deterministic roll.
5. Start new burns with configured duration for ignited tiles;
   when an ignited tile belongs to a 3x3 building, burn the entire
   footprint.
6. Increment fire-cycle.
7. Return new state with updated world, fire-burn-timers, and
   fire-cycle."
  (cl-check-type state elcity-state)
  (let* ((timers (elcity-state-fire-burn-timers state))
         ;; Capture S6 coverage map before demolition may invalidate it.
         ;; Demolition calls apply-tile-updates which clears all S2-S6
         ;; snapshots; ignition rolls must use pre-demolition coverage.
         (coverage-map (elcity-state-api-quality-fire-coverage-map state))
         ;; Phase 1-2: decrement and collect expired.
         (result (elcity-fire--decrement-timers timers))
         (active-timers (car result))
         (expired (cdr result))
         ;; Phase 3: demolish expired footprints.
         (demo-result (elcity-fire--demolish-expired
                       state expired active-timers))
         (post-demo-state (car demo-result))
         (active-timers (cdr demo-result))
         ;; Phase 4-5: roll ignitions with footprint expansion.
         ;; Ignition checks run on cadence ticks (every 3 months),
         ;; starting after the first full interval has elapsed.
         (active-timers (if (and (> (elcity-state-fire-cycle state) 0)
                                 (= 0 (mod (elcity-state-fire-cycle state)
                                           elcity-fire-ignition-cadence)))
                            (elcity-fire--roll-ignitions
                             post-demo-state active-timers coverage-map)
                          active-timers))
         ;; Phase 6-7: update state.
         (next (copy-elcity-state post-demo-state)))
    ;; Restore S2-S6 snapshots from pre-fire state so that evaluation
    ;; (running immediately after fire) reads this tick's computed
    ;; snapshots rather than falling back to defaults.
    (elcity-state-api--copy-zones-snapshots next state)
    (setf (elcity-state-fire-burn-timers next) active-timers)
    (setf (elcity-state-fire-cycle next)
          (1+ (elcity-state-fire-cycle state)))
    next))

(provide 'elcity-fire)

;;; elcity-fire.el ends here
