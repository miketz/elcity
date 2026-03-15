;;; elcity-state.el --- Core simulation state -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; Canonical simulation state container for the functional core.
;;
;; State owns:
;; - scheduler counters (S0),
;; - authoritative world tiles (S1),
;; - S2 power scan and zone power-summary snapshots,
;; - S3 traffic route outcomes and congestion-memory snapshots,
;; - S4 zone level/population scan snapshots,
;; - S5 demand snapshots, lag memory, and tax-rate input,
;; - S6 quality field snapshot and cache keys,
;; - S7 treasury, budget policy, and service-effectiveness snapshots.
;; - fire-cycle and fire-burn-timers (authoritative fire incident state).
;; - S8 evaluation snapshot and lag memory.
;;
;; Subsystems should treat state as immutable input and return a copied state
;; with explicit field updates.

;;; Code:

(require 'cl-lib)
(require 'elcity-budget)
(require 'elcity-demand)
(require 'elcity-evaluation)
(require 'elcity-power)
(require 'elcity-quality)
(require 'elcity-terrain)
(require 'elcity-traffic)
(require 'elcity-world)
(require 'elcity-zones)

(cl-defstruct (elcity-state
               (:constructor elcity-state-create))
  "Simulation state value for the ElCity kernel.
Field meaning:
- TICK is absolute simulation time.
- ZONES-CYCLE/FIELDS-CYCLE/ECONOMY-CYCLE/EVALUATION-CYCLE are per-stage
  cadence counters.
- WORLD is authoritative tile-resolution map data.
- POWER-WORLD/POWER-CAPACITY-KEY/POWER-SCAN/ZONE-POWER-SUMMARY are S2 cached
  outputs.  POWER-CAPACITY-KEY stores the effective capacity used for the scan.
- TRAFFIC-SCAN is the latest S3 output (routes recomputed each zones tick;
  congestion map reused as continuity input).
- ZONES-WORLD/ZONES-SCAN are S4 cached outputs.
- DEMAND-SNAPSHOT is published S5 demand consumed by other stages.
- DEMAND-MEMORY is internal S5 lag memory used for next update.
- TAX-RATE is policy input consumed by S5.
- QUALITY-WORLD/QUALITY-SNAPSHOT are published S6 fields keyed by world identity.
- QUALITY-FIRE-EFFECTIVENESS/QUALITY-POLICE-EFFECTIVENESS are S6 cache keys for
  service-coupled coverage inputs consumed when QUALITY-SNAPSHOT was computed.
- FIRE-CYCLE is per-stage cadence counter incremented each tick by the fire stage.
- FIRE-BURN-TIMERS is authoritative sparse hash table (X . Y) -> remaining-ticks.
  nil means no active burns.  Only tiles currently burning have entries.
- TREASURY is available city cash clamped to S7 treasury bounds.
- BUDGET-DEMAND stores absolute S7 service demand derived from city state.
- BUDGET-POLICY stores per-service S7 funding percent policy.
- BUDGET-SNAPSHOT is published S7 budget/effectiveness output.
- EVALUATION-SNAPSHOT is published S8 score/approval/complaint output.
- EVALUATION-MEMORY is internal S8 lag memory used for next update.
- SEED is deterministic replay seed.

Core state invariants:
1. WORLD is authoritative.  All stage snapshots are derived.
2. Snapshot cache validity is keyed by world identity (`eq`) and funding:
   POWER-SCAN is valid only for POWER-WORLD and POWER-CAPACITY-KEY,
   ZONES-SCAN is valid only for ZONES-WORLD,
   QUALITY-SNAPSHOT is valid only for QUALITY-WORLD plus
   QUALITY-FIRE-EFFECTIVENESS/QUALITY-POLICE-EFFECTIVENESS/
   QUALITY-INFRA-EFFECTIVENESS.
   TRAFFIC-SCAN route outcomes are recomputed every zones tick; only its
   congestion map may be reused as continuity input for the next S3 pass.
   Any world replacement must invalidate caches whose world key no longer
   matches.
3. Stage ownership is explicit:
   infra owns TICK,
   zones owns ZONES-CYCLE and S2/S3/S4 snapshots,
   fields owns FIELDS-CYCLE and S6 quality fields,
   economy owns ECONOMY-CYCLE and S5/S7 fields,
   fire owns FIRE-CYCLE and FIRE-BURN-TIMERS,
   evaluation owns EVALUATION-CYCLE and S8 fields.
4. DEMAND-SNAPSHOT and DEMAND-MEMORY are separate snapshots by design:
   snapshot is externally consumed output, memory is lag state for next tick.
5. Tax and demand values are normalized through map/demand helpers before
   downstream consumption.
6. S7 policy and snapshot values are normalized through map/budget helpers
   before downstream consumption.
7. S8 snapshot and memory values are normalized through map/evaluation helpers
   before downstream consumption.
8. State values are treated as immutable by convention: stages return copied
   states instead of mutating input state in place."
  ;; S0 scheduler counters.
  (tick 0 :type integer)
  (zones-cycle 0 :type integer)
  (fields-cycle 0 :type integer)
  (economy-cycle 0 :type integer)
  (evaluation-cycle 0 :type integer)
  ;; S1 authoritative map.
  (world nil :type (or null elcity-world-map))
  ;; S2 cache: keyed by POWER-WORLD identity and POWER-CAPACITY-KEY.
  (power-world nil :type (or null elcity-world-map))
  (power-capacity-key nil :type (or null integer))
  (power-scan nil :type (or null elcity-power-scan))
  (zone-power-summary nil :type (or null elcity-zone-power-summary))
  ;; S3 traffic scan (recomputed every tick; no world-identity cache).
  (traffic-scan nil :type (or null elcity-traffic-scan))
  ;; S4 cache: keyed by ZONES-WORLD identity.
  (zones-world nil :type (or null elcity-world-map))
  (zones-scan nil :type (or null elcity-zone-dynamics-scan))
  ;; S5 published snapshot and internal lag memory.
  (demand-snapshot nil :type (or null elcity-demand-snapshot))
  (demand-memory nil :type (or null elcity-demand-snapshot))
  (tax-rate elcity-demand-default-tax-rate :type integer)
  ;; S6 cache: keyed by QUALITY-WORLD identity.
  (quality-world nil :type (or null elcity-world-map))
  (quality-snapshot nil :type (or null elcity-quality-snapshot))
  (quality-fire-effectiveness nil :type (or null integer))
  (quality-police-effectiveness nil :type (or null integer))
  (quality-infra-effectiveness nil :type (or null integer))
  (quality-congestion-map nil :type (or null elcity-tile-field))
  ;; S7 treasury, service demand, funding policy, and published snapshot.
  (treasury elcity-budget-default-treasury :type integer)
  (budget-demand nil :type (or null elcity-budget-demand))
  (budget-policy nil :type (or null elcity-budget-policy))
  (budget-snapshot nil :type (or null elcity-budget-snapshot))
  ;; Fire incident state (authoritative, between S7 and S8).
  (fire-cycle 0 :type integer)
  (fire-burn-timers nil :type (or null hash-table))
  ;; S8 published snapshot and internal lag memory.
  (evaluation-snapshot nil :type (or null elcity-evaluation-snapshot))
  (evaluation-memory nil :type (or null elcity-evaluation-memory))
  (seed 0 :type integer))

(defun elcity-make-initial-state (&optional seed width height initial-tile)
  "Create initial simulation state.
SEED defaults to 0.  WIDTH and HEIGHT default to
`elcity-world-default-width' and `elcity-world-default-height'.  INITIAL-TILE
defaults to 0.  When INITIAL-TILE is nil (omitted), terrain generation stamps
a quarter-circle water body; when explicitly provided (including 0), terrain
generation is skipped for stable test/scenario maps.
The returned state includes a world map and deterministic S6/S7/S8 defaults."
  (let* ((effective-seed (or seed 0))
         (world (elcity-world-map-make
                 (or width elcity-world-default-width)
                 (or height elcity-world-default-height)
                 (or initial-tile 0)))
         ;; Stamp terrain when initial-tile was not explicitly provided.
         (world (if initial-tile
                    world
                  (elcity-terrain-stamp-water world effective-seed)))
         (budget-demand (elcity-budget-demand-create
                        :infrastructure-requested 0
                        :fire-requested 0
                        :police-requested 0))
         (budget-policy (elcity-budget-default-policy))
         (budget-snapshot (elcity-budget-default-snapshot
                           elcity-budget-default-treasury
                           budget-demand
                           budget-policy)))
    (elcity-state-create :tick 0
                         :seed effective-seed
                         :world world
                         ;; Tick-0 zones pass should match pre-S5 behavior.
                         :demand-snapshot (elcity-demand-default-snapshot)
                         :demand-memory (elcity-demand-default-snapshot)
                         :tax-rate elcity-demand-default-tax-rate
                         :quality-world world
                         :quality-snapshot (elcity-quality-default-snapshot
                                            world)
                         ;; Keep bootstrap effect keys nil so first fields tick
                         ;; recomputes quality from real world sources.
                         :quality-fire-effectiveness nil
                         :quality-police-effectiveness nil
                         :treasury elcity-budget-default-treasury
                         :budget-demand budget-demand
                         :budget-policy budget-policy
                         :budget-snapshot budget-snapshot
                         :evaluation-snapshot
                         (elcity-evaluation-default-snapshot)
                         :evaluation-memory
                         (elcity-evaluation-default-memory))))

(provide 'elcity-state)

;;; elcity-state.el ends here
