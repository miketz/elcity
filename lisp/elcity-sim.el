;;; elcity-sim.el --- S0 simulation kernel scaffold -*- lexical-binding: t; -*-
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: games, simulation

;; Copyright (C) 2026

;;; Commentary:

;; Deterministic S0 tick dispatcher and stage shells.
;;
;; `elcity-step' defines the canonical stage order and executes each stage
;; exactly once per tick.  Stage functions keep subsystem boundaries explicit
;; and are expected to return new state values instead of mutating inputs.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'elcity-actions)
(require 'elcity-budget)
(require 'elcity-building)
(require 'elcity-demand)
(require 'elcity-evaluation)
(require 'elcity-fire)
(require 'elcity-state-api)
(require 'elcity-power)
(require 'elcity-quality)
(require 'elcity-state)
(require 'elcity-traffic)
(require 'elcity-world)
(require 'elcity-zones)

(defun elcity-step (state &optional actions)
  "Advance STATE by one simulation tick and return updated state.
ACTIONS is current-tick S10 action batch."
  (thread-first state
                (elcity-stage-infra actions)
                (elcity-stage-zones actions)
                (elcity-stage-fields actions)
                (elcity-stage-economy actions)
                (elcity-stage-fire actions)
                (elcity-stage-evaluation actions)))

(defun elcity-sim--apply-tile-update-actions (state ordered-actions)
  "Return STATE after applying tile-update ORDERED-ACTIONS as one batch."
  (cl-check-type state elcity-state)
  (cl-check-type ordered-actions list)
  (let ((world (elcity-state-world state))
        tile-updates)
    (dolist (action ordered-actions)
      (when (eq (plist-get action :kind) 'tile-update)
        (let ((x (plist-get action :x))
              (y (plist-get action :y))
              (tile (plist-get action :tile)))
          (unless (elcity-world-in-bounds-p world x y)
            (error "Malformed action: tile-update out of bounds (%d,%d) for world %dx%d"
                   x
                   y
                   (elcity-world-map-width world)
                   (elcity-world-map-height world)))
          (push (cons (cons x y) tile)
                tile-updates))))
    (if (null tile-updates)
        state
      ;; World edits are applied as one bounded map API batch.
      (elcity-state-api-apply-tile-updates state (nreverse tile-updates)))))

(defun elcity-sim--apply-policy-actions (state ordered-actions)
  "Return STATE after applying tax/budget ORDERED-ACTIONS."
  (cl-check-type state elcity-state)
  (cl-check-type ordered-actions list)
  (let ((next state)
        (tax-rate-seen nil)
        (tax-rate-value 0)
        percent-overrides)
    (dolist (action ordered-actions)
      (pcase (plist-get action :kind)
        ('tax-rate
         (setq tax-rate-seen t
               tax-rate-value (plist-get action :value)))
        ('budget-percent
         (let* ((service (plist-get action :service))
                (percent (plist-get action :percent))
                (existing (assq service percent-overrides)))
           (if existing
               (setcdr existing percent)
             (push (cons service percent) percent-overrides))))))
    (when tax-rate-seen
      (setq next (elcity-state-api-with-tax-rate next tax-rate-value)))
    ;; Apply percent overrides in canonical S7 service order.
    (dolist (service elcity-budget-service-kinds)
      (let ((entry (assq service percent-overrides)))
        (when entry
          (setq next (elcity-state-api-with-service-budget-percent next
                                                             service
                                                             (cdr entry))))))
    next))

(defun elcity-sim--apply-actions (state actions)
  "Return STATE after deterministic S10 ACTIONS application.
Malformed batch inputs fail during normalization before any state mutation."
  (cl-check-type state elcity-state)
  (let* ((ordered (elcity-actions-normalize-and-order-batch actions))
         (after-tiles (elcity-sim--apply-tile-update-actions state ordered)))
    (elcity-sim--apply-policy-actions after-tiles ordered)))

(defun elcity-stage-infra (state &optional actions)
  "Run infra stage for STATE and return a new state value.
Apply current tick ACTIONS before advancing tick counter."
  (cl-check-type state elcity-state)
  (let* ((action-state (elcity-sim--apply-actions state actions))
         (next (copy-elcity-state action-state)))
    ;; S10 action application is part of infra before stage counters advance.
    ;; Infra owns global time progression for the current tick.
    (setf (elcity-state-tick next)
          (1+ (elcity-state-tick action-state)))
    next))

(defun elcity-sim--compute-traffic-scan (state world)
  "Return next S3 traffic scan for STATE and WORLD.
Routes are always recomputed with congestion gating.
Previous congestion map is passed for continuity."
  (cl-check-type state elcity-state)
  (cl-check-type world elcity-world-map)
  (let ((infra-eff (elcity-sim--demand-relative-effectiveness
                    state 'infrastructure)))
    (elcity-traffic-scan-world world
                               (and (elcity-state-traffic-scan state)
                                    (elcity-traffic-scan-congestion-map
                                     (elcity-state-traffic-scan state)))
                               infra-eff)))

(defun elcity-sim--compute-power-results (state world)
  "Return cons cell of (POWER-SCAN . ZONE-SUMMARY) for STATE and WORLD.
Effective capacity is scaled by infrastructure funding level.
Cache hit requires same world identity and same effective capacity key;
on hit the source scan is skipped entirely."
  (cl-check-type state elcity-state)
  (cl-check-type world elcity-world-map)
  (let* ((cached-scan (elcity-state-power-scan state))
         (infra-eff (elcity-sim--demand-relative-effectiveness
                     state 'infrastructure)))
    ;; Fast path: world unchanged and cached scan present.  Derive
    ;; effective capacity from cached sources (same world = same sources)
    ;; and compare against stored key.
    (if (and (eq world (elcity-state-power-world state))
             cached-scan
             (elcity-state-zone-power-summary state)
             (eql (elcity-power-effective-capacity
                   (elcity-power-total-capacity
                    (elcity-power-scan-sources cached-scan))
                   infra-eff)
                  (elcity-state-power-capacity-key state)))
        (cons cached-scan
              (elcity-state-zone-power-summary state))
      ;; Miss: full source scan + BFS traversal.
      (let* ((sources (elcity-power-collect-sources world))
             (rated (elcity-power-total-capacity sources))
             (effective-cap (elcity-power-effective-capacity
                             rated infra-eff))
             (scan (elcity-power-scan-conductive-network
                    world sources effective-cap))
             (zone-summary (elcity-power-summarize-zones
                            world
                            (elcity-power-scan-grid scan))))
        (cons scan zone-summary)))))

(defun elcity-sim--compute-zone-dynamics-scan (scan-state world)
  "Return deterministic S4 zones scan for SCAN-STATE and WORLD."
  (cl-check-type scan-state elcity-state)
  (cl-check-type world elcity-world-map)
  (let ((gate-provider (lambda (x y _kind)
                         (list :powered (elcity-state-api-powered-tile-p scan-state x y nil)
                               :road-connected (elcity-state-api-zone-road-connected-p
                                                scan-state x y)
                               :destination-reachable (elcity-state-api-zone-destination-reachable-p
                                                       scan-state x y))))
        (demand-provider (lambda (kind)
                           (elcity-state-api-demand-at scan-state kind)))
        (quality-provider (lambda (x y kind)
                            (elcity-state-api-local-quality-at scan-state x y kind)))
        (congestion-provider (lambda (x y)
                               (elcity-state-api-zone-average-congestion-at
                                scan-state
                                x
                                y)))
        (pollution-provider (lambda (x y)
                              (elcity-state-api-local-pollution-at scan-state x y))))
    (if (and (elcity-state-zones-scan scan-state)
             (eq world (elcity-state-zones-world scan-state)))
        (elcity-zones-scan-with-cached-zone-centers
         world
         (elcity-state-zones-scan scan-state)
         (elcity-state-zones-cycle scan-state)
         (elcity-state-seed scan-state)
         gate-provider
         demand-provider
         quality-provider
         congestion-provider
         pollution-provider)
      (elcity-zones-scan-world
       world
       (and (elcity-state-zones-scan scan-state)
            (elcity-zone-dynamics-scan-level-map
             (elcity-state-zones-scan scan-state)))
       (elcity-state-zones-cycle scan-state)
       (elcity-state-seed scan-state)
       gate-provider
       demand-provider
       quality-provider
       congestion-provider
       pollution-provider))))

(defun elcity-stage-zones (state &optional _actions)
  "Run zones stage for STATE and return a new state value.
_ACTIONS is accepted for stage signature compatibility.
S10 action batches are consumed in infra stage."
  (cl-check-type state elcity-state)
  (let* ((world (elcity-state-world state))
         (power-results (elcity-sim--compute-power-results state world))
         (power-scan (car power-results))
         (power-summary (cdr power-results))
         (traffic-scan (elcity-sim--compute-traffic-scan state world))
         (next (copy-elcity-state state)))
    ;; S2/S3 snapshots for this tick are set before S4 gating reads.
    (setf (elcity-state-power-world next) world
          (elcity-state-power-capacity-key next)
          (elcity-power-scan-capacity power-scan)
          (elcity-state-power-scan next) power-scan
          (elcity-state-zone-power-summary next) power-summary
          (elcity-state-traffic-scan next) traffic-scan)
    (let* ((zones-scan (elcity-sim--compute-zone-dynamics-scan
                        next
                        (elcity-state-world next)))
           (tile-updates (elcity-zones-scan-tile-updates
                          (elcity-state-world next)
                          zones-scan)))
      (unless (null tile-updates)
        (setq next (elcity-state-api-apply-tile-updates next tile-updates))
        ;; S4 center-index transitions invalidate snapshot caches via map API.
        ;; Reattach S2/S3 snapshots used for this tick's gate decisions.
        (let ((updated-world (elcity-state-world next)))
          (setf (elcity-state-power-world next) updated-world
                (elcity-state-power-capacity-key next)
                (elcity-power-scan-capacity power-scan)
                (elcity-state-power-scan next) power-scan
                (elcity-state-zone-power-summary next) power-summary
                (elcity-state-traffic-scan next) traffic-scan)))
      (setf (elcity-state-zones-world next) (elcity-state-world next)
            (elcity-state-zones-scan next) zones-scan))
    ;; Zones stage now applies power and traffic gating metrics.
    (setf (elcity-state-zones-cycle next)
          (1+ (elcity-state-zones-cycle state)))
    next))

(defun elcity-sim--compute-quality-snapshot (state fire-effectiveness
                                                  police-effectiveness
                                                  infra-effectiveness
                                                  &optional congestion-map)
  "Return deterministic S6 quality snapshot for STATE.
FIRE-EFFECTIVENESS, POLICE-EFFECTIVENESS, and INFRA-EFFECTIVENESS are
current S7 inputs in permille.  CONGESTION-MAP is optional S3 congestion
tile-field for pollution scaling.  When world identity, all effectiveness
cache keys, and congestion map identity match, reuse published S6 snapshot."
  (cl-check-type state elcity-state)
  (cl-check-type fire-effectiveness integer)
  (cl-check-type police-effectiveness integer)
  (cl-check-type infra-effectiveness integer)
  (let ((world (elcity-state-world state))
        (cached (elcity-state-quality-snapshot state))
        (cached-fire (elcity-state-quality-fire-effectiveness state))
        (cached-police (elcity-state-quality-police-effectiveness state))
        (cached-infra (elcity-state-quality-infra-effectiveness state))
        (cached-congestion (elcity-state-quality-congestion-map state)))
    (if (and cached
             (eq world (elcity-state-quality-world state))
             (integerp cached-fire)
             (integerp cached-police)
             (integerp cached-infra)
             (= fire-effectiveness cached-fire)
             (= police-effectiveness cached-police)
             (= infra-effectiveness cached-infra)
             (or (eq congestion-map cached-congestion)
                 (and congestion-map
                      cached-congestion
                      (= (length (elcity-tile-field-cells congestion-map))
                         (length (elcity-tile-field-cells cached-congestion)))
                      (equal (elcity-tile-field-cells congestion-map)
                             (elcity-tile-field-cells cached-congestion)))))
        cached
      (elcity-quality-compute world
                              (elcity-state-api-zone-level-map state)
                              fire-effectiveness
                              police-effectiveness
                              (elcity-state-api-power-grid state)
                              infra-effectiveness
                              congestion-map))))

(defun elcity-sim--evaluation-signals (state)
  "Return normalized S8 signals aggregated from STATE snapshots."
  (cl-check-type state elcity-state)
  (let* ((zones-total (elcity-state-api-zones-total-count state))
         (blocked (elcity-state-api-traffic-blocked-zones state))
         (no-road (elcity-state-api-traffic-no-road-zones state))
         (gates-blocked-any (elcity-state-api-zones-blocked-any-count state))
         (tax-pressure (elcity-evaluation-normalize-01
                        (- (elcity-state-api-tax-rate state)
                           elcity-demand-tax-rate-min)
                        (- elcity-demand-tax-rate-max
                           elcity-demand-tax-rate-min)))
         (infrastructure-eff (elcity-state-api-service-effectiveness state 'infrastructure))
         (fire-eff (elcity-state-api-service-effectiveness state 'fire))
         (police-eff (elcity-state-api-service-effectiveness state 'police))
         (budget-snap (elcity-state-api-budget-snapshot state))
         (infrastructure-requested
          (elcity-budget-snapshot-infrastructure-requested budget-snap))
         (fire-requested
          (elcity-budget-snapshot-fire-requested budget-snap))
         (police-requested
          (elcity-budget-snapshot-police-requested budget-snap))
         (total-requested
          (elcity-budget-snapshot-total-requested budget-snap))
         (weighted-eff (if (<= total-requested 0)
                           elcity-evaluation-max
                         (round (+ (* infrastructure-eff
                                      infrastructure-requested)
                                   (* fire-eff fire-requested)
                                   (* police-eff police-requested))
                                total-requested)))
         (budget-eff-shortfall (if (<= total-requested 0)
                                   0
                                 (- elcity-evaluation-max weighted-eff)))
         (cash-delta (elcity-state-api-budget-cash-delta state))
         (treasury (elcity-state-api-treasury state)))
    (elcity-evaluation-signals-create
     :power-unserved
     (elcity-evaluation-normalize-shortfall-ratio
      (elcity-state-api-zone-powered-ratio state))
     :traffic-shortfall
     (elcity-evaluation-normalize-shortfall-ratio
      (elcity-state-api-traffic-reachable-ratio state))
     :traffic-hard-fail
     (elcity-evaluation-normalize-01 (+ blocked no-road) zones-total)
     :gates-blocked-ratio
     (elcity-evaluation-normalize-01 gates-blocked-any zones-total)
     :res-demand-neg
     (elcity-evaluation-normalize-negative-demand
      (elcity-state-api-residential-demand state))
     :com-demand-neg
     (elcity-evaluation-normalize-negative-demand
      (elcity-state-api-commercial-demand state))
     :ind-demand-neg
     (elcity-evaluation-normalize-negative-demand
      (elcity-state-api-industrial-demand state))
     :tax-pressure tax-pressure
     :budget-eff-shortfall
     (elcity-evaluation-clamp-severity budget-eff-shortfall)
     :budget-deficit
     (if (<= total-requested 0)
         0
       (elcity-evaluation-normalize-01 (max 0 (- cash-delta)) total-requested))
     :budget-runway
     (if (or (<= total-requested 0)
             (>= treasury total-requested))
         0
       (elcity-evaluation-normalize-01 (- total-requested treasury)
                                        total-requested))
     ;; S8 consumes latest published S6 field aggregates.
     :s6-landvalue-pressure (- elcity-evaluation-max
                               (elcity-state-api-quality-land-value-avg state))
     :s6-pollution-pressure (elcity-state-api-quality-pollution-avg state)
     :s6-crime-pressure (elcity-state-api-quality-crime-avg state))))

(defun elcity-sim--compute-evaluation-results (state)
  "Return cons cell (NEXT-MEMORY . NEXT-SNAPSHOT) for S8 on STATE."
  (cl-check-type state elcity-state)
  (elcity-evaluation-advance (elcity-state-api-evaluation-memory state)
                             (elcity-sim--evaluation-signals state)))

(defun elcity-sim--demand-relative-effectiveness (state kind)
  "Return demand-relative effectiveness for service KIND in STATE.
Computed as min(1000, spent * 1000 / max(1, demand)).  When demand is
zero (no stations/roads), returns 1000 (no penalty)."
  (let ((spent (elcity-state-api-service-spending state kind))
        (demand (elcity-state-api-service-budget-request state kind)))
    (if (<= demand 0)
        1000
      (min 1000 (/ (* spent 1000) demand)))))

(defun elcity-stage-fields (state &optional _actions)
  "Run fields stage for STATE and return a new state value.
_ACTIONS is accepted for stage signature compatibility.
S10 action batches are consumed in infra stage."
  (cl-check-type state elcity-state)
  (let ((next (copy-elcity-state state)))
    ;; Fields stage owns S6 quality publication for the current world snapshot.
    ;; S6 coverage uses demand-relative effectiveness (spent/demand) so that
    ;; reducing funding percent degrades quality even when treasury covers the
    ;; lowered target.  S7 target-relative effectiveness is preserved for S8.
    (let* ((fire-effectiveness (elcity-sim--demand-relative-effectiveness
                                next 'fire))
           (police-effectiveness (elcity-sim--demand-relative-effectiveness
                                  next 'police))
           (infra-effectiveness (elcity-sim--demand-relative-effectiveness
                                 next 'infrastructure))
           (congestion-map (elcity-state-api-traffic-congestion-map next))
           (quality-snapshot (elcity-sim--compute-quality-snapshot
                              next
                              fire-effectiveness
                              police-effectiveness
                              infra-effectiveness
                              congestion-map))
           (world (elcity-state-world next)))
      (setf (elcity-state-quality-world next) world
            (elcity-state-quality-snapshot next) quality-snapshot
            (elcity-state-quality-fire-effectiveness next) fire-effectiveness
            (elcity-state-quality-police-effectiveness next) police-effectiveness
            (elcity-state-quality-infra-effectiveness next) infra-effectiveness
            (elcity-state-quality-congestion-map next) congestion-map))
    ;; Fields stage tracks derived-map update cadence.
    (setf (elcity-state-fields-cycle next)
          (1+ (elcity-state-fields-cycle state)))
    next))

(defun elcity-sim--compute-demand-results (state)
  "Return cons cell of (NEXT-MEMORY . NEXT-SNAPSHOT) for S5 on STATE."
  (cl-check-type state elcity-state)
  (let* ((memory (elcity-state-api-demand-memory state))
         (inputs (elcity-demand-inputs-from-zone-summary
                  (elcity-state-api-zones-summary state)
                  (elcity-state-api-tax-rate state)
                  (elcity-state-api-traffic-congestion-road-avg state))))
    (elcity-demand-advance memory inputs)))

(defun elcity-sim--budget-tax-income-input (state)
  "Return S7 tax-income input consumed by economy stage for STATE.
Tax-income is derived from latest S4 population totals and current tax-rate."
  (cl-check-type state elcity-state)
  (elcity-budget-tax-income-from-zone-summary
   (elcity-state-api-zones-summary state)
   (elcity-state-api-tax-rate state)))

(defun elcity-sim--count-power-sources (world)
  "Return (COAL-COUNT . NUCLEAR-COUNT) for power sources in WORLD.
Delegates detection to `elcity-power-collect-sources' to avoid
duplicating source-index logic."
  (let ((coal 0)
        (nuclear 0))
    (dolist (source (elcity-power-collect-sources world))
      (pcase (elcity-power-source-kind source)
        ('coal (setq coal (1+ coal)))
        ('nuclear (setq nuclear (1+ nuclear)))))
    (cons coal nuclear)))

(defun elcity-sim--compute-service-demand (world)
  "Return `elcity-budget-demand' computed from WORLD tile contents.
Iterates the tile vector once, counting infrastructure tiles
\(roads, powerlines, crossings), service station centers, and parks.
Power plant counts are obtained from the canonical S2 source pipeline.
Each service has a base floor cost that applies regardless of tile count,
closing the zero-infrastructure degenerate strategy."
  (cl-check-type world elcity-world-map)
  (let ((tiles (elcity-world-map-tiles world))
        (infrastructure-count 0)
        (fire-count 0)
        (police-count 0)
        (park-count 0))
    (dotimes (i (length tiles))
      (let ((idx (elcity-tile-index (aref tiles i))))
        ;; Road indices include crossings but exclude powerlines.
        (cond
         ((elcity-traffic-road-index-p idx)
          (setq infrastructure-count (1+ infrastructure-count)))
         ((elcity-traffic-powerline-index-p idx)
          (setq infrastructure-count (1+ infrastructure-count)))
         ((= idx elcity-building-fire-station-center-index)
          (setq fire-count (1+ fire-count)))
         ((= idx elcity-building-police-station-center-index)
          (setq police-count (1+ police-count)))
         ((= idx elcity-building-park-center-index)
          (setq park-count (1+ park-count))))))
    (let ((power-counts (elcity-sim--count-power-sources world)))
      (elcity-budget-demand-create
       :infrastructure-requested
       (elcity-budget-clamp-requested
        (+ elcity-budget-infrastructure-base-cost
           (* infrastructure-count elcity-budget-infrastructure-cost-per-tile)
           (* park-count elcity-budget-park-cost)
           (* (car power-counts) elcity-budget-coal-operational-cost)
           (* (cdr power-counts) elcity-budget-nuclear-operational-cost)))
       :fire-requested
       (elcity-budget-clamp-requested
        (+ elcity-budget-fire-base-cost
           (* fire-count elcity-budget-fire-station-cost)))
       :police-requested
       (elcity-budget-clamp-requested
        (+ elcity-budget-police-base-cost
           (* police-count elcity-budget-police-station-cost)))))))

(defun elcity-sim--compute-budget-snapshot (state demand)
  "Return deterministic S7 budget snapshot for STATE using DEMAND.
Crime-avg from S6 quality snapshot drives collectability attenuation."
  (cl-check-type state elcity-state)
  (cl-check-type demand elcity-budget-demand)
  (elcity-budget-advance (elcity-state-api-treasury state)
                         (elcity-sim--budget-tax-income-input state)
                         demand
                         (elcity-state-api-budget-policy state)
                         (elcity-state-api-quality-crime-avg state)))

(defun elcity-stage-economy (state &optional _actions)
  "Run economy stage for STATE and return a new state value.
_ACTIONS is accepted for stage signature compatibility.
S10 action batches are consumed in infra stage."
  (cl-check-type state elcity-state)
  (let* ((next (copy-elcity-state state))
         (demand-results (elcity-sim--compute-demand-results state)))
    ;; Stage order is explicit: S5 demand is published before S7 budget update.
    (setf (elcity-state-demand-memory next) (car demand-results)
          (elcity-state-demand-snapshot next) (cdr demand-results))
    ;; Compute dynamic service demand from current world state.
    (let* ((dynamic-demand
            (elcity-sim--compute-service-demand (elcity-state-world next)))
           (budget-snapshot
            (elcity-sim--compute-budget-snapshot next dynamic-demand)))
      (setf (elcity-state-budget-demand next) dynamic-demand)
      (setf (elcity-state-budget-snapshot next) budget-snapshot
            (elcity-state-treasury next)
            (elcity-budget-snapshot-treasury budget-snapshot)))
    ;; Economy stage owns S5 demand and S7 budget update cadence.
    (setf (elcity-state-economy-cycle next)
          (1+ (elcity-state-economy-cycle state)))
    next))

(defun elcity-stage-fire (state &optional _actions)
  "Run fire incident stage for STATE and return new state.
_ACTIONS is accepted for stage signature compatibility."
  (cl-check-type state elcity-state)
  (elcity-fire-step state))

(defun elcity-stage-evaluation (state &optional _actions)
  "Run evaluation stage for STATE and return a new state value.
_ACTIONS is accepted for stage signature compatibility.
S10 action batches are consumed in infra stage."
  (cl-check-type state elcity-state)
  (let* ((next (copy-elcity-state state))
         (evaluation-results (elcity-sim--compute-evaluation-results state)))
    ;; S8 evaluation runs every tick and owns snapshot+memory publication.
    (setf (elcity-state-evaluation-memory next) (car evaluation-results)
          (elcity-state-evaluation-snapshot next) (cdr evaluation-results))
    (setf (elcity-state-evaluation-cycle next)
          (1+ (elcity-state-evaluation-cycle state)))
    next))

(provide 'elcity-sim)

;;; elcity-sim.el ends here
