;;; elcity-state-api.el --- Simulation-facing state accessors -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; Simulation-facing API for S1 world access.
;;
;; Downstream systems should use this module instead of mutating state/world
;; structures directly.  The API centralizes:
;; - reading world tiles,
;; - applying tile updates,
;; - exposing S2 power snapshots and zone-level power metrics,
;; - exposing S3 traffic route/congestion snapshots and gate metrics,
;; - exposing S4 zone level/population snapshots and transition metrics,
;; - exposing S5 demand snapshots and tax-rate policy input,
;; - exposing S6 quality snapshots and local-quality projections,
;; - exposing S7 budget snapshot, treasury, and service policy accessors.
;; - exposing S8 evaluation snapshot and complaint/score accessors.

;;; Code:

(require 'cl-lib)
(require 'elcity-budget)
(require 'elcity-demand)
(require 'elcity-evaluation)
(require 'elcity-power)
(require 'elcity-quality)
(require 'elcity-state)
(require 'elcity-tile-field)
(require 'elcity-traffic)
(require 'elcity-world)
(require 'elcity-zones)

(defun elcity-state-api-world (state)
  "Return the authoritative world map from STATE."
  (cl-check-type state elcity-state)
  (elcity-state-world state))

(defun elcity-state-api-tile-at (state x y &optional default)
  "Return tile at X,Y from STATE world map, or DEFAULT when out of bounds."
  (cl-check-type state elcity-state)
  (elcity-world-tile-at (elcity-state-api-world state) x y default))

(defun elcity-state-api--empty-power-scan (state)
  "Return default empty S2 power scan for STATE world."
  (cl-check-type state elcity-state)
  (elcity-power-scan-create
   :grid (elcity-power-grid-for-world (elcity-state-world state) nil)
   :sources nil
   :capacity 0
   :powered-tiles 0
   :overloaded nil))

(defun elcity-state-api--empty-zone-power-summary ()
  "Return default zone-power summary used before the first S2 zones pass."
  (elcity-zone-power-summary-create
   :powered 0
   :unpowered 0
   :ratio 1.0))

(defun elcity-state-api--empty-traffic-scan (state)
  "Return default empty S3 traffic scan for STATE world."
  (cl-check-type state elcity-state)
  (elcity-traffic-empty-scan-for-world (elcity-state-world state)))

(defun elcity-state-api--empty-zones-scan (state)
  "Return default empty S4 zones scan for STATE world."
  (cl-check-type state elcity-state)
  (elcity-zones-empty-scan-for-world (elcity-state-world state)))

(defun elcity-state-api--default-demand-snapshot ()
  "Return default S5 demand snapshot used before first economy pass."
  (elcity-demand-default-snapshot))

(defun elcity-state-api--default-quality-snapshot (state)
  "Return default S6 quality snapshot sized for STATE world."
  (cl-check-type state elcity-state)
  (elcity-quality-default-snapshot (elcity-state-world state)))

(defun elcity-state-api--default-budget-demand ()
  "Return default S7 budget demand used before first economy pass."
  (elcity-budget-default-demand))

(defun elcity-state-api--default-budget-policy ()
  "Return default S7 budget policy used before first economy pass."
  (elcity-budget-default-policy))

(defun elcity-state-api--default-evaluation-snapshot ()
  "Return default S8 snapshot used before first evaluation pass."
  (elcity-evaluation-default-snapshot))

(defun elcity-state-api--default-evaluation-memory ()
  "Return default S8 lag memory used before first evaluation pass."
  (elcity-evaluation-default-memory))

(defun elcity-state-api-power-scan (state)
  "Return latest S2 power scan from STATE, or deterministic empty scan."
  (cl-check-type state elcity-state)
  (or (elcity-state-power-scan state)
      (elcity-state-api--empty-power-scan state)))

(defun elcity-state-api-zone-power-summary (state)
  "Return latest S2 zone power summary from STATE, or default summary."
  (cl-check-type state elcity-state)
  (or (elcity-state-zone-power-summary state)
      (elcity-state-api--empty-zone-power-summary)))

(defun elcity-state-api-power-grid (state)
  "Return tile-resolution power grid from STATE."
  (cl-check-type state elcity-state)
  (elcity-power-scan-grid (elcity-state-api-power-scan state)))

(defun elcity-state-api-power-capacity (state)
  "Return generation capacity budget from latest S2 scan in STATE."
  (cl-check-type state elcity-state)
  (elcity-power-scan-capacity (elcity-state-api-power-scan state)))

(defun elcity-state-api-powered-tiles (state)
  "Return number of powered conductive tiles from latest S2 scan in STATE."
  (cl-check-type state elcity-state)
  (elcity-power-scan-powered-tiles (elcity-state-api-power-scan state)))

(defun elcity-state-api-power-overloaded-p (state)
  "Return non-nil when latest S2 scan in STATE exceeded capacity."
  (cl-check-type state elcity-state)
  (elcity-power-scan-overloaded (elcity-state-api-power-scan state)))

(defun elcity-state-api-powered-tile-p (state x y &optional default)
  "Return non-nil when tile X,Y is powered in STATE.
Return DEFAULT when X,Y is out of bounds."
  (cl-check-type state elcity-state)
  (elcity-power-grid-cell-at (elcity-state-api-power-grid state)
                             x
                             y
                             default))

(defun elcity-state-api-powered-zones (state)
  "Return powered zone-center count from latest S2 zone summary in STATE."
  (cl-check-type state elcity-state)
  (elcity-zone-power-summary-powered (elcity-state-api-zone-power-summary state)))

(defun elcity-state-api-unpowered-zones (state)
  "Return unpowered zone-center count from latest S2 zone summary in STATE."
  (cl-check-type state elcity-state)
  (elcity-zone-power-summary-unpowered (elcity-state-api-zone-power-summary state)))

(defun elcity-state-api-zone-powered-ratio (state)
  "Return powered-zone ratio from latest S2 zone summary in STATE."
  (cl-check-type state elcity-state)
  (elcity-zone-power-summary-ratio (elcity-state-api-zone-power-summary state)))

(defun elcity-state-api-zones-power-penalty (state)
  "Return derived power penalty from latest S2 zone summary in STATE."
  (cl-check-type state elcity-state)
  (elcity-power-zone-penalty (elcity-state-api-zone-power-summary state)))

(defun elcity-state-api-traffic-scan (state)
  "Return latest S3 traffic scan from STATE, or deterministic empty scan."
  (cl-check-type state elcity-state)
  (or (elcity-state-traffic-scan state)
      (elcity-state-api--empty-traffic-scan state)))

(defun elcity-state-api-traffic-summary (state)
  "Return route-outcome summary from latest S3 traffic scan in STATE."
  (cl-check-type state elcity-state)
  (elcity-traffic-scan-summary (elcity-state-api-traffic-scan state)))

(defun elcity-state-api-traffic-congestion-map (state)
  "Return tile-resolution congestion memory map from latest S3 scan in STATE."
  (cl-check-type state elcity-state)
  (elcity-traffic-scan-congestion-map (elcity-state-api-traffic-scan state)))

(defun elcity-state-api-traffic-route-results (state)
  "Return deterministic route-result entries from latest S3 scan in STATE."
  (cl-check-type state elcity-state)
  (elcity-traffic-scan-results (elcity-state-api-traffic-scan state)))

(defun elcity-state-api-traffic-reachable-zones (state)
  "Return reachable route count from latest S3 traffic summary in STATE."
  (cl-check-type state elcity-state)
  (elcity-traffic-summary-reachable (elcity-state-api-traffic-summary state)))

(defun elcity-state-api-traffic-blocked-zones (state)
  "Return blocked route count from latest S3 traffic summary in STATE."
  (cl-check-type state elcity-state)
  (elcity-traffic-summary-blocked (elcity-state-api-traffic-summary state)))

(defun elcity-state-api-traffic-no-road-zones (state)
  "Return no-road route count from latest S3 traffic summary in STATE."
  (cl-check-type state elcity-state)
  (elcity-traffic-summary-no-road (elcity-state-api-traffic-summary state)))

(defun elcity-state-api-traffic-reachable-ratio (state)
  "Return reachable ratio from latest S3 traffic summary in STATE."
  (cl-check-type state elcity-state)
  (elcity-traffic-summary-ratio (elcity-state-api-traffic-summary state)))

(defun elcity-state-api-traffic-congestion-sum (state)
  "Return sum of all congestion-memory tile cells from latest S3 scan in STATE."
  (cl-check-type state elcity-state)
  (elcity-traffic-congestion-map-sum (elcity-state-api-traffic-congestion-map state)))


(defun elcity-state-api-traffic-effective-congestion-cap (state)
  "Return infra-adjusted S3 congestion block threshold for STATE.
Defaults to hard cap when infra effectiveness is not yet published."
  (cl-check-type state elcity-state)
  (elcity-traffic-effective-congestion-cap
   (elcity-state-quality-infra-effectiveness state)))

(defun elcity-state-api-traffic-congestion-at (state x y)
  "Return S3 congestion value at tile X,Y in STATE."
  (cl-check-type state elcity-state)
  (let ((cmap (elcity-state-api-traffic-congestion-map state)))
    (elcity-tile-field-ref cmap x y 0)))

(defun elcity-state-api-zone-bottleneck-congestion-at (state center-x center-y)
  "Return max S3 congestion in STATE on perimeter roads near CENTER-X,CENTER-Y.
Returns 0 when no perimeter roads are present."
  (cl-check-type state elcity-state)
  (cl-check-type center-x integer)
  (cl-check-type center-y integer)
  (elcity-traffic-zone-bottleneck-congestion
   (elcity-state-api-world state)
   (elcity-state-api-traffic-congestion-map state)
   center-x center-y))

(defun elcity-state-api-zone-average-congestion-at (state center-x center-y)
  "Return average S3 congestion on perimeter roads near CENTER-X,CENTER-Y.
Average is rounded up to nearest integer so low non-zero pressure remains
visible in player-facing UI.  Returns 0 when no perimeter roads are present
in STATE."
  (cl-check-type state elcity-state)
  (cl-check-type center-x integer)
  (cl-check-type center-y integer)
  (elcity-traffic-zone-average-congestion
   (elcity-state-api-world state)
   (elcity-state-api-traffic-congestion-map state)
   center-x center-y))

(defun elcity-state-api-traffic-congestion-road-avg (state)
  "Return average S3 congestion over road-active tiles in STATE.
Only drivable road/crossing tiles are counted.  Returns 0 when none exist."
  (cl-check-type state elcity-state)
  (elcity-traffic-congestion-road-avg
   (elcity-state-api-world state)
   (elcity-state-api-traffic-congestion-map state)))

(defun elcity-state-api-traffic-congestion-cap-ratio (state)
  "Return fraction of active congestion cells that are capped in STATE.
Returns 0.0 when no cells are active.  Value is a float in [0.0, 1.0]."
  (cl-check-type state elcity-state)
  (plist-get (elcity-traffic-congestion-distribution-stats
              (elcity-state-api-traffic-congestion-map state))
             :cap-ratio))

(defun elcity-state-api-traffic-congestion-mid-ratio (state)
  "Return fraction of active congestion cells in the mid range in STATE.
Mid-range cells have 0 < value < cap.  Returns 0.0 when no cells are active.
Value is a float in [0.0, 1.0]."
  (cl-check-type state elcity-state)
  (plist-get (elcity-traffic-congestion-distribution-stats
              (elcity-state-api-traffic-congestion-map state))
             :mid-ratio))

(defun elcity-state-api-zones-traffic-penalty (state)
  "Return derived traffic penalty from latest S3 traffic summary in STATE."
  (cl-check-type state elcity-state)
  (elcity-traffic-zone-penalty (elcity-state-api-traffic-summary state)))

(defun elcity-state-api-quality-snapshot (state)
  "Return latest S6 quality snapshot from STATE, or deterministic default."
  (cl-check-type state elcity-state)
  (or (elcity-state-quality-snapshot state)
      (elcity-state-api--default-quality-snapshot state)))

(defun elcity-state-api-quality-pollution-map (state)
  "Return S6 pollution map from latest quality snapshot in STATE."
  (cl-check-type state elcity-state)
  (elcity-quality-snapshot-pollution-map (elcity-state-api-quality-snapshot state)))

(defun elcity-state-api-quality-land-value-map (state)
  "Return S6 land-value map from latest quality snapshot in STATE."
  (cl-check-type state elcity-state)
  (elcity-quality-snapshot-land-value-map (elcity-state-api-quality-snapshot state)))

(defun elcity-state-api-quality-crime-map (state)
  "Return S6 crime map from latest quality snapshot in STATE."
  (cl-check-type state elcity-state)
  (elcity-quality-snapshot-crime-map (elcity-state-api-quality-snapshot state)))

(defun elcity-state-api-quality-pollution-avg (state)
  "Return S6 average pollution scalar from latest snapshot in STATE."
  (cl-check-type state elcity-state)
  (elcity-quality-snapshot-pollution-avg (elcity-state-api-quality-snapshot state)))

(defun elcity-state-api-quality-land-value-avg (state)
  "Return S6 average land-value scalar from latest snapshot in STATE."
  (cl-check-type state elcity-state)
  (elcity-quality-snapshot-land-value-avg (elcity-state-api-quality-snapshot state)))

(defun elcity-state-api-quality-crime-avg (state)
  "Return S6 population-weighted crime scalar from latest snapshot in STATE."
  (cl-check-type state elcity-state)
  (elcity-quality-snapshot-crime-avg (elcity-state-api-quality-snapshot state)))

(defun elcity-state-api-quality-fire-coverage-map (state)
  "Return S6 fire-coverage tile field map from latest snapshot in STATE.
May be nil before first S6 tick or when loaded from older saves."
  (cl-check-type state elcity-state)
  (elcity-quality-snapshot-fire-coverage-map
   (elcity-state-api-quality-snapshot state)))

(defun elcity-state-api-quality-police-coverage-map (state)
  "Return S6 police-coverage tile field map from latest snapshot in STATE.
May be nil before first S6 tick or when loaded from older saves."
  (cl-check-type state elcity-state)
  (elcity-quality-snapshot-police-coverage-map
   (elcity-state-api-quality-snapshot state)))

(defun elcity-state-api-local-pollution-at (state x y)
  "Return S6 local pollution value at tile X,Y in STATE.
Returns 0 when out of bounds or before S6 fields exist."
  (cl-check-type state elcity-state)
  (cl-check-type x integer)
  (cl-check-type y integer)
  (if (not (elcity-world-in-bounds-p (elcity-state-api-world state) x y))
      0
    (let* ((snapshot (elcity-state-api-quality-snapshot state))
           (pollution-map (elcity-quality-snapshot-pollution-map snapshot)))
      (elcity-tile-field-ref pollution-map x y 0))))

(defun elcity-state-api-local-quality-at (state x y kind)
  "Return S4 local-quality projection in STATE for tile X,Y and KIND.
Projection contract:
  residential: clamp(land-value - pollution - crime, -1000, 1000)
  commercial: clamp(land-value - crime, -1000, 1000)
  industrial: 0
KIND is validated against supported S4 zone kinds."
  (cl-check-type state elcity-state)
  (elcity-quality-local-quality-at
   (elcity-state-api-quality-snapshot state)
   (elcity-state-api-world state)
   x y kind))

(defun elcity-state-api-demand-snapshot (state)
  "Return latest S5 demand snapshot from STATE, or default snapshot."
  (cl-check-type state elcity-state)
  (or (elcity-state-demand-snapshot state)
      (elcity-state-api--default-demand-snapshot)))

(defun elcity-state-api-demand-memory (state)
  "Return latest S5 lag-memory snapshot from STATE, or default snapshot."
  (cl-check-type state elcity-state)
  (or (elcity-state-demand-memory state)
      (elcity-state-api--default-demand-snapshot)))

(defun elcity-state-api-demand-at (state kind)
  "Return S5 demand value for KIND from STATE."
  (cl-check-type state elcity-state)
  (elcity-demand-snapshot-at (elcity-state-api-demand-snapshot state)
                             kind))

(defun elcity-state-api-residential-demand (state)
  "Return residential S5 demand from STATE."
  (cl-check-type state elcity-state)
  (elcity-state-api-demand-at state 'residential))

(defun elcity-state-api-commercial-demand (state)
  "Return commercial S5 demand from STATE."
  (cl-check-type state elcity-state)
  (elcity-state-api-demand-at state 'commercial))

(defun elcity-state-api-industrial-demand (state)
  "Return industrial S5 demand from STATE."
  (cl-check-type state elcity-state)
  (elcity-state-api-demand-at state 'industrial))

(defun elcity-state-api-tax-rate (state)
  "Return tax-rate percent consumed by S5 in STATE."
  (cl-check-type state elcity-state)
  (elcity-demand-clamp-tax-rate (elcity-state-tax-rate state)))

(defun elcity-state-api-budget-demand (state)
  "Return S7 service demand from STATE, or deterministic default."
  (cl-check-type state elcity-state)
  (or (elcity-state-budget-demand state)
      (elcity-state-api--default-budget-demand)))

(defun elcity-state-api-budget-policy (state)
  "Return S7 funding percent policy from STATE, or deterministic default."
  (cl-check-type state elcity-state)
  (or (elcity-state-budget-policy state)
      (elcity-state-api--default-budget-policy)))

(defun elcity-state-api-treasury (state)
  "Return clamped S7 treasury from STATE."
  (cl-check-type state elcity-state)
  (elcity-budget-clamp-treasury (elcity-state-treasury state)))

(defun elcity-state-api-budget-snapshot (state)
  "Return latest S7 budget snapshot from STATE, or default full-effect snapshot."
  (cl-check-type state elcity-state)
  (or (elcity-state-budget-snapshot state)
      (elcity-budget-default-snapshot (elcity-state-api-treasury state)
                                      (elcity-state-api-budget-demand state)
                                      (elcity-state-api-budget-policy state))))

(defun elcity-state-api-budget-cash-delta (state)
  "Return S7 cash delta from latest budget snapshot in STATE."
  (cl-check-type state elcity-state)
  (elcity-budget-snapshot-cash-delta (elcity-state-api-budget-snapshot state)))

(defun elcity-state-api-construction-spent (state)
  "Return S12 construction spending from latest budget snapshot in STATE."
  (cl-check-type state elcity-state)
  (elcity-budget-snapshot-construction-spent (elcity-state-api-budget-snapshot state)))

(defun elcity-state-api-budget-tax-income (state)
  "Return S7 tax-income input from latest budget snapshot in STATE."
  (cl-check-type state elcity-state)
  (elcity-budget-snapshot-tax-income (elcity-state-api-budget-snapshot state)))

(defun elcity-state-api-budget-collectability (state)
  "Return S7 collectability permille from latest budget snapshot in STATE."
  (cl-check-type state elcity-state)
  (elcity-budget-snapshot-collectability (elcity-state-api-budget-snapshot state)))

(defun elcity-state-api-budget-gross-tax-income (state)
  "Return S7 gross tax income before collectability from latest snapshot in STATE."
  (cl-check-type state elcity-state)
  (elcity-budget-snapshot-gross-tax-income (elcity-state-api-budget-snapshot state)))

(defun elcity-state-api-budget-total-requested (state)
  "Return total requested S7 service budget from latest snapshot in STATE."
  (cl-check-type state elcity-state)
  (elcity-budget-snapshot-total-requested (elcity-state-api-budget-snapshot state)))

(defun elcity-state-api-budget-total-target (state)
  "Return total target S7 service budget from latest snapshot in STATE."
  (cl-check-type state elcity-state)
  (elcity-budget-snapshot-total-target (elcity-state-api-budget-snapshot state)))

(defun elcity-state-api-budget-total-spent (state)
  "Return total spent S7 service budget from latest snapshot in STATE."
  (cl-check-type state elcity-state)
  (elcity-budget-snapshot-total-spent (elcity-state-api-budget-snapshot state)))

(defun elcity-state-api-service-budget-request (state kind)
  "Return requested S7 service demand in STATE for service KIND."
  (cl-check-type state elcity-state)
  (elcity-budget-demand-requested-at (elcity-state-api-budget-demand state)
                                     kind))

(defun elcity-state-api-service-budget-percent (state kind)
  "Return funding percent for service KIND in STATE."
  (cl-check-type state elcity-state)
  (elcity-budget-policy-percent-at (elcity-state-api-budget-policy state)
                                   kind))

(defun elcity-state-api-service-budget-target (state kind)
  "Return target S7 service budget in STATE for service KIND."
  (cl-check-type state elcity-state)
  (elcity-budget-snapshot-target-at (elcity-state-api-budget-snapshot state)
                                    kind))

(defun elcity-state-api-service-spending (state kind)
  "Return spent S7 budget in STATE for service KIND."
  (cl-check-type state elcity-state)
  (elcity-budget-snapshot-spent-at (elcity-state-api-budget-snapshot state)
                                   kind))

(defun elcity-state-api-service-effectiveness (state kind)
  "Return S7 effectiveness permille in STATE for service KIND."
  (cl-check-type state elcity-state)
  (elcity-budget-snapshot-effectiveness-at (elcity-state-api-budget-snapshot state)
                                           kind))

(defun elcity-state-api-evaluation-snapshot (state)
  "Return latest S8 evaluation snapshot from STATE, or deterministic default."
  (cl-check-type state elcity-state)
  (or (elcity-state-evaluation-snapshot state)
      (elcity-state-api--default-evaluation-snapshot)))

(defun elcity-state-api-evaluation-memory (state)
  "Return latest S8 lag memory from STATE, or deterministic default."
  (cl-check-type state elcity-state)
  (or (elcity-state-evaluation-memory state)
      (elcity-state-api--default-evaluation-memory)))

(defun elcity-state-api-city-score (state)
  "Return published S8 city score from STATE."
  (cl-check-type state elcity-state)
  (elcity-evaluation-snapshot-city-score (elcity-state-api-evaluation-snapshot state)))

(defun elcity-state-api-approval (state)
  "Return published S8 approval proxy from STATE."
  (cl-check-type state elcity-state)
  (elcity-evaluation-snapshot-approval (elcity-state-api-evaluation-snapshot state)))

(defun elcity-state-api-top-complaints (state)
  "Return published S8 top complaint symbols from STATE."
  (cl-check-type state elcity-state)
  (elcity-evaluation-snapshot-top-complaints (elcity-state-api-evaluation-snapshot state)))

(defun elcity-state-api-complaint-severity (state kind)
  "Return published S8 complaint severity from STATE for KIND."
  (cl-check-type state elcity-state)
  (elcity-evaluation-snapshot-complaint-severity-at
   (elcity-state-api-evaluation-snapshot state)
   kind))

(defun elcity-state-api-zone-traffic-status-at (state x y &optional default)
  "Return S3 route-status at zone center X,Y in STATE, or DEFAULT."
  (cl-check-type state elcity-state)
  (cl-check-type x integer)
  (cl-check-type y integer)
  (elcity-traffic-route-status-at (elcity-state-api-traffic-scan state)
                                  x
                                  y
                                  default))

(defun elcity-state-api-zone-traffic-accessible-p (state x y)
  "Return non-nil when zone center X,Y is traffic-accessible in STATE."
  (cl-check-type state elcity-state)
  (cl-check-type x integer)
  (cl-check-type y integer)
  (elcity-traffic-zone-accessible-p
   (elcity-state-api-zone-traffic-status-at state x y 'no-road)))

(defun elcity-state-api-zone-road-connected-p (state x y)
  "Return non-nil when zone center X,Y has local road adjacency in STATE.
True for `reachable' and `blocked' route status; nil for `no-road'."
  (cl-check-type state elcity-state)
  (cl-check-type x integer)
  (cl-check-type y integer)
  (elcity-traffic-zone-road-connected-p
   (elcity-state-api-zone-traffic-status-at state x y 'no-road)))

(defun elcity-state-api-zone-destination-reachable-p (state x y)
  "Return non-nil when zone center X,Y has a reachable destination in STATE.
True only for `reachable' route status."
  (cl-check-type state elcity-state)
  (cl-check-type x integer)
  (cl-check-type y integer)
  (elcity-traffic-zone-destination-reachable-p
   (elcity-state-api-zone-traffic-status-at state x y 'no-road)))

(defun elcity-state-api-zones-scan (state)
  "Return latest S4 zones scan from STATE, or deterministic empty scan."
  (cl-check-type state elcity-state)
  (or (elcity-state-zones-scan state)
      (elcity-state-api--empty-zones-scan state)))

(defun elcity-state-api-zone-level-map (state)
  "Return S4 zone-level map from latest zones scan in STATE."
  (cl-check-type state elcity-state)
  (elcity-zone-dynamics-scan-level-map (elcity-state-api-zones-scan state)))

(defun elcity-state-api-zone-level-at (state x y &optional default)
  "Return S4 zone level at X,Y in STATE, or DEFAULT when out of bounds."
  (cl-check-type state elcity-state)
  (cl-check-type x integer)
  (cl-check-type y integer)
  (elcity-zones-level-at (elcity-state-api-zone-level-map state) x y default))

(defun elcity-state-api-zones-summary (state)
  "Return S4 zone-dynamics summary from latest zones scan in STATE."
  (cl-check-type state elcity-state)
  (elcity-zone-dynamics-scan-summary (elcity-state-api-zones-scan state)))

(defun elcity-state-api-zones-transitions (state)
  "Return S4 transition entries from latest zones scan in STATE."
  (cl-check-type state elcity-state)
  (elcity-zone-dynamics-scan-transitions (elcity-state-api-zones-scan state)))

(defun elcity-state-api-residential-population (state)
  "Return residential population aggregate from latest S4 summary in STATE."
  (cl-check-type state elcity-state)
  (elcity-zone-dynamics-summary-residential-pop (elcity-state-api-zones-summary state)))

(defun elcity-state-api-commercial-population (state)
  "Return commercial population aggregate from latest S4 summary in STATE."
  (cl-check-type state elcity-state)
  (elcity-zone-dynamics-summary-commercial-pop (elcity-state-api-zones-summary state)))

(defun elcity-state-api-industrial-population (state)
  "Return industrial population aggregate from latest S4 summary in STATE."
  (cl-check-type state elcity-state)
  (elcity-zone-dynamics-summary-industrial-pop (elcity-state-api-zones-summary state)))

(defun elcity-state-api-total-population (state)
  "Return total R/C/I population aggregate from latest S4 summary in STATE."
  (cl-check-type state elcity-state)
  (+ (elcity-state-api-residential-population state)
     (elcity-state-api-commercial-population state)
     (elcity-state-api-industrial-population state)))

(defun elcity-state-api-zones-grown-count (state)
  "Return count of S4 growth transitions in latest summary for STATE."
  (cl-check-type state elcity-state)
  (elcity-zone-dynamics-summary-grown (elcity-state-api-zones-summary state)))

(defun elcity-state-api-zones-declined-count (state)
  "Return count of S4 decline transitions in latest summary for STATE."
  (cl-check-type state elcity-state)
  (elcity-zone-dynamics-summary-declined (elcity-state-api-zones-summary state)))

(defun elcity-state-api-zones-held-count (state)
  "Return count of S4 hold outcomes in latest summary for STATE."
  (cl-check-type state elcity-state)
  (elcity-zone-dynamics-summary-held (elcity-state-api-zones-summary state)))

(defun elcity-state-api-zones-blocked-power-count (state)
  "Return count of S4 evaluated zones blocked by missing power gate in STATE."
  (cl-check-type state elcity-state)
  (elcity-zone-dynamics-summary-blocked-power (elcity-state-api-zones-summary state)))

(defun elcity-state-api-zones-blocked-traffic-count (state)
  "Return count of S4 evaluated zones blocked by missing traffic gate in STATE."
  (cl-check-type state elcity-state)
  (elcity-zone-dynamics-summary-blocked-traffic (elcity-state-api-zones-summary state)))

(defun elcity-state-api-zones-blocked-both-count (state)
  "Return count of S4 evaluated zones blocked by both gates in STATE."
  (cl-check-type state elcity-state)
  (elcity-zone-dynamics-summary-blocked-both (elcity-state-api-zones-summary state)))

(defun elcity-state-api-zones-blocked-pollution-count (state)
  "Return count of S4 residential zones blocked by high local pollution in STATE."
  (cl-check-type state elcity-state)
  (elcity-zone-dynamics-summary-blocked-pollution (elcity-state-api-zones-summary state)))

(defun elcity-state-api-zones-blocked-any-count (state)
  "Return count of S4 evaluated zones blocked by any gate in STATE.
Union of power, traffic, and pollution gate failures without double-counting."
  (cl-check-type state elcity-state)
  (elcity-zone-dynamics-summary-blocked-any (elcity-state-api-zones-summary state)))

(defun elcity-state-api-zones-evaluated-count (state)
  "Return count of cadence-eligible S4 zone centers evaluated in STATE."
  (cl-check-type state elcity-state)
  (elcity-zone-dynamics-summary-evaluated (elcity-state-api-zones-summary state)))

(defun elcity-state-api-zones-total-count (state)
  "Return total zone-center count scanned by latest S4 pass in STATE."
  (cl-check-type state elcity-state)
  (elcity-zone-dynamics-summary-total-zones (elcity-state-api-zones-summary state)))

(defun elcity-state-api--copy-zones-snapshots (dst src)
  "Copy cached S2-S6 snapshot fields from SRC state into DST state.
Returns DST.  Used to restore snapshots after mid-pipeline tile mutations."
  (setf (elcity-state-power-world dst) (elcity-state-power-world src)
        (elcity-state-power-capacity-key dst) (elcity-state-power-capacity-key src)
        (elcity-state-power-scan dst) (elcity-state-power-scan src)
        (elcity-state-zone-power-summary dst) (elcity-state-zone-power-summary src)
        (elcity-state-traffic-scan dst) (elcity-state-traffic-scan src)
        (elcity-state-zones-world dst) (elcity-state-zones-world src)
        (elcity-state-zones-scan dst) (elcity-state-zones-scan src)
        (elcity-state-quality-world dst) (elcity-state-quality-world src)
        (elcity-state-quality-snapshot dst) (elcity-state-quality-snapshot src)
        (elcity-state-quality-fire-effectiveness dst) (elcity-state-quality-fire-effectiveness src)
        (elcity-state-quality-police-effectiveness dst) (elcity-state-quality-police-effectiveness src)
        (elcity-state-quality-infra-effectiveness dst) (elcity-state-quality-infra-effectiveness src)
        (elcity-state-quality-congestion-map dst) (elcity-state-quality-congestion-map src))
  dst)

(defun elcity-state-api--invalidate-zones-snapshots (state)
  "Clear cached zones-stage snapshots in STATE and return STATE.
This invalidates S2 power, S3 traffic, S4 zones scan, and S6 quality fields."
  (cl-check-type state elcity-state)
  (setf (elcity-state-power-world state) nil
        (elcity-state-power-capacity-key state) nil
        (elcity-state-power-scan state) nil
        (elcity-state-zone-power-summary state) nil
        (elcity-state-traffic-scan state) nil
        (elcity-state-zones-world state) nil
        (elcity-state-zones-scan state) nil
        (elcity-state-quality-world state) nil
        (elcity-state-quality-snapshot state) nil
        (elcity-state-quality-fire-effectiveness state) nil
        (elcity-state-quality-police-effectiveness state) nil
        (elcity-state-quality-infra-effectiveness state) nil
        (elcity-state-quality-congestion-map state) nil)
  state)

(defun elcity-state-api-with-quality-results (state snapshot)
  "Return STATE copied with published S6 quality SNAPSHOT applied."
  (cl-check-type state elcity-state)
  (cl-check-type snapshot elcity-quality-snapshot)
  (let ((next (copy-elcity-state state)))
    (setf (elcity-state-quality-world next) (elcity-state-world state)
          (elcity-state-quality-snapshot next) snapshot
          ;; Caller-provided snapshots may not encode service-effectiveness
          ;; provenance; force conservative recompute on future fields stage.
          (elcity-state-quality-fire-effectiveness next) nil
          (elcity-state-quality-police-effectiveness next) nil)
    next))

(defun elcity-state-api-with-demand-results (state snapshot memory)
  "Return STATE copied with S5 demand SNAPSHOT and lag MEMORY applied."
  (cl-check-type state elcity-state)
  (cl-check-type snapshot elcity-demand-snapshot)
  (cl-check-type memory elcity-demand-snapshot)
  (let ((next (copy-elcity-state state)))
    (setf (elcity-state-demand-snapshot next) snapshot
          (elcity-state-demand-memory next) memory)
    next))

(defun elcity-state-api-with-budget-results (state treasury snapshot)
  "Return STATE copied with S7 TREASURY and budget SNAPSHOT applied."
  (cl-check-type state elcity-state)
  (cl-check-type treasury integer)
  (cl-check-type snapshot elcity-budget-snapshot)
  (let ((next (copy-elcity-state state)))
    (setf (elcity-state-treasury next)
          (elcity-budget-clamp-treasury treasury)
          (elcity-state-budget-snapshot next) snapshot)
    next))

(defun elcity-state-api-with-budget-snapshot (state snapshot)
  "Return STATE copied with budget SNAPSHOT applied.
Does not touch treasury.  Used by S12 to update construction-spent."
  (cl-check-type state elcity-state)
  (cl-check-type snapshot elcity-budget-snapshot)
  (let ((next (copy-elcity-state state)))
    (setf (elcity-state-budget-snapshot next) snapshot)
    next))

(defun elcity-state-api-with-treasury (state treasury)
  "Return STATE copied with TREASURY applied (clamped).
Does not touch budget snapshot.  Used by S12 construction deductions."
  (cl-check-type state elcity-state)
  (cl-check-type treasury integer)
  (let ((next (copy-elcity-state state)))
    (setf (elcity-state-treasury next)
          (elcity-budget-clamp-treasury treasury))
    next))

(defun elcity-state-api-with-evaluation-results (state snapshot memory)
  "Return STATE copied with S8 evaluation SNAPSHOT and lag MEMORY applied."
  (cl-check-type state elcity-state)
  (cl-check-type snapshot elcity-evaluation-snapshot)
  (cl-check-type memory elcity-evaluation-memory)
  (let ((next (copy-elcity-state state)))
    (setf (elcity-state-evaluation-snapshot next) snapshot
          (elcity-state-evaluation-memory next) memory)
    next))

(defun elcity-state-api-with-budget-demand (state demand)
  "Return STATE copied with S7 service DEMAND applied.
Applying demand invalidates published budget snapshot so fallback contracts are
deterministic until next explicit budget publish."
  (cl-check-type state elcity-state)
  (cl-check-type demand elcity-budget-demand)
  (let ((next (copy-elcity-state state)))
    (setf (elcity-state-budget-demand next)
          (copy-elcity-budget-demand demand)
          (elcity-state-budget-snapshot next) nil)
    next))

(defun elcity-state-api-with-budget-policy (state policy)
  "Return STATE copied with S7 funding percent POLICY applied.
Applying policy invalidates published budget snapshot so fallback contracts are
deterministic until next explicit budget publish."
  (cl-check-type state elcity-state)
  (cl-check-type policy elcity-budget-policy)
  (let ((next (copy-elcity-state state)))
    (setf (elcity-state-budget-policy next)
          (copy-elcity-budget-policy policy)
          (elcity-state-budget-snapshot next) nil)
    next))

(defun elcity-state-api-with-service-budget-percent (state kind percent)
  "Return STATE copied with KIND service funding PERCENT applied."
  (cl-check-type state elcity-state)
  (cl-check-type percent integer)
  (elcity-budget-check-kind kind)
  (elcity-state-api-with-budget-policy
   state
   (elcity-budget-policy-with-percent (elcity-state-api-budget-policy state)
                                      kind
                                      percent)))

(defun elcity-state-api-with-tax-rate (state tax-rate)
  "Return STATE copied with TAX-RATE percent applied."
  (cl-check-type state elcity-state)
  (cl-check-type tax-rate integer)
  (let ((next (copy-elcity-state state)))
    (setf (elcity-state-tax-rate next)
          (elcity-demand-clamp-tax-rate tax-rate))
    next))

(defun elcity-state-api-with-world (state world)
  "Return STATE with WORLD applied.
Use this when replacing the entire world map, not sparse tile deltas."
  (cl-check-type state elcity-state)
  (cl-check-type world elcity-world-map)
  (let ((next (copy-elcity-state state)))
    (setf (elcity-state-world next) world)
    ;; Invalidate world-derived snapshots; they are recomputed from WORLD.
    (elcity-state-api--invalidate-zones-snapshots next)))

(defun elcity-state-api--check-tile-updates (tile-updates)
  "Validate TILE-UPDATES format.
Each update must be ((X . Y) . TILE) with integer X, Y, and TILE."
  (cl-check-type tile-updates list)
  (dolist (update tile-updates)
    (unless (and (consp update)
                 (consp (car update))
                 (integerp (caar update))
                 (integerp (cdar update))
                 (integerp (cdr update)))
      (error "Invalid tile update; expected ((X . Y) . TILE): %S" update))))

(defun elcity-state-api-apply-tile-updates (state tile-updates)
  "Return STATE with TILE-UPDATES applied.
TILE-UPDATES is a list of ((X . Y) . TILE) updates.
Updates are applied in list order."
  (cl-check-type state elcity-state)
  (elcity-state-api--check-tile-updates tile-updates)
  (if (null tile-updates)
      state
    (let* ((world (elcity-world-with-tiles (elcity-state-world state)
                                           tile-updates))
           (next (copy-elcity-state state)))
      (setf (elcity-state-world next) world)
      ;; Invalidate world-derived snapshots; downstream stages recompute.
      (elcity-state-api--invalidate-zones-snapshots next))))

(defun elcity-state-api-fire-cycle (state)
  "Return fire stage cycle counter from STATE."
  (cl-check-type state elcity-state)
  (elcity-state-fire-cycle state))

(defun elcity-state-api-fire-burn-timers (state)
  "Return fire burn timers hash table from STATE, or nil."
  (cl-check-type state elcity-state)
  (elcity-state-fire-burn-timers state))

(defun elcity-state-api-fire-burning-p (state x y)
  "Return non-nil when tile at X,Y in STATE is currently burning."
  (cl-check-type state elcity-state)
  (cl-check-type x integer)
  (cl-check-type y integer)
  (let ((timers (elcity-state-fire-burn-timers state)))
    (and timers (gethash (cons x y) timers))))

(defun elcity-state-api-fire-burn-remaining (state x y)
  "Return remaining burn ticks for tile at X,Y in STATE, or 0 if not burning."
  (cl-check-type state elcity-state)
  (cl-check-type x integer)
  (cl-check-type y integer)
  (let ((timers (elcity-state-fire-burn-timers state)))
    (if timers
        (or (gethash (cons x y) timers) 0)
      0)))

(defun elcity-state-api-fire-active-count (state)
  "Return number of actively burning tiles in STATE."
  (cl-check-type state elcity-state)
  (let ((timers (elcity-state-fire-burn-timers state)))
    (if timers (hash-table-count timers) 0)))

(defun elcity-state-api-with-fire-burn-timers (state timers)
  "Return STATE copied with TIMERS as the fire burn timers."
  (cl-check-type state elcity-state)
  (when timers
    (cl-check-type timers hash-table))
  (let ((next (copy-elcity-state state)))
    (setf (elcity-state-fire-burn-timers next) timers)
    next))

(provide 'elcity-state-api)

;;; elcity-state-api.el ends here
