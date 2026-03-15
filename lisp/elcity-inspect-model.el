;;; elcity-inspect-model.el --- Shared per-tile inspection data model -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; Pure data module for per-tile inspection.
;;
;; Provides a single structured tile-info record and a builder function
;; that derives all fields from existing map/domain accessors.  Player
;; and debug tooltip formatters consume this struct without duplicating
;; tile classification or status derivation logic.
;;
;; This module must not depend on player or debug-render modules.
;;
;; Public API:
;;   (elcity-inspect-tile-info-at state x y power-scan connected-grid)
;;     -> elcity-inspect-tile-info struct
;;
;;   (elcity-inspect-explain-name info)    -> string
;;   (elcity-inspect-explain-effect info)  -> string
;;   (elcity-inspect-explain-params info)  -> string or nil
;;   (elcity-inspect-explain-quality info) -> string
;;   (elcity-inspect-explain-improve info) -> string or nil

;;; Code:

(require 'cl-lib)
(require 'elcity-state-api)
(require 'elcity-power)
(require 'elcity-quality)
(require 'elcity-state)
(require 'elcity-tile)
(require 'elcity-tile-field)
(require 'elcity-traffic)
(require 'elcity-world)
(require 'elcity-zones)

;;; ---------- Data contract ----------

(cl-defstruct (elcity-inspect-tile-info
               (:constructor elcity-inspect-tile-info--create)
               (:copier nil))
  "Per-tile inspection data record."
  ;; Identity
  (x 0 :type integer)
  (y 0 :type integer)
  (tile-index 0 :type integer)
  ;; Context geometry
  (context-kind nil)
  (context-center-x nil)
  (context-center-y nil)
  (footprint-role nil)
  ;; Zone context (nil for non-zones)
  (zone-kind nil)
  (zone-level nil)
  (zone-population nil)
  ;; Building context (nil for non-buildings)
  (structure-kind nil)
  (structure-name nil)
  ;; Gates
  (powered-p nil)
  (power-status nil)
  (traffic-accessible-p nil)
  (traffic-status nil)
  ;; S6 quality at tile
  (pollution nil)
  (land-value nil)
  (crime nil)
  (local-quality nil)
  ;; S5 demand for zone kind (nil for non-zones)
  (demand nil)
  ;; S3 congestion at tile cell
  (congestion nil)
  ;; S3 effective congestion cap (block threshold from infra-effectiveness)
  (congestion-effective-cap elcity-traffic-congestion-cap :type integer)
  ;; S3 max congestion among perimeter roads (zones only)
  (zone-bottleneck-congestion nil)
  ;; S3 mean congestion among perimeter roads (zones only)
  (zone-average-congestion nil)
  ;; Fire state
  (burning-p nil)
  (fire-timer nil))

;;; ---------- Building context specs ----------

(defconst elcity-inspect--building-specs
  `((,elcity-power-nuclear-plant-index 4 2 "Nuclear Plant" power-plant)
    (,elcity-power-coal-plant-index 3 1 "Coal Plant" power-plant)
    (,elcity-quality-fire-station-center-index 3 1 "Fire Station" service)
    (,elcity-quality-police-station-center-index 3 1 "Police Station" service)
    (,elcity-quality-park-center-index 1 0 "Park" amenity))
  "Building inspection specs.
Each entry is (CENTER-INDEX SIZE RADIUS NAME STRUCTURE-KIND).")

;;; ---------- Internal helpers ----------

(defun elcity-inspect--building-footprint-contains-p
    (center-x center-y size center-offset x y)
  "Return non-nil when tile X,Y belongs to SIZE stamp at CENTER-X,CENTER-Y.
CENTER-OFFSET is the center's offset from the top-left anchor."
  (let* ((left (- center-x center-offset))
         (top (- center-y center-offset))
         (right (+ left (1- size)))
         (bottom (+ top (1- size))))
    (and (<= left x) (<= x right)
         (<= top y) (<= y bottom))))

(defun elcity-inspect--find-building-center (state x y center-index size radius)
  "Return (CX . CY) for CENTER-INDEX near X,Y in STATE, else nil.
SIZE is footprint dimension, RADIUS bounds the center search region."
  (catch 'found
    (dotimes (dy (1+ (* 2 radius)))
      (dotimes (dx (1+ (* 2 radius)))
        (let* ((cx (+ x (- dx radius)))
               (cy (+ y (- dy radius)))
               (tile (elcity-state-api-tile-at state cx cy 0)))
          (when (and (= center-index (elcity-tile-index tile))
                     (elcity-inspect--building-footprint-contains-p
                      cx cy size radius x y))
            (throw 'found (cons cx cy))))))
    nil))

(defun elcity-inspect--building-context (state x y)
  "Return building context plist for tile X,Y in STATE, or nil.
Plist keys: :name :structure-kind :center-x :center-y :center-index."
  (cl-loop for (center-index size radius name structure-kind)
           in elcity-inspect--building-specs
           for center = (elcity-inspect--find-building-center
                         state x y center-index size radius)
           when center
           return (list :name name
                        :structure-kind structure-kind
                        :center-index center-index
                        :center-x (car center)
                        :center-y (cdr center))))

(defun elcity-inspect--zone-center-context (state x y)
  "Return zone context plist for tile X,Y in STATE, or nil.
Only classifies tile as zone footprint when its own tile index is in a
zone-kind range (center or border).  Non-zone tiles adjacent to zone
centers are not misclassified.
Plist keys: :kind :center-x :center-y :footprint-tile."
  (let* ((tile (elcity-state-api-tile-at state x y 0))
         (tile-index (elcity-tile-index tile))
         (kind-at-point (and (elcity-tile-zone-center-p tile)
                             (elcity-zones-kind-for-index tile-index))))
    (if kind-at-point
        (list :kind kind-at-point
              :center-x x
              :center-y y
              :footprint-tile nil)
      ;; Only search for a nearby center when this tile itself has a
      ;; zone-kind index (i.e. it is a border tile for some zone kind).
      (when (elcity-zones-kind-for-index tile-index)
        (catch 'found
          (dotimes (dy 3)
            (dotimes (dx 3)
              (let* ((cx (+ x (1- dx)))
                     (cy (+ y (1- dy)))
                     (center-tile (elcity-state-api-tile-at state cx cy 0))
                     (center-kind (and (elcity-tile-zone-center-p center-tile)
                                       (elcity-zones-kind-for-index
                                        (elcity-tile-index center-tile)))))
                (when center-kind
                  (throw 'found
                         (list :kind center-kind
                               :center-x cx
                               :center-y cy
                               :footprint-tile t))))))
          nil)))))

(defun elcity-inspect--power-status (tile x y power-scan connected-grid)
  "Return power-status symbol for TILE at X,Y.
POWER-SCAN and CONNECTED-GRID provide grid context.
Returns one of: `powered', `unpowered', `disconnected', `n/a'."
  (cond
   ((not (elcity-tile-conductive-p tile)) 'n/a)
   ((and power-scan
         (elcity-power-grid-cell-at (elcity-power-scan-grid power-scan) x y nil))
    'powered)
   ((and connected-grid
         (elcity-power-grid-cell-at connected-grid x y nil))
    'unpowered)
   (t 'disconnected)))

(defun elcity-inspect--quality-fields (state x y)
  "Return (POLLUTION LAND-VALUE CRIME) for tile X,Y in STATE."
  (let* ((snapshot (elcity-state-api-quality-snapshot state))
         (poll-map (elcity-quality-snapshot-pollution-map snapshot))
         (lv-map (elcity-quality-snapshot-land-value-map snapshot))
         (crime-map (elcity-quality-snapshot-crime-map snapshot)))
    (list (elcity-tile-field-ref poll-map x y 0)
          (elcity-tile-field-ref lv-map x y 0)
          (elcity-tile-field-ref crime-map x y 0))))

(defun elcity-inspect--congestion-at (state x y)
  "Return S3 congestion value at tile X,Y in STATE."
  (elcity-state-api-traffic-congestion-at state x y))

;;; ---------- Zone congestion ----------

(defun elcity-inspect--zone-bottleneck-congestion (state cx cy)
  "Return max congestion among perimeter roads near zone center CX,CY in STATE.
Returns 0 when no perimeter roads exist."
  (elcity-state-api-zone-bottleneck-congestion-at state cx cy))

(defun elcity-inspect--zone-average-congestion (state cx cy)
  "Return mean congestion among perimeter roads near zone center CX,CY in STATE.
Returns 0 when no perimeter roads exist."
  (elcity-state-api-zone-average-congestion-at state cx cy))

;;; ---------- Connected grid utility ----------

(defun elcity-inspect-connected-conductive-grid (state power-scan)
  "Return grid of conductive tiles connected to any source in STATE.
POWER-SCAN provides deterministic source order.  The returned grid
distinguishes tiles that are connected (but possibly unpowered due to
capacity limits) from tiles that are fully disconnected."
  (cl-check-type state elcity-state)
  (cl-check-type power-scan elcity-power-scan)
  (let* ((world (elcity-state-api-world state))
         (capacity (* (elcity-world-map-width world)
                      (elcity-world-map-height world)))
         (sources (elcity-power-scan-sources power-scan)))
    (elcity-power-scan-grid
     (elcity-power-scan-conductive-network world sources capacity))))

;;; ---------- Public builder ----------

(defun elcity-inspect-tile-info-at (state x y power-scan connected-grid)
  "Return `elcity-inspect-tile-info' for tile X,Y in STATE.
POWER-SCAN and CONNECTED-GRID provide power context.
This API is strict: callers must pass non-nil, type-valid scan inputs."
  (cl-check-type state elcity-state)
  (cl-check-type x integer)
  (cl-check-type y integer)
  (cl-check-type power-scan elcity-power-scan)
  (cl-check-type connected-grid elcity-power-grid)
  (let* ((tile (elcity-state-api-tile-at state x y 0))
         (index (elcity-tile-index tile))
         ;; Context resolution
         (zone-ctx (elcity-inspect--zone-center-context state x y))
         (building-ctx (unless zone-ctx
                         (elcity-inspect--building-context state x y)))
         ;; Context geometry
         (context-kind (cond (zone-ctx 'zone)
                             (building-ctx 'building)
                             ((elcity-traffic-crossing-index-p index) 'crossing)
                             ((elcity-traffic-road-index-p index) 'road)
                             ((elcity-traffic-powerline-index-p index) 'powerline)
                             ((= index 0) 'empty)
                             ((= index 1) 'water)
                             (t 'other)))
         (ctx-cx (cond (zone-ctx (plist-get zone-ctx :center-x))
                       (building-ctx (plist-get building-ctx :center-x))))
         (ctx-cy (cond (zone-ctx (plist-get zone-ctx :center-y))
                       (building-ctx (plist-get building-ctx :center-y))))
         ;; Footprint role
         (footprint-role
          (cond
           ((and zone-ctx (not (plist-get zone-ctx :footprint-tile))) 'center)
           ((and zone-ctx (plist-get zone-ctx :footprint-tile)) 'footprint)
           ((and building-ctx ctx-cx (= x ctx-cx) (= y ctx-cy)) 'center)
           (building-ctx 'footprint)
           (t 'tile)))
         ;; Zone fields
         (zone-kind (and zone-ctx (plist-get zone-ctx :kind)))
         (zone-cx (and zone-ctx (plist-get zone-ctx :center-x)))
         (zone-cy (and zone-ctx (plist-get zone-ctx :center-y)))
         (zone-level (and zone-ctx
                          (elcity-state-api-zone-level-at state zone-cx zone-cy 0)))
         (zone-pop (and zone-ctx zone-kind zone-level
                        (elcity-zones-population-for-level zone-kind zone-level)))
         ;; Building fields
         (structure-kind (and building-ctx
                              (plist-get building-ctx :structure-kind)))
         (structure-name (and building-ctx
                              (plist-get building-ctx :name)))
         ;; Power status — for zone centers, evaluate at zone center
         (power-tile (if zone-ctx
                        (elcity-state-api-tile-at state zone-cx zone-cy 0)
                      tile))
         (power-x (or zone-cx x))
         (power-y (or zone-cy y))
         (pwr-status (elcity-inspect--power-status
                      power-tile power-x power-y power-scan connected-grid))
         (powered-p (eq pwr-status 'powered))
         ;; Traffic status — zones use S3 route status, stations use road adjacency
         (station-road-adj
          (and (eq structure-kind 'service) ctx-cx ctx-cy
               (elcity-quality-station-road-adjacent-p
                (elcity-state-api-world state) ctx-cx ctx-cy)))
         (traffic-status (cond
                          (zone-ctx
                           (elcity-state-api-zone-traffic-status-at
                            state zone-cx zone-cy 'no-road))
                          ((eq structure-kind 'service)
                           (if station-road-adj 'reachable 'no-road))))
         (traffic-accessible (cond
                              (zone-ctx
                               (elcity-traffic-zone-accessible-p
                                (or traffic-status 'no-road)))
                              ((eq structure-kind 'service)
                               station-road-adj)))
         ;; Quality fields
         (quality (elcity-inspect--quality-fields state x y))
         (local-quality (and zone-ctx
                             (elcity-state-api-local-quality-at
                              state zone-cx zone-cy zone-kind)))
         ;; Demand (zones only)
         (demand (and zone-kind
                      (elcity-state-api-demand-at state zone-kind)))
         ;; Congestion
         (congestion (elcity-inspect--congestion-at state x y))
         ;; Effective congestion cap from infra-effectiveness
         (effective-cap
          (elcity-traffic-effective-congestion-cap
           (elcity-state-quality-infra-effectiveness state)))
         ;; Zone bottleneck congestion (max of perimeter roads)
         (zone-bottleneck (and zone-ctx
                               (elcity-inspect--zone-bottleneck-congestion
                                state zone-cx zone-cy)))
         ;; Zone average congestion (mean of perimeter roads)
         (zone-average (and zone-ctx
                            (elcity-inspect--zone-average-congestion
                             state zone-cx zone-cy)))
         ;; Fire state — query at context center for zones/buildings
         (fire-x (or ctx-cx x))
         (fire-y (or ctx-cy y))
         (fire-timers (elcity-state-api-fire-burn-timers state))
         (fire-remaining (and fire-timers
                              (gethash (cons fire-x fire-y) fire-timers)))
         (burning-p (not (null fire-remaining))))
    (pcase-let ((`(,pollution ,land-value ,crime) quality))
      (elcity-inspect-tile-info--create
       :x x
       :y y
       :tile-index index
       :context-kind context-kind
       :context-center-x ctx-cx
       :context-center-y ctx-cy
       :footprint-role footprint-role
       :zone-kind zone-kind
       :zone-level zone-level
       :zone-population zone-pop
       :structure-kind structure-kind
       :structure-name structure-name
       :powered-p powered-p
       :power-status pwr-status
       :traffic-accessible-p traffic-accessible
       :traffic-status traffic-status
       :pollution pollution
       :land-value land-value
       :crime crime
       :local-quality local-quality
       :demand demand
       :congestion congestion
       :congestion-effective-cap effective-cap
       :zone-bottleneck-congestion zone-bottleneck
       :zone-average-congestion zone-average
       :burning-p burning-p
       :fire-timer fire-remaining))))

;;; ---------- Explain constants ----------

(defconst elcity-inspect-quality-max 1000
  "Maximum value for S6 quality fields (pollution, crime, land-value).")

;;; ---------- Explain: tile name ----------

(defun elcity-inspect-explain-name (info)
  "Return display name string for tile INFO."
  (let ((ctx (elcity-inspect-tile-info-context-kind info))
        (burning (elcity-inspect-tile-info-burning-p info)))
    (let ((base
           (pcase ctx
             ('zone
              (let ((kind (elcity-inspect-tile-info-zone-kind info))
                    (footprint (eq 'footprint
                                   (elcity-inspect-tile-info-footprint-role info))))
                (concat
                 (pcase kind
                   ('residential "Residential Zone")
                   ('commercial "Commercial Zone")
                   ('industrial "Industrial Zone"))
                 (if footprint " (footprint)" ""))))
             ('building
              (let ((name (elcity-inspect-tile-info-structure-name info))
                    (footprint (eq 'footprint
                                   (elcity-inspect-tile-info-footprint-role info))))
                (concat name (if footprint " (footprint)" ""))))
             ('crossing "Road-Powerline Crossing")
             ('road "Road")
             ('powerline "Powerline")
             ('empty "Empty")
             ('water "Water")
             (_ "Unknown"))))
      (if burning
          (concat base " (Burning)")
        base))))

;;; ---------- Explain: effect sentence ----------

(defun elcity-inspect-explain-effect (info)
  "Return one-sentence effect description for tile INFO."
  (let ((fire-timer (elcity-inspect-tile-info-fire-timer info)))
    (if fire-timer
        (format "On fire — will demolish in %d ticks." fire-timer)
      (pcase (elcity-inspect-tile-info-context-kind info)
        ('zone
         (pcase (elcity-inspect-tile-info-zone-kind info)
           ('residential "Provides housing for residents.")
           ('commercial "Provides commercial jobs and services.")
           ('industrial "Provides industrial jobs and goods.")))
        ('building
         (pcase (elcity-inspect-tile-info-structure-kind info)
           ('power-plant "Generates power for connected zones.")
           ('amenity "Reduces local pollution in a small radius.")
           ('service
            (let ((name (elcity-inspect-tile-info-structure-name info)))
              (cond
               ((string= name "Fire Station")
                "Mitigates nearby pollution/crime and boosts land value.")
               ((string= name "Police Station")
                "Reduces crime in nearby area.")
               (t "Provides city services."))))))
        ('crossing "Carries road traffic and power transmission.")
        ('road "Connects zones for traffic access.")
        ('powerline "Transmits power between zones and plants.")
        ('empty "Vacant land available for development.")
        ('water "Water body, cannot be built on. Nearby shore raises land value.")
        (_ nil)))))

;;; ---------- Explain: key params ----------

(defun elcity-inspect-explain-params (info)
  "Return key parameters string for tile INFO."
  (pcase (elcity-inspect-tile-info-context-kind info)
    ('zone
     (let* ((level (elcity-inspect-tile-info-zone-level info))
            (pop (elcity-inspect-tile-info-zone-population info))
            (powered (elcity-inspect-tile-info-powered-p info))
            (traffic (elcity-inspect-tile-info-traffic-status info))
            (demand (elcity-inspect-tile-info-demand info))
            (avg-cong (elcity-inspect-tile-info-zone-average-congestion info))
            (ecap (elcity-inspect-tile-info-congestion-effective-cap info))
            (fire-timer (elcity-inspect-tile-info-fire-timer info))
            (parts (list (format "Level: %d" level)
                         (format "Pop: %d" pop)
                         (format "Power: %s"
                                 (if powered "yes" "no"))
                         (format "Traffic: %s"
                                 (symbol-name traffic))
                         (format "Demand: %d" demand))))
       (when (and avg-cong (> avg-cong 0))
         (setq parts
               (append parts
                       (list (format "Congestion: %d/%d"
                                     avg-cong ecap)))))
       (when fire-timer
         (setq parts
               (append parts
                       (list (format "Burning: %d ticks" fire-timer)))))
       (string-join parts " | ")))
    ('building
     (let* ((sk (elcity-inspect-tile-info-structure-kind info))
            (parts nil))
       ;; Amenities (parks) are always active — no power/road gate.
       (unless (eq sk 'amenity)
         (push (format "Power: %s"
                       (symbol-name
                        (elcity-inspect-tile-info-power-status info)))
               parts))
       (when (eq sk 'service)
         (push (format "Roads: %s"
                       (if (elcity-inspect-tile-info-traffic-accessible-p info)
                           "yes" "no"))
               parts))
       (let ((fire-timer (elcity-inspect-tile-info-fire-timer info)))
         (when fire-timer
           (push (format "Burning: %d ticks" fire-timer) parts)))
       (if parts
           (string-join (nreverse parts) " | ")
         "Always active")))
    ('crossing
     (let ((cong (elcity-inspect-tile-info-congestion info))
           (ecap (elcity-inspect-tile-info-congestion-effective-cap info)))
       (format "Power: %s | Congestion: %d/%d"
               (symbol-name (elcity-inspect-tile-info-power-status info))
               cong ecap)))
    ('road
     (let ((cong (elcity-inspect-tile-info-congestion info))
           (ecap (elcity-inspect-tile-info-congestion-effective-cap info)))
       (format "Congestion: %d/%d"
               cong ecap)))
    ('powerline
     (format "Power: %s"
             (symbol-name (elcity-inspect-tile-info-power-status info))))
    (_ nil)))

;;; ---------- Explain: quality line ----------

(defun elcity-inspect-explain-quality (info)
  "Return quality display string for tile INFO.
Shows pollution/crime/land-value as x/max.  Zone tiles also
include local-quality."
  (let* ((poll (elcity-inspect-tile-info-pollution info))
         (crime (elcity-inspect-tile-info-crime info))
         (lv (elcity-inspect-tile-info-land-value info))
         (lq (elcity-inspect-tile-info-local-quality info))
         (base (format "Pollution: %d/%d | Crime: %d/%d | Land value: %d/%d"
                       (or poll 0) elcity-inspect-quality-max
                       (or crime 0) elcity-inspect-quality-max
                       (or lv 0) elcity-inspect-quality-max)))
    (if lq
        (format "%s | Local quality: %d/%d"
                base lq elcity-inspect-quality-max)
      base)))

;;; ---------- Explain: improvement sentence ----------

(defun elcity-inspect-explain-improve (info)
  "Return one-sentence improvement guidance for tile INFO.
Uses fixed bottleneck priority: burning > power > traffic > pollution >
crime > land-value > generic."
  (if (elcity-inspect-tile-info-burning-p info)
      "Build a fire station nearby to prevent future fires."
    (pcase (elcity-inspect-tile-info-context-kind info)
      ('zone (elcity-inspect--zone-improve info))
      ('building (elcity-inspect--building-improve info))
      ('crossing (elcity-inspect--infra-improve info))
      ('road (elcity-inspect--infra-improve info))
      ('powerline (elcity-inspect--powerline-improve info))
      ('empty "Zone or build here to develop your city.")
      ('water "Water cannot be developed. Build near shore for land-value bonus.")
      (_ nil))))

(defun elcity-inspect--zone-improve (info)
  "Return improvement sentence for zone tile INFO."
  (let ((powered (elcity-inspect-tile-info-powered-p info))
        (traffic (elcity-inspect-tile-info-traffic-status info))
        (poll (or (elcity-inspect-tile-info-pollution info) 0))
        (crime (or (elcity-inspect-tile-info-crime info) 0))
        (lv (or (elcity-inspect-tile-info-land-value info) 0))
        (zone-kind (elcity-inspect-tile-info-zone-kind info)))
    (cond
     ((not powered)
      "Connect to a power plant with powerlines.")
     ((not (eq traffic 'reachable))
      "Build roads to connect this zone to other zone types.")
     ((and (eq zone-kind 'residential)
           (> poll elcity-zones-pollution-gate-threshold))
      "High pollution blocks residential growth; move industry away or add parks.")
     ((> poll 500)
      "Reduce nearby pollution sources to improve conditions.")
     ((> crime 500)
      "Build a police station nearby to reduce crime.")
     ((< lv 200)
      "Improve land value by reducing pollution and crime.")
     (t
      "Zone is developing; ensure demand stays positive for growth."))))

(defun elcity-inspect--building-improve (info)
  "Return improvement sentence for building tile INFO."
  (let ((power-status (elcity-inspect-tile-info-power-status info))
        (structure-kind (elcity-inspect-tile-info-structure-kind info)))
    (cond
     ((eq structure-kind 'amenity)
      "Park reduces nearby pollution; place more parks near industrial areas.")
     ((and (eq structure-kind 'service)
           (not (elcity-inspect-tile-info-traffic-accessible-p info)))
      "Build a road next to this station for full effectiveness.")
     ((memq power-status '(unpowered disconnected))
      "Connect to power grid with powerlines.")
     (t
      "Building is operational; fund its service budget to maintain effectiveness."))))

(defun elcity-inspect--infra-improve (info)
  "Return improvement sentence for road or crossing tile INFO."
  (let* ((cong (or (elcity-inspect-tile-info-congestion info) 0))
         (ecap (elcity-inspect-tile-info-congestion-effective-cap info))
         (threshold (/ ecap 2)))
    (if (> cong threshold)
        "High congestion; build alternative routes to spread traffic."
      "Road is functioning normally.")))

(defun elcity-inspect--powerline-improve (info)
  "Return improvement sentence for powerline tile INFO."
  (let ((status (elcity-inspect-tile-info-power-status info)))
    (pcase status
      ('disconnected "Connect this powerline to a power plant.")
      ('unpowered "Power grid is over capacity; build another power plant.")
      (_ "Powerline is transmitting normally."))))

(provide 'elcity-inspect-model)

;;; elcity-inspect-model.el ends here
