;;; elcity-persist.el --- S9 persistence: save/load game state -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; S9 persistence for saving and loading ElCity game state.
;;
;; This module owns:
;; - versioned save-file format contract,
;; - serialization of authoritative state fields,
;; - deserialization with derived-field reconstruction,
;; - atomic file I/O with explicit failure modes,
;; - hardened parsing for untrusted save-file input.
;;
;; Authoritative fields persisted:
;; - S0: tick, zones-cycle, fields-cycle, economy-cycle, evaluation-cycle.
;; - S1: world width, height, tiles vector.
;; - S5: demand-snapshot (R/C/I), demand-memory (R/C/I), tax-rate.
;; - S7: treasury, budget-policy (infrastructure/fire/police percents).
;; - S8: evaluation-snapshot (score, approval, complaint-severity-alist),
;;       evaluation-memory (score, approval, complaint-severity-alist).
;; - Fire incident: fire-cycle, fire-burn-timers.
;; - seed.
;; - Continuation context required for exact next-tick equivalence:
;;   S2 power-scan + zone-power-summary, S3 traffic-scan, S4 zones-scan,
;;   S6 quality-snapshot + cache keys, S7 budget-demand + budget-snapshot.
;;
;; Continuation context is mandatory in v3 payloads (tile-only format).
;;
;; Public API:
;;   (elcity-persist-save-game STATE FILE)  -> FILE path on success.
;;   (elcity-persist-load-game FILE)        -> elcity-state on success.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'elcity-state)
(require 'elcity-budget)
(require 'elcity-demand)
(require 'elcity-evaluation)
(require 'elcity-quality)
(require 'elcity-tile-field)
(require 'elcity-world)

(defconst elcity-persist-format-version 6
  "Current save-file format version.
Increment when authoritative field set or encoding changes.
Version 4 adds fire incident fields (fire-cycle, fire-burn-timers).
Version 5 adds collectability and gross-tax-income to budget-snapshot.
Version 6 reorganizes multi-tile building indices into contiguous
row-major ranges (old split center/border indices are invalid).")

(defconst elcity-persist-min-supported-version 6
  "Minimum save-file format version that can be loaded.
Saves before version 6 use legacy split building indices that are
incompatible with the contiguous family registry.")

(defconst elcity-persist-file-tag 'elcity-save
  "Symbol tag at the head of every save file for format identification.")

(defconst elcity-persist-max-file-bytes (* 8 1024 1024)
  "Maximum accepted save-file size in bytes for untrusted input handling.")

;;; --- Serialization (state -> plist) ---

(defun elcity-persist--serialize-demand (snapshot)
  "Serialize demand SNAPSHOT to a plist."
  (list :residential (elcity-demand-snapshot-residential snapshot)
        :commercial (elcity-demand-snapshot-commercial snapshot)
        :industrial (elcity-demand-snapshot-industrial snapshot)))

(defun elcity-persist--serialize-budget-policy (policy)
  "Serialize budget POLICY to a plist."
  (list :infrastructure-percent
        (elcity-budget-policy-infrastructure-percent policy)
        :fire-percent
        (elcity-budget-policy-fire-percent policy)
        :police-percent
        (elcity-budget-policy-police-percent policy)))

(defun elcity-persist--serialize-eval-snapshot (snapshot)
  "Serialize evaluation SNAPSHOT to a plist."
  (list :city-score
        (elcity-evaluation-snapshot-city-score snapshot)
        :approval
        (elcity-evaluation-snapshot-approval snapshot)
        :complaint-severity-alist
        (elcity-evaluation-snapshot-complaint-severity-alist snapshot)))

(defun elcity-persist--serialize-eval-memory (memory)
  "Serialize evaluation MEMORY to a plist."
  (list :score
        (elcity-evaluation-memory-score memory)
        :approval
        (elcity-evaluation-memory-approval memory)
        :complaint-severity-alist
        (elcity-evaluation-memory-complaint-severity-alist memory)))

(defun elcity-persist--serialize-fire-burn-timers (timers)
  "Serialize fire burn TIMERS hash table to a sorted alist.
Returns nil when TIMERS is nil.  Each entry is ((X . Y) . REMAINING-TICKS)
sorted by (X . Y) for deterministic output."
  (when timers
    (let (entries)
      (maphash (lambda (key value)
                 (push (cons key value) entries))
               timers)
      (sort entries (lambda (a b)
                      (let ((ax (caar a)) (ay (cdar a))
                            (bx (caar b)) (by (cdar b)))
                        (or (< ax bx)
                            (and (= ax bx) (< ay by)))))))))

(defun elcity-persist--serialize-state (state)
  "Serialize authoritative fields of STATE to a plist payload."
  (let ((world (elcity-state-world state)))
    (list
     ;; S0 counters.
     :tick (elcity-state-tick state)
     :zones-cycle (elcity-state-zones-cycle state)
     :fields-cycle (elcity-state-fields-cycle state)
     :economy-cycle (elcity-state-economy-cycle state)
     :evaluation-cycle (elcity-state-evaluation-cycle state)
     ;; S1 world.
     :world-width (elcity-world-map-width world)
     :world-height (elcity-world-map-height world)
     :world-tiles (elcity-world-map-tiles world)
     ;; S5 demand.
     :demand-snapshot
     (elcity-persist--serialize-demand
      (elcity-state-demand-snapshot state))
     :demand-memory
     (elcity-persist--serialize-demand
      (elcity-state-demand-memory state))
     :tax-rate (elcity-state-tax-rate state)
     ;; S7 budget policy and treasury.
     :treasury (elcity-state-treasury state)
     :budget-policy
     (elcity-persist--serialize-budget-policy
      (elcity-state-budget-policy state))
     ;; S8 evaluation.
     :evaluation-snapshot
     (elcity-persist--serialize-eval-snapshot
      (elcity-state-evaluation-snapshot state))
     :evaluation-memory
     (elcity-persist--serialize-eval-memory
      (elcity-state-evaluation-memory state))
     ;; Fire incident state.
     :fire-cycle (elcity-state-fire-cycle state)
     :fire-burn-timers (elcity-persist--serialize-fire-burn-timers
                        (elcity-state-fire-burn-timers state))
     ;; Replay seed.
     :seed (elcity-state-seed state)
     ;; Continuation context to preserve exact next-tick behavior.
     :power-scan (elcity-state-power-scan state)
     :power-capacity-key (elcity-state-power-capacity-key state)
     :zone-power-summary (elcity-state-zone-power-summary state)
     :traffic-scan (elcity-state-traffic-scan state)
     :zones-scan (elcity-state-zones-scan state)
     :quality-snapshot (elcity-state-quality-snapshot state)
     :quality-fire-effectiveness
     (elcity-state-quality-fire-effectiveness state)
     :quality-police-effectiveness
     (elcity-state-quality-police-effectiveness state)
     :quality-infra-effectiveness
     (elcity-state-quality-infra-effectiveness state)
     :budget-demand (elcity-state-budget-demand state)
     :budget-snapshot (elcity-state-budget-snapshot state))))

(defun elcity-persist--make-envelope (payload)
  "Wrap PAYLOAD plist in a versioned save envelope."
  (list elcity-persist-file-tag
        :version elcity-persist-format-version
        :payload payload))

;;; --- Deserialization (plist -> state) ---

(defun elcity-persist--deserialize-demand (plist)
  "Reconstruct demand snapshot from PLIST."
  (elcity-demand-snapshot-create
   :residential (plist-get plist :residential)
   :commercial (plist-get plist :commercial)
   :industrial (plist-get plist :industrial)))

(defun elcity-persist--deserialize-budget-policy (plist)
  "Reconstruct budget policy from PLIST."
  (elcity-budget-policy-create
   :infrastructure-percent (plist-get plist :infrastructure-percent)
   :fire-percent (plist-get plist :fire-percent)
   :police-percent (plist-get plist :police-percent)))

(defun elcity-persist--deserialize-eval-snapshot (plist)
  "Reconstruct evaluation snapshot from PLIST."
  (elcity-evaluation-snapshot-create
   :city-score (plist-get plist :city-score)
   :approval (plist-get plist :approval)
   ;; top-complaints is derived from severity alist during evaluation.
   :top-complaints
   (elcity-persist--top-complaints-from-severity
    (plist-get plist :complaint-severity-alist))
   :complaint-severity-alist
   (plist-get plist :complaint-severity-alist)))

(defun elcity-persist--deserialize-eval-memory (plist)
  "Reconstruct evaluation memory from PLIST."
  (elcity-evaluation-memory-create
   :score (plist-get plist :score)
   :approval (plist-get plist :approval)
   :complaint-severity-alist
   (plist-get plist :complaint-severity-alist)))

(defun elcity-persist--top-complaints-from-severity (severity-alist)
  "Derive top complaint kinds from SEVERITY-ALIST.
Returns the top N kinds sorted by severity descending with canonical
kind-order tie-break, matching S8 ranking semantics."
  (let* ((n elcity-evaluation-top-complaints-count)
         (sorted (sort (copy-sequence severity-alist)
                       (lambda (a b)
                         (let ((sa (cdr a))
                               (sb (cdr b)))
                           (if (/= sa sb)
                               (> sa sb)
                             ;; Tie-break by canonical kind order.
                             (< (cl-position (car a)
                                             elcity-evaluation-complaint-kinds)
                                (cl-position (car b)
                                             elcity-evaluation-complaint-kinds))))))))
    (mapcar #'car (seq-take sorted n))))

(defun elcity-persist--deserialize-fire-burn-timers (alist world)
  "Reconstruct fire burn timers hash table from ALIST for WORLD.
Returns nil when ALIST is nil.  Validates coordinate bounds and value types."
  (when alist
    (let ((timers (make-hash-table :test 'equal))
          (w (elcity-world-map-width world))
          (h (elcity-world-map-height world)))
      (dolist (entry alist)
        (unless (and (consp entry) (consp (car entry)))
          (error "Invalid fire-burn-timers entry: %S" entry))
        (let ((x (caar entry))
              (y (cdar entry))
              (remaining (cdr entry)))
          (unless (and (integerp x) (integerp y))
            (error "Invalid fire-burn-timers coordinate: (%S . %S)" x y))
          (unless (and (>= x 0) (< x w) (>= y 0) (< y h))
            (error "Fire-burn-timers coordinate out of bounds: (%d . %d) in %dx%d world"
                   x y w h))
          (unless (and (integerp remaining) (> remaining 0))
            (error "Invalid fire-burn-timers remaining ticks: %S at (%d,%d)"
                   remaining x y))
          (puthash (cons x y) remaining timers)))
      timers)))

(defun elcity-persist--reconstruct-world (payload)
  "Reconstruct world map from PAYLOAD plist."
  (let ((width (plist-get payload :world-width))
        (height (plist-get payload :world-height))
        (tiles (plist-get payload :world-tiles)))
    (unless (and (integerp width) (> width 0))
      (error "Invalid world width in save payload: %S" width))
    (unless (and (integerp height) (> height 0))
      (error "Invalid world height in save payload: %S" height))
    (unless (vectorp tiles)
      (error "Invalid world tiles in save payload"))
    (unless (= (length tiles) (* width height))
      (error "World tiles length %d does not match %dx%d"
             (length tiles) width height))
    (elcity-world-map-create
     :width width
     :height height
     :tiles (copy-sequence tiles))))

(defun elcity-persist--payload-integer (payload key &optional minimum)
  "Return integer value for KEY in PAYLOAD, optionally enforcing MINIMUM."
  (let ((value (plist-get payload key)))
    (unless (integerp value)
      (error "Invalid non-integer value for %S in save payload: %S"
             key value))
    (when (and minimum (< value minimum))
      (error "Invalid value for %S in save payload: %S < %S"
             key value minimum))
    value))

(defun elcity-persist--tile-field-compatible-p (world field)
  "Return non-nil when tile FIELD dimensions match WORLD."
  (and (elcity-tile-field-p field)
       (= (elcity-tile-field-width field)
          (elcity-world-map-width world))
       (= (elcity-tile-field-height field)
          (elcity-world-map-height world))))

(defun elcity-persist--quality-snapshot-compatible-p (world snapshot)
  "Return non-nil when quality SNAPSHOT dimensions match WORLD."
  (and (elcity-quality-snapshot-p snapshot)
       (elcity-persist--tile-field-compatible-p
        world
        (elcity-quality-snapshot-pollution-map snapshot))
       (elcity-persist--tile-field-compatible-p
        world
        (elcity-quality-snapshot-land-value-map snapshot))
       (elcity-persist--tile-field-compatible-p
        world
        (elcity-quality-snapshot-crime-map snapshot))))

(defun elcity-persist--power-scan-compatible-p (world scan)
  "Return non-nil when power SCAN dimensions match WORLD."
  (and (elcity-power-scan-p scan)
       (let ((grid (elcity-power-scan-grid scan)))
         (and (elcity-power-grid-p grid)
              (= (elcity-power-grid-width grid)
                 (elcity-world-map-width world))
              (= (elcity-power-grid-height grid)
                 (elcity-world-map-height world))))))

(defun elcity-persist--traffic-scan-compatible-p (world scan)
  "Return non-nil when traffic SCAN dimensions match WORLD."
  (and (elcity-traffic-scan-p scan)
       (elcity-traffic-summary-p (elcity-traffic-scan-summary scan))
       (cl-every #'elcity-traffic-route-result-p
                 (elcity-traffic-scan-results scan))
       (elcity-traffic-congestion-map-compatible-p
        world
        (elcity-traffic-scan-congestion-map scan))
       (let ((smap (elcity-traffic-scan-status-map scan)))
         (or (null smap)
             (and (elcity-tile-field-p smap)
                  (= (elcity-tile-field-width smap)
                     (elcity-world-map-width world))
                  (= (elcity-tile-field-height smap)
                     (elcity-world-map-height world)))))))

(defun elcity-persist--zones-scan-compatible-p (world scan)
  "Return non-nil when zones SCAN dimensions match WORLD."
  (and (elcity-zone-dynamics-scan-p scan)
       (elcity-zone-dynamics-summary-p
        (elcity-zone-dynamics-scan-summary scan))
       (elcity-zones-level-map-compatible-p
        world
        (elcity-zone-dynamics-scan-level-map scan))))

(defconst elcity-persist--v4-budget-snapshot-length 23
  "Record length of v4 budget-snapshot (type tag + 22 data slots).")

(defconst elcity-persist--v5-budget-snapshot-slots 24
  "Number of data slots in v5 budget-snapshot (excludes type tag).")

(defun elcity-persist--migrate-budget-snapshot (snapshot)
  "Migrate v4 budget SNAPSHOT to v5 format if needed.
V4 snapshots lack collectability and gross-tax-income slots.
Migration sets collectability to max (1000) and gross-tax-income
equal to tax-income (pre-collectability semantics)."
  (if (and (recordp snapshot)
           (= (length snapshot)
              elcity-persist--v4-budget-snapshot-length))
      (let ((new (make-record 'elcity-budget-snapshot
                              elcity-persist--v5-budget-snapshot-slots
                              nil)))
        ;; Copy all v4 data slots (indices 1..22)
        (dotimes (i (1- elcity-persist--v4-budget-snapshot-length))
          (aset new (1+ i) (aref snapshot (1+ i))))
        ;; collectability = 1000 (full collection, no crime penalty)
        (aset new 23 elcity-budget-collectability-max)
        ;; gross-tax-income = tax-income (index 2) before collectability
        (aset new 24 (aref snapshot 2))
        new)
    snapshot))

(defun elcity-persist--reconstruct-state (payload)
  "Reconstruct full simulation state from PAYLOAD plist.
Authoritative and continuation fields are restored from payload."
  ;; Validate required keys.
  (dolist (key '(:tick :zones-cycle :fields-cycle :economy-cycle :evaluation-cycle
                 :world-width :world-height :world-tiles
                 :demand-snapshot :demand-memory :tax-rate
                 :treasury :budget-policy
                 :fire-cycle :fire-burn-timers
                 :evaluation-snapshot :evaluation-memory :seed
                 :power-scan :zone-power-summary :traffic-scan :zones-scan
                 :quality-snapshot
                 :quality-fire-effectiveness
                 :quality-police-effectiveness
                 :quality-infra-effectiveness
                 :budget-demand :budget-snapshot))
    (unless (plist-member payload key)
      (error "Missing required key in save payload: %S" key)))
  (let* ((tick (elcity-persist--payload-integer payload :tick 0))
         (zones-cycle (elcity-persist--payload-integer payload :zones-cycle 0))
         (fields-cycle (elcity-persist--payload-integer payload :fields-cycle 0))
         (economy-cycle (elcity-persist--payload-integer payload :economy-cycle 0))
         (evaluation-cycle (elcity-persist--payload-integer
                            payload
                            :evaluation-cycle
                            0))
         (tax-rate (elcity-demand-clamp-tax-rate
                    (elcity-persist--payload-integer payload :tax-rate)))
         (treasury (elcity-budget-clamp-treasury
                    (elcity-persist--payload-integer payload :treasury)))
         (seed (elcity-persist--payload-integer payload :seed))
         (fire-cycle (elcity-persist--payload-integer payload :fire-cycle 0))
         (world (elcity-persist--reconstruct-world payload))
         (fire-burn-timers
          (elcity-persist--deserialize-fire-burn-timers
           (plist-get payload :fire-burn-timers) world))
         (demand-snapshot
          (elcity-persist--deserialize-demand
           (plist-get payload :demand-snapshot)))
         (demand-memory
          (elcity-persist--deserialize-demand
           (plist-get payload :demand-memory)))
         (budget-policy
          (elcity-persist--deserialize-budget-policy
           (plist-get payload :budget-policy)))
         (eval-snapshot
          (elcity-persist--deserialize-eval-snapshot
           (plist-get payload :evaluation-snapshot)))
         (eval-memory
          (elcity-persist--deserialize-eval-memory
           (plist-get payload :evaluation-memory)))
         (power-scan (plist-get payload :power-scan))
         (power-capacity-key (plist-get payload :power-capacity-key))
         (zone-power-summary (plist-get payload :zone-power-summary))
         (traffic-scan (plist-get payload :traffic-scan))
         (zones-scan (plist-get payload :zones-scan))
         (quality-snapshot (plist-get payload :quality-snapshot))
         (quality-fire-effectiveness
          (plist-get payload :quality-fire-effectiveness))
         (quality-police-effectiveness
          (plist-get payload :quality-police-effectiveness))
         (quality-infra-effectiveness
          (plist-get payload :quality-infra-effectiveness))
         (budget-demand (plist-get payload :budget-demand))
         (budget-snapshot (plist-get payload :budget-snapshot)))
    ;; Validate optional continuation context payload.
    (when power-scan
      (unless (elcity-persist--power-scan-compatible-p world power-scan)
        (error "Power scan in save payload is not compatible with world")))
    (unless (or (null power-capacity-key) (integerp power-capacity-key))
      (error "Invalid power-capacity-key in save payload: %S"
             power-capacity-key))
    (when (and (not power-scan) zone-power-summary)
      (error "Invalid save payload: zone-power-summary without power-scan"))
    (setq zone-power-summary
          (or zone-power-summary
              (and power-scan
                   (elcity-power-summarize-zones
                    world
                    (elcity-power-scan-grid power-scan)))))
    (when zone-power-summary
      (unless (elcity-zone-power-summary-p zone-power-summary)
        (error "Invalid zone power summary in save payload")))
    (when traffic-scan
      (unless (elcity-persist--traffic-scan-compatible-p world traffic-scan)
        (error "Traffic scan in save payload is not compatible with world")))
    (when zones-scan
      (unless (elcity-persist--zones-scan-compatible-p world zones-scan)
        (error "Zones scan in save payload is not compatible with world")))
    (when quality-snapshot
      (unless (elcity-persist--quality-snapshot-compatible-p world quality-snapshot)
        (error "Quality snapshot in save payload is not compatible with world")))
    (unless (or (null quality-fire-effectiveness)
                (integerp quality-fire-effectiveness))
      (error "Invalid quality-fire-effectiveness in save payload: %S"
             quality-fire-effectiveness))
    (unless (or (null quality-police-effectiveness)
                (integerp quality-police-effectiveness))
      (error "Invalid quality-police-effectiveness in save payload: %S"
             quality-police-effectiveness))
    (unless (or (null quality-infra-effectiveness)
                (integerp quality-infra-effectiveness))
      (error "Invalid quality-infra-effectiveness in save payload: %S"
             quality-infra-effectiveness))
    (unless (elcity-budget-demand-p budget-demand)
      (error "Invalid budget-demand in save payload"))
    ;; Migrate v4 budget-snapshot (missing collectability fields)
    (setq budget-snapshot
          (elcity-persist--migrate-budget-snapshot budget-snapshot))
    (unless (elcity-budget-snapshot-p budget-snapshot)
      (error "Invalid budget-snapshot in save payload"))
    (unless (elcity-budget-snapshot-consistent-p budget-snapshot)
      (error "Invalid budget-snapshot arithmetic invariants in save payload"))
    (elcity-state-create
     ;; S0 counters.
     :tick tick
     :zones-cycle zones-cycle
     :fields-cycle fields-cycle
     :economy-cycle economy-cycle
     :evaluation-cycle evaluation-cycle
     ;; S1 authoritative world.
     :world world
     ;; S2/S3/S4 continuation context (when present).
     :power-world (and power-scan world)
     :power-capacity-key power-capacity-key
     :power-scan power-scan
     :zone-power-summary zone-power-summary
     :traffic-scan traffic-scan
     :zones-world (and zones-scan world)
     :zones-scan zones-scan
     ;; S5 restored.
     :demand-snapshot demand-snapshot
     :demand-memory demand-memory
     :tax-rate tax-rate
     ;; S6 restored continuation context.
     :quality-world world
     :quality-snapshot quality-snapshot
     :quality-fire-effectiveness quality-fire-effectiveness
     :quality-police-effectiveness quality-police-effectiveness
     :quality-infra-effectiveness quality-infra-effectiveness
     ;; S7 restored.
     :treasury treasury
     :budget-demand budget-demand
     :budget-policy budget-policy
     :budget-snapshot budget-snapshot
     ;; Fire incident state.
     :fire-cycle fire-cycle
     :fire-burn-timers fire-burn-timers
     ;; S8 restored.
     :evaluation-snapshot eval-snapshot
     :evaluation-memory eval-memory
     ;; Replay seed.
     :seed seed)))

;;; --- File I/O ---

(defun elcity-persist--validate-envelope (data)
  "Validate save-file DATA and return the payload plist.
Signals an error for invalid tag, missing version, or version mismatch."
  (unless (and (listp data) (>= (length data) 3))
    (error "Malformed save file: expected (TAG :version N :payload PLIST)"))
  (let ((tag (car data))
        (props (cdr data)))
    (unless (eq tag elcity-persist-file-tag)
      (error "Invalid save file tag: %S (expected %S)"
             tag elcity-persist-file-tag))
    (let ((version (plist-get props :version)))
      (unless (integerp version)
        (error "Missing or invalid version in save file"))
      (unless (<= elcity-persist-min-supported-version
                  version
                  elcity-persist-format-version)
        (if (< version elcity-persist-min-supported-version)
            (error "Save file version %d is too old; minimum supported is %d"
                   version elcity-persist-min-supported-version)
          (error "Save file version %d is newer than this build; expected version %d"
                 version elcity-persist-format-version)))
      (unless (plist-member props :payload)
        (error "Missing payload in save file"))
      (let ((payload (plist-get props :payload)))
        (unless (listp payload)
          (error "Invalid payload in save file"))
        payload))))

(defun elcity-persist-save-game (state file)
  "Save game STATE to FILE.
Writes a versioned plist payload atomically.  Returns FILE path on
success.  Signals an error if writing fails."
  (cl-check-type state elcity-state)
  (cl-check-type file string)
  (let* ((payload (elcity-persist--serialize-state state))
         (envelope (elcity-persist--make-envelope payload))
         (path (expand-file-name file))
         (dir (file-name-directory path))
         tmpfile)
    (when dir
      (make-directory dir t))
    (setq tmpfile (make-temp-file
                   (expand-file-name ".elcity-save-" (or dir default-directory))
                   nil
                   ".tmp"))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (let ((print-length nil)
                  (print-level nil)
                  (coding-system-for-write 'utf-8-unix))
              (prin1 envelope (current-buffer))))
          ;; Atomic on the same filesystem because TMPFILE is created in DIR.
          (rename-file tmpfile path t))
      (when (and tmpfile
                 (file-exists-p tmpfile))
        (delete-file tmpfile)))
    path))

(defun elcity-persist--safe-read-save-data (path)
  "Read and return one save form from PATH using hardened reader settings."
  (let* ((attrs (file-attributes path))
         (size (and attrs (file-attribute-size attrs))))
    (when (and (integerp size)
               (> size elcity-persist-max-file-bytes))
      (error "Save file too large: %d bytes (max %d)"
             size
             elcity-persist-max-file-bytes)))
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix))
      (insert-file-contents path))
    (goto-char (point-min))
    (let ((read-eval nil)
          (read-circle nil)
          (read-symbol-shorthands nil))
      ;; Reader hardening is carried via dynamically scoped reader vars.
      (ignore read-eval read-circle read-symbol-shorthands)
      (condition-case err
          (let ((form (read (current-buffer))))
            (skip-chars-forward " \t\n\r\f")
            (unless (eobp)
              (error "Trailing data after save payload"))
            form)
        (error
         (error "Failed to parse save file %s: %s"
                path (error-message-string err)))))))

(defun elcity-persist-load-game (file)
  "Load game state from FILE.
Returns a fully reconstructed `elcity-state' on success.
Signals an error for missing file, malformed data, or version mismatch."
  (cl-check-type file string)
  (let ((path (expand-file-name file)))
    (unless (file-exists-p path)
      (error "Save file not found: %s" path))
    (let ((data (elcity-persist--safe-read-save-data path)))
      (let ((payload (elcity-persist--validate-envelope data)))
        (elcity-persist--reconstruct-state payload)))))

;;; --- Summary printing ---

(defun elcity-persist-state-summary (state)
  "Return a deterministic summary string for STATE.
Includes key fields useful for debugging: tick, world size, demand,
treasury, score, and approval."
  (cl-check-type state elcity-state)
  (let* ((world (elcity-state-world state))
         (demand (elcity-state-demand-snapshot state))
         (eval-snap (elcity-state-evaluation-snapshot state)))
    (format (concat "tick: %d\n"
                    "seed: %d\n"
                    "world: %dx%d\n"
                    "tax-rate: %d\n"
                    "treasury: %d\n"
                    "demand-r: %d\n"
                    "demand-c: %d\n"
                    "demand-i: %d\n"
                    "city-score: %d\n"
                    "approval: %d")
            (elcity-state-tick state)
            (elcity-state-seed state)
            (elcity-world-map-width world)
            (elcity-world-map-height world)
            (elcity-state-tax-rate state)
            (elcity-state-treasury state)
            (elcity-demand-snapshot-residential demand)
            (elcity-demand-snapshot-commercial demand)
            (elcity-demand-snapshot-industrial demand)
            (elcity-evaluation-snapshot-city-score eval-snap)
            (elcity-evaluation-snapshot-approval eval-snap))))

;;; --- Machine-readable state dump ---

(defun elcity-persist-state-dump (state)
  "Return a machine-readable plist dump of necessary STATE fields.
Output schema is stable and intended for LLM/tooling consumption.
Keys and value types:
  :tick             integer  - absolute simulation time
  :seed             integer  - deterministic replay seed
  :world-width      integer  - world tile width
  :world-height     integer  - world tile height
  :tax-rate         integer  - current tax rate percent [0, 20]
  :treasury         integer  - available city cash
  :demand-r         integer  - residential demand [-2000, 2000]
  :demand-c         integer  - commercial demand [-2000, 2000]
  :demand-i         integer  - industrial demand [-2000, 2000]
  :city-score       integer  - evaluation score [0, 1000]
  :approval         integer  - approval proxy [0, 1000]
  :top-complaints   list     - top 3 complaint kind symbols"
  (cl-check-type state elcity-state)
  (let* ((world (elcity-state-world state))
         (demand (elcity-state-demand-snapshot state))
         (eval-snap (elcity-state-evaluation-snapshot state)))
    (list :tick (elcity-state-tick state)
          :seed (elcity-state-seed state)
          :world-width (elcity-world-map-width world)
          :world-height (elcity-world-map-height world)
          :tax-rate (elcity-state-tax-rate state)
          :treasury (elcity-state-treasury state)
          :demand-r (elcity-demand-snapshot-residential demand)
          :demand-c (elcity-demand-snapshot-commercial demand)
          :demand-i (elcity-demand-snapshot-industrial demand)
          :city-score (elcity-evaluation-snapshot-city-score eval-snap)
          :approval (elcity-evaluation-snapshot-approval eval-snap)
          :top-complaints
          (elcity-evaluation-snapshot-top-complaints eval-snap))))

;;; --- CLI batch entrypoints ---

(defun elcity-persist-batch-load ()
  "Load a save file from CLI and print summary.
Usage: Emacs --batch -l elcity-persist -f elcity-persist-batch-load FILE
Exits non-zero on any error."
  (let ((args command-line-args-left))
    (unless args
      (message "Error: no save file path provided")
      (kill-emacs 1))
    (let ((file (car args)))
      (setq command-line-args-left (cdr args))
      (condition-case err
          (let ((state (elcity-persist-load-game file)))
            (message "%s" (elcity-persist-state-summary state)))
        (error
         (message "Error: %s" (error-message-string err))
         (kill-emacs 1))))))

(defun elcity-persist-batch-dump ()
  "Load a save file from CLI and print machine-readable dump.
Usage: Emacs --batch -l elcity-persist -f elcity-persist-batch-dump FILE
Output is a single s-expression plist suitable for programmatic parsing.
Exits non-zero on any error."
  (let ((args command-line-args-left))
    (unless args
      (message "Error: no save file path provided")
      (kill-emacs 1))
    (let ((file (car args)))
      (setq command-line-args-left (cdr args))
      (condition-case err
          (let* ((state (elcity-persist-load-game file))
                 (dump (elcity-persist-state-dump state)))
            (let ((print-length nil)
                  (print-level nil))
              (message "%S" dump)))
        (error
         (message "Error: %s" (error-message-string err))
         (kill-emacs 1))))))

(provide 'elcity-persist)

;;; elcity-persist.el ends here
