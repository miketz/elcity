;;; elcity-player-core.el --- Player shared data and contracts -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; Shared data definitions, session struct, buffer-local state,
;; tool metadata helpers, tile coordinate helpers, action queue helpers,
;; and state advance helper for the player subsystem.
;;
;; Other player submodules (ui, session, input) depend on this module.
;; This module depends on `elcity-player-render' (tile view helpers)
;; but has no dependencies on other player submodules.

;;; Code:

(require 'cl-lib)
(require 'elcity)
(require 'elcity-building-cost)
(require 'elcity-budget)
(require 'elcity-player-render)

;;; ---------- Constants ----------

(defconst elcity-player-buffer-name "*ElCity*"
  "Buffer name for the player UI.")

(defconst elcity-player-tick-interval 0.3
  "Simulation tick interval in seconds for player run-loop.")

(eval-and-compile
(defconst elcity-player-tool-specs
  '((road
     :key "r"
     :name "Road"
     :help "Place road (1x1). Roads connect zones for traffic."
     :footprint (:size 1 :anchor tile)
     :engine-anchor tile)
    (powerline
     :key "p"
     :name "Power"
     :help "Place powerline (1x1). Connects zones to power plants."
     :footprint (:size 1 :anchor tile)
     :engine-anchor tile)
    (zone-residential
     :key "R"
     :name "Res"
     :help "Zone residential (3x3). Provides housing."
     :footprint (:size 3 :anchor center)
     :engine-anchor center)
    (zone-commercial
     :key "C"
     :name "Com"
     :help "Zone commercial (3x3). Provides jobs."
     :footprint (:size 3 :anchor center)
     :engine-anchor center)
    (zone-industrial
     :key "I"
     :name "Ind"
     :help "Zone industrial (3x3). Provides jobs."
     :footprint (:size 3 :anchor center)
     :engine-anchor center)
    (coal-plant
     :key "c"
     :name "Coal"
     :help "Build coal plant (3x3). Generates power."
     :footprint (:size 3 :anchor center)
     :engine-anchor top-left)
    (nuclear-plant
     :key "n"
     :name "Nuke"
     :help "Build nuclear plant (4x4). Generates power."
     :footprint (:size 4 :anchor center)
     :engine-anchor top-left)
    (fire-station
     :key "f"
     :name "Fire"
     :help "Build fire station (3x3). Mitigates nearby pollution/crime and boosts land value."
     :footprint (:size 3 :anchor center)
     :engine-anchor top-left)
    (police-station
     :key "o"
     :name "Police"
     :help "Build police station (3x3). Reduces crime."
     :footprint (:size 3 :anchor center)
     :engine-anchor top-left)
    (park
     :key "k"
     :name "Park"
     :help "Build park (1x1). Reduces local pollution."
     :footprint (:size 1 :anchor tile)
     :engine-anchor tile)
    (bulldoze
     :key "b"
     :name "Bulldoze"
     :help "Bulldoze at tile. Clears the detected building/zone footprint."
     :footprint (:size 1 :anchor tile)
     :engine-anchor tile)
    (inspect
     :key "i"
     :name "Inspect"
     :help "Inspect tile [i]. Click or RET to see tile info."))
  "Ordered canonical tool metadata for palette, keys, and placement.
Each entry is (TOOL-KIND . PROPS), where PROPS is a plist with:
- `:key': palette/keybinding shortcut string,
- `:name': display label,
- `:help': palette help-echo text,
- `:footprint': placement preview footprint plist (when applicable),
- `:engine-anchor': placement-anchor semantics for S11 calls:
  `tile', `center', or `top-left'.
The interactive selection command `elcity-player-select-KIND' is
derived by convention via `elcity-player-tool-command'.")) ;; eval-and-compile

(defconst elcity-player-budget-percent-step 10
  "Percentage point change per budget increase/decrease command.")

(defconst elcity-player-tax-rate-step 1
  "Percentage point change per tax rate increase/decrease command.")

(defconst elcity-player-budget-control-cycle
  (cons 'tax elcity-budget-service-kinds)
  "Cycling order for budget control selection.
Starts with `tax', then service kinds (infrastructure, fire, police).")

;;; ---------- Session model ----------

(cl-defstruct (elcity-player-session
               (:constructor elcity-player-session-create))
  "Player session container.
STATE is the current `elcity-state' snapshot.
ACTIVE-TOOL is the currently selected building tool kind.
RUNNING indicates whether the simulation timer is active.
BUDGET-CONTROL is the active budget control for `[+/-]' adjustment:
  `tax' for tax rate, or a service kind (infrastructure/fire/police)
  for budget funding percent.
OVERLAY is the active map overlay kind (a symbol from
  `elcity-overlay-kinds'), or nil for normal view."
  (state nil :type elcity-state)
  (active-tool 'inspect :type symbol)
  (running nil :type boolean)
  (budget-control 'tax :type symbol)
  (overlay nil :type symbol))

;;; ---------- Buffer-local state ----------

(defvar-local elcity-player--session nil
  "Buffer-local `elcity-player-session' in player buffers.")

(defvar-local elcity-player--timer nil
  "Buffer-local run-loop timer in player buffers.")

(defvar-local elcity-player--map-start-pos nil
  "Buffer position where the tile map region begins.
Set during rendering, used for mouse-click coordinate conversion.")

(defvar-local elcity-player--palette-start-pos nil
  "Buffer position where the tool palette region begins.")

(defconst elcity-player-undo-max 3
  "Maximum number of states kept in the undo ring.")

(defvar-local elcity-player--undo-ring nil
  "Buffer-local undo ring for building placements.
A list of recent `elcity-state' snapshots, newest first.
Maximum length is `elcity-player-undo-max'.")

(defvar-local elcity-player--pending-actions nil
  "Buffer-local list of S10 actions queued by player commands.
Consumed and cleared on the next simulation tick or single-step.")

(defvar-local elcity-player--inhibit-confinement nil
  "Non-nil means cursor confinement is suppressed.
Set during render/update internals and isearch/minibuffer states.")

(defvar-local elcity-player--preview-overlays nil
  "List of active preview overlays.")

(defvar-local elcity-player--preview-tile nil
  "Last tile coords (X . Y) where preview was drawn, or nil.")

;;; ---------- Preview helpers ----------

(defun elcity-player-clear-preview ()
  "Remove all preview overlays."
  (dolist (ov elcity-player--preview-overlays)
    (delete-overlay ov))
  (setq elcity-player--preview-overlays nil
        elcity-player--preview-tile nil))

;;; ---------- Tool info helpers ----------

(defun elcity-player-tool-props (tool-kind)
  "Return metadata plist for TOOL-KIND, or nil when unknown."
  (cdr (assq tool-kind elcity-player-tool-specs)))

(defun elcity-player-tool-kinds ()
  "Return ordered player tool kinds."
  (mapcar #'car elcity-player-tool-specs))

(defun elcity-player-tool-key (tool-kind)
  "Return keybinding string for TOOL-KIND."
  (plist-get (elcity-player-tool-props tool-kind) :key))

(defun elcity-player-tool-command (tool-kind)
  "Return interactive selection command symbol for TOOL-KIND.
Derived by convention: `elcity-player-select-KIND'.
Signals an error when TOOL-KIND is not in `elcity-player-tool-specs'."
  (unless (elcity-player-tool-props tool-kind)
    (error "Unknown tool kind: %s" tool-kind))
  (intern (format "elcity-player-select-%s" tool-kind)))

(defun elcity-player-tool-footprint (tool-kind)
  "Return footprint plist for TOOL-KIND, or nil when not placeable."
  (plist-get (elcity-player-tool-props tool-kind) :footprint))

(defun elcity-player-tool-engine-anchor (tool-kind)
  "Return engine-anchor semantics for TOOL-KIND."
  (plist-get (elcity-player-tool-props tool-kind) :engine-anchor))

(defun elcity-player-tool-display-name (tool-kind)
  "Return display name for TOOL-KIND."
  (or (plist-get (elcity-player-tool-props tool-kind) :name)
      (symbol-name tool-kind)))

(defun elcity-player-tool-cost-display (tool-kind)
  "Return cost display string for TOOL-KIND."
  (if (eq tool-kind 'inspect)
      "-"
    (let ((entry (assq tool-kind elcity-building-cost-table)))
      (if entry
          (let ((cost (cdr entry)))
            (if (>= cost 1000)
                (format "$%dk" (/ cost 1000))
              (format "$%d" cost)))
        "?"))))

(defun elcity-player-tool-help-echo (tool-kind)
  "Return help-echo tooltip string for TOOL-KIND."
  (plist-get (elcity-player-tool-props tool-kind) :help))

;;; ---------- Tile coordinate helpers ----------

(defun elcity-player-tile-at-point ()
  "Return (TILE-X . TILE-Y) for the tile at point, or nil."
  (let ((pos (point))
        (map-start elcity-player--map-start-pos)
        (palette-start elcity-player--palette-start-pos))
    (when (and map-start palette-start
               (>= pos map-start) (< pos palette-start))
      (let* ((state (elcity-player-session-state elcity-player--session))
             (world (elcity-state-world state))
             (width (elcity-world-map-width world))
             (height (elcity-world-map-height world)))
        (if (elcity-player-render-tile-view-available-p)
            ;; Graphical: 2 chars per tile, newline after each row
            (let* ((offset (- pos map-start))
                   (row-len (1+ (* width 2)))
                   (tile-y (/ offset row-len))
                   (col (% offset row-len))
                   (tile-x (/ col 2)))
              (when (and (>= tile-x 0) (< tile-x width)
                         (>= tile-y 0) (< tile-y height))
                (cons tile-x tile-y)))
          ;; Text fallback: assumes elcity-player-render-world-lines format:
          ;;   "    0123..." header, then "%3d <tiles>" rows.
          (let* ((offset (- pos map-start))
                 (header-len (+ 4 width 1))
                 (body-offset (- offset header-len))
                 (row-len (+ 4 width 1))
                 (tile-y (/ body-offset row-len))
                 (col (- (% body-offset row-len) 4))
                 (tile-x col))
            (when (and (>= body-offset 0)
                       (>= tile-x 0) (< tile-x width)
                       (>= tile-y 0) (< tile-y height))
              (cons tile-x tile-y))))))))

(defun elcity-player-tile-to-pos (tile-x tile-y)
  "Return buffer position for tile at TILE-X, TILE-Y, or nil."
  (let ((map-start elcity-player--map-start-pos))
    (when map-start
      (let* ((state (elcity-player-session-state elcity-player--session))
             (world (elcity-state-world state))
             (width (elcity-world-map-width world)))
        (if (elcity-player-render-tile-view-available-p)
            ;; Graphical: 2 chars per tile, newline after each row
            (+ map-start (* tile-y (1+ (* width 2))) (* tile-x 2))
          ;; Text fallback: assumes elcity-player-render-world-lines format
          (let ((header-len (+ 4 width 1))
                (row-len (+ 4 width 1)))
            (+ map-start header-len (* tile-y row-len) 4 tile-x)))))))

(defun elcity-player-point-in-map-p ()
  "Return non-nil when point is within the tile map region."
  (and elcity-player--map-start-pos
       elcity-player--palette-start-pos
       (>= (point) elcity-player--map-start-pos)
       (< (point) elcity-player--palette-start-pos)))

(defun elcity-player-goto-tile (tile-x tile-y)
  "Move point to buffer position of tile at TILE-X, TILE-Y."
  (let ((pos (elcity-player-tile-to-pos tile-x tile-y)))
    (when pos
      (goto-char pos))))

;;; ---------- Region classification ----------

(defun elcity-player-region-at-pos (pos)
  "Return region symbol for buffer POS: `map', `status', `palette', or nil.
Returns nil when region markers are not yet initialized."
  (cond
   ((not (and elcity-player--map-start-pos
              elcity-player--palette-start-pos))
    nil)
   ((< pos elcity-player--map-start-pos)
    'status)
   ((< pos elcity-player--palette-start-pos)
    'map)
   (t
    'palette)))

;;; ---------- Action queue helpers ----------

(defun elcity-player-enqueue-action (action)
  "Append ACTION to the pending S10 queue preserving input order."
  (setq elcity-player--pending-actions
        (append elcity-player--pending-actions (list action))))

(defun elcity-player-pending-budget-percent (service)
  "Return most recent pending percent for SERVICE, or nil if none queued."
  (let (pending)
    (dolist (action elcity-player--pending-actions pending)
      (when (and (eq 'budget-percent (plist-get action :kind))
                 (eq service (plist-get action :service)))
        (setq pending (plist-get action :percent))))))

(defun elcity-player-effective-budget-percent (service)
  "Return effective budget percent for SERVICE considering pending actions."
  (or (elcity-player-pending-budget-percent service)
      (elcity-state-api-service-budget-percent
       (elcity-player-session-state elcity-player--session) service)))

(defun elcity-player-pending-tax-rate ()
  "Return most recent pending tax rate value, or nil if none queued."
  (let (pending)
    (dolist (action elcity-player--pending-actions pending)
      (when (eq 'tax-rate (plist-get action :kind))
        (setq pending (plist-get action :value))))))

(defun elcity-player-effective-tax-rate ()
  "Return effective tax rate considering pending actions."
  (or (elcity-player-pending-tax-rate)
      (elcity-state-api-tax-rate
       (elcity-player-session-state elcity-player--session))))

;;; ---------- State advance ----------

(defun elcity-player-advance-state ()
  "Advance session state by one tick, consuming pending actions.
Clears pending actions and undo ring.  Callers are responsible
for subsequent rendering."
  (let* ((state (elcity-player-session-state elcity-player--session))
         (actions elcity-player--pending-actions)
         (new-state (elcity-step state actions)))
    (setq elcity-player--pending-actions nil)
    (setf (elcity-player-session-state elcity-player--session) new-state)
    (setq elcity-player--undo-ring nil)))

(provide 'elcity-player-core)

;;; elcity-player-core.el ends here
