;;; elcity-player-ui.el --- Player display and render orchestration -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; Status bar, tool palette, tooltip formatting, and render
;; orchestration for the player subsystem.
;;
;; Depends on `elcity-player-core' for session struct, buffer-local
;; state, and tool metadata helpers.

;;; Code:

(require 'cl-lib)
(require 'elcity-budget)
(require 'elcity-budget-explain)
(require 'elcity-budget-explain-render)
(require 'elcity-inspect-model)
(require 'elcity-overlay)
(require 'elcity-player-core)
(require 'elcity-player-render)
(require 'elcity-time)
(require 'elcity-traffic)
(require 'elcity-world)

;;; ---------- Overview panel ----------

(defconst elcity-player-ui-overview--item-separator "  |  "
  "Separator between item labels within one status line.")

(defconst elcity-player-ui-overview--controls-hint
  "Run/Pause [space]  Budget [B] [+/-]  Overlay [m]  Tool [T]/[TAB]/[S-TAB]  Save [S]  Load [L]"
  "Persistent controls hint shown in status chrome.")

(defconst elcity-player-ui-overview--section-face
  '(:inherit mode-line-emphasis :weight bold)
  "Face for GUI section prefixes.")

(defconst elcity-player-ui-overview--value-box-face
  '(:box (:line-width (-1 . -1) :style flat-button))
  "Fallback chip face in non-GUI contexts.")

(defconst elcity-player-ui-overview--value-min-width 12
  "Minimum inner width used for GUI value chips.")

(defconst elcity-player-ui-overview--section-titles
  '("Economy" "Growth & Policy" "City Health" "Runtime")
  "Canonical section order for the player overview panel.")

(defun elcity-player-ui-overview-traffic-severity (label)
  "Map traffic LABEL text to overview severity."
  (pcase label
    ((or "none" "low" "moderate") 'normal)
    ("heavy" 'warning)
    ("severe" 'critical)
    (_ (error "Unknown traffic label: %S" label))))

(defun elcity-player-ui-overview-complaint-severity (value)
  "Map complaint VALUE (0..1000) to overview severity."
  (cond
   ((< value 400) 'normal)
   ((< value 700) 'warning)
   (t 'critical)))

(defun elcity-player-ui-overview-budget-severity (income treasury spending)
  "Map budget pressure from INCOME/TREASURY/SPENDING to overview severity."
  (cond
   ((>= income spending) 'normal)
   ((> treasury (* spending 6)) 'warning)
   (t 'critical)))

(defun elcity-player-ui-overview-severity-face (severity)
  "Return face for overview SEVERITY."
  (pcase severity
    ('normal nil)
    ('warning 'warning)
    ('critical 'error)
    (_ (error "Unknown overview severity: %S" severity))))

(defun elcity-player-ui--value-chip-face-gui ()
  "Return GUI chip face plist for overview values."
  (list
   :box '(:line-width (-1 . -1) :style flat-button)
   :background (or (face-background 'mode-line-inactive nil t)
                   (face-background 'highlight nil t)
                   (face-background 'default nil t)
                   "#d8d8d8")))

(defun elcity-player-ui--value-face (severity active)
  "Return composed value face for SEVERITY and ACTIVE."
  (let* ((gui-p (elcity-player-render-tile-view-available-p))
         (sev-face (elcity-player-ui-overview-severity-face severity))
         (base-face (if gui-p
                        (elcity-player-ui--value-chip-face-gui)
                      elcity-player-ui-overview--value-box-face))
         (active-face (if gui-p
                          (append (elcity-player-ui--value-chip-face-gui)
                                  '(:weight bold))
                        '(:box (:line-width (-1 . -1) :style flat-button)
                          :weight bold))))
    (cond
     ((and active sev-face) (list sev-face active-face))
     (active active-face)
     (sev-face (list sev-face base-face))
     (t base-face))))

(defun elcity-player-ui--value-chip-text (value)
  "Return VALUE text with consistent chip padding for the current UI mode."
  (let ((plain (format "%s" value)))
    (if (elcity-player-render-tile-view-available-p)
        (let ((target-width (max elcity-player-ui-overview--value-min-width
                                 (string-width plain))))
          (format (format " %%-%ds " target-width) plain))
      plain)))

(cl-defun elcity-player-ui--render-item
    (label value &key (severity 'normal) tooltip active label-width)
  "Render one overview LABEL/VALUE pair.
SEVERITY controls value emphasis and TOOLTIP sets help-echo text.
When ACTIVE is non-nil, render the value in active emphasis.
When LABEL-WIDTH is non-nil, pad LABEL before the value."
  (let* ((value-text (elcity-player-ui--value-chip-text value))
         (label-prefix
          (if label-width
              (let ((pad (max 0 (- label-width (string-width label)))))
                (concat label ":" (make-string (1+ pad) ?\s)))
            (concat label ": ")))
         (styled-value (propertize value-text
                                   'face (elcity-player-ui--value-face
                                          severity active)))
         (text (concat label-prefix styled-value)))
    (if tooltip
        (propertize text 'help-echo tooltip)
      text)))

(defun elcity-player-ui--render-budget-summary-value (parts active-control)
  "Render budget summary token PARTS and emphasize ACTIVE-CONTROL."
  (let* ((gui-p (elcity-player-render-tile-view-available-p))
         (content (mapconcat #'cdr parts " "))
         (value-text (elcity-player-ui--value-chip-text content))
         (styled (propertize value-text
                             'face (elcity-player-ui--value-face 'normal nil)))
         (offset (if gui-p 1 0)))
    (dolist (part parts)
      (let* ((kind (car part))
             (token (cdr part))
             (start offset)
             (end (+ start (length token))))
        (when (eq kind active-control)
          (add-face-text-property start end '(:weight bold) 'append styled))
        (setq offset (1+ end))))
    styled))

(defun elcity-player-ui--render-budget-summary-item
    (parts active-control tooltip &optional label-width)
  "Render compact budget summary from PARTS.
ACTIVE-CONTROL selects the emphasized token, TOOLTIP sets help-echo,
and LABEL-WIDTH pads the `T/I/F/P` label before its value."
  (let* ((prefix (if label-width
                     (let ((pad (max 0 (- label-width (string-width "T/I/F/P")))))
                       (concat "T/I/F/P:" (make-string (1+ pad) ?\s)))
                   "T/I/F/P: "))
         (item (concat
               prefix
               (elcity-player-ui--render-budget-summary-value parts active-control))))
    (if tooltip
        (propertize item 'help-echo tooltip)
      item)))

(defun elcity-player-ui--section-width ()
  "Return width used for GUI section prefixes."
  (apply #'max
         (mapcar #'string-width
                 (append elcity-player-ui-overview--section-titles
                         (list "Controls")))))

(defun elcity-player-ui--format-section-line (title items)
  "Render one overview section TITLE from list of item strings ITEMS."
  (let ((body (mapconcat #'identity
                         items
                         elcity-player-ui-overview--item-separator)))
    (if (elcity-player-render-tile-view-available-p)
        (concat (propertize
                 (format (format "[%%-%ds] "
                                 (elcity-player-ui--section-width))
                         title)
                 'face elcity-player-ui-overview--section-face)
                body)
      body)))

(defun elcity-player-ui--format-continuation-line (items)
  "Render wrapped continuation line from list of item strings ITEMS."
  (let ((body (mapconcat #'identity
                         items
                         elcity-player-ui-overview--item-separator)))
    (if (elcity-player-render-tile-view-available-p)
        (concat
         (make-string (+ 3 (elcity-player-ui--section-width)) ?\s)
         body)
      body)))

(defun elcity-player-ui--string-pad-right (text width)
  "Return TEXT padded with trailing spaces to WIDTH."
  (let ((pad (- width (string-width text))))
    (if (<= pad 0)
        text
      (concat text (make-string pad ?\s)))))

(defun elcity-player-ui--slot-widths (sections)
  "Return max rendered width for each item slot in SECTIONS."
  (let* ((max-slots (apply #'max
                           (mapcar (lambda (section)
                                     (length (cdr section)))
                                   sections)))
         (widths (make-vector max-slots 0)))
    (dolist (section sections)
      (let ((idx 0))
        (dolist (item (cdr section))
          (aset widths idx (max (aref widths idx)
                                (string-width item)))
          (setq idx (1+ idx)))))
    (append widths nil)))

(defun elcity-player-ui--pad-items-to-slot-widths (items slot-widths)
  "Pad ITEMS to SLOT-WIDTHS by slot index."
  (let ((idx 0)
        padded)
    (dolist (item items)
      (let ((slot-width (or (nth idx slot-widths)
                            (string-width item))))
        (push (elcity-player-ui--string-pad-right item slot-width) padded))
      (setq idx (1+ idx)))
    (nreverse padded)))

(defun elcity-player-ui--max-line-width (lines)
  "Return max `string-width' across LINES."
  (let ((width 0))
    (dolist (line lines)
      (setq width (max width (string-width line))))
    width))

(defun elcity-player-ui--map-width-columns (state)
  "Return rendered map width in columns for STATE."
  (if (elcity-player-render-tile-view-available-p)
      (let* ((tile-px (* elcity-player-render-tile-pixel-size
                         (elcity-player-render-tile-scale)))
             (char-px (max 1 (frame-char-width)))
             (tile-cols (max 1 (ceiling (/ tile-px (float char-px))))))
        (* tile-cols (elcity-world-map-width (elcity-state-world state))))
    (elcity-player-ui--max-line-width
     (elcity-player-render-world-lines state))))

(defun elcity-player-ui--content-width (state status-lines)
  "Return max content width from STATUS-LINES and map width for STATE."
  (max (elcity-player-ui--max-line-width status-lines)
       (elcity-player-ui--map-width-columns state)))

(defun elcity-player-ui--apply-layout-margins (content-width &optional window)
  "Apply symmetric margins so CONTENT-WIDTH is centered in WINDOW."
  (let* ((window (or window (selected-window)))
         (line-capacity (window-max-chars-per-line window))
         (current (window-margins window))
         (left (or (car current) 0))
         (right (or (cdr current) 0))
         ;; Keep margins only when content fits after reclaiming existing ones.
         (reclaimable-width (+ line-capacity left right))
         (margin (if (> content-width reclaimable-width)
                     0
                   (max 0 (/ (- reclaimable-width content-width) 2)))))
    ;; Keep the selected game window on the same display-table contract
    ;; as the buffer so truncation markers stay visually suppressed.
    (when (and buffer-display-table
               (window-live-p window)
               (eq (window-buffer window) (current-buffer)))
      (set-window-display-table window buffer-display-table))
    (unless (and (= left margin) (= right margin))
      (set-window-margins window margin margin))))

(defun elcity-player-ui-on-window-size-change (_frame)
  "Recompute player UI margins after a window size change."
  (when (and (derived-mode-p 'elcity-player-mode)
             elcity-player--session)
    (let ((window (get-buffer-window (current-buffer) t)))
      (when window
        (let* ((state (elcity-player-session-state elcity-player--session))
               (status-lines (elcity-player--render-status-lines
                              elcity-player--session)))
          (elcity-player-ui--apply-layout-margins
           (elcity-player-ui--content-width state status-lines)
           window))))))

(defun elcity-player-ui--build-tooltip-map (session)
  "Build a compact tooltip alist from SESSION state."
  (let* ((state (elcity-player-session-state session))
         (explanation (elcity-budget-explain-at state)))
    (list
     (cons 'treasury (elcity-budget-explain-format-treasury explanation))
     (cons 'income (elcity-budget-explain-format-income explanation))
     (cons 'spending (elcity-budget-explain-format-spend explanation))
     (cons 'demand (elcity-player--demand-tooltip state))
     (cons 'tax (elcity-player--tax-rate-tooltip state))
     (cons 'score (elcity-player--score-tooltip state))
     (cons 'population (elcity-player--population-tooltip state))
     (cons 'traffic (elcity-player--traffic-tooltip state)))))

(defun elcity-player-ui-overview-render-session-lines (session)
  "Render overview lines directly from SESSION."
  (let* ((state (elcity-player-session-state session))
         (gui-p (elcity-player-render-tile-view-available-p))
         (tool (elcity-player-session-active-tool session))
         (overlay-kind (elcity-player-session-overlay session))
         (budget-ctl (elcity-player-session-budget-control session))
         (tooltip-map (elcity-player-ui--build-tooltip-map session))
         (explanation (elcity-budget-explain-at state))
         (treasury-explain (elcity-budget-explanation-treasury explanation))
         (treasury (elcity-budget-treasury-explanation-treasury treasury-explain))
         (income (elcity-budget-treasury-explanation-tax-income treasury-explain))
         (spending (elcity-budget-treasury-explanation-total-spent treasury-explain))
         (tax-rate (elcity-player-effective-tax-rate))
         (tax-pending-p (not (null (elcity-player-pending-tax-rate))))
         (has-pending (or tax-pending-p
                          (cl-some #'elcity-player-pending-budget-percent
                                   elcity-budget-service-kinds)))
         (budget-parts
          (list
           (cons 'tax (format "T%d%%%s" tax-rate (if tax-pending-p "*" "")))
           (cons 'infrastructure
                 (format "I%d%%%s"
                         (elcity-player-effective-budget-percent 'infrastructure)
                         (if (elcity-player-pending-budget-percent 'infrastructure)
                             "*"
                           "")))
           (cons 'fire
                 (format "F%d%%%s"
                         (elcity-player-effective-budget-percent 'fire)
                         (if (elcity-player-pending-budget-percent 'fire)
                             "*"
                           "")))
           (cons 'police
                 (format "P%d%%%s"
                         (elcity-player-effective-budget-percent 'police)
                         (if (or (elcity-player-pending-budget-percent 'police)
                                 has-pending)
                             "*"
                           "")))))
         (traffic-label (elcity-player--traffic-severity-label state))
         (top-complaints (elcity-state-api-top-complaints state))
         (worst-complaint
          (if top-complaints
              (apply #'max
                     (mapcar (lambda (kind)
                               (elcity-state-api-complaint-severity state kind))
                             top-complaints))
            0))
         (budget-severity
          (elcity-player-ui-overview-budget-severity income treasury spending))
         (traffic-severity
          (elcity-player-ui-overview-traffic-severity traffic-label))
         (complaint-severity
          (elcity-player-ui-overview-complaint-severity worst-complaint))
         ;; Keep value columns aligned by slot across sections.
         (slot-label-widths
          (list
           (apply #'max (mapcar #'string-width '("Treasury" "Demand" "Score" "Tool")))
           (apply #'max (mapcar #'string-width '("Income" "T/I/F/P" "Population" "Date")))
           (apply #'max (mapcar #'string-width '("Spending" "Traffic" "Overlay")))))
         (economy-items
          (list
           (elcity-player-ui--render-item
            "Treasury" (format "$%d" treasury)
            :severity budget-severity
            :tooltip (cdr (assq 'treasury tooltip-map))
            :label-width (nth 0 slot-label-widths))
           (elcity-player-ui--render-item
            "Income" (format "$%d" income)
            :tooltip (cdr (assq 'income tooltip-map))
            :label-width (nth 1 slot-label-widths))
           (elcity-player-ui--render-item
            "Spending" (format "$%d" spending)
            :tooltip (cdr (assq 'spending tooltip-map))
            :label-width (nth 2 slot-label-widths))))
         (growth-items
          (list
           (elcity-player-ui--render-item
            "Demand"
            (format "R:%s C:%s I:%s"
                    (elcity-player--demand-direction
                     (elcity-state-api-demand-at state 'residential))
                    (elcity-player--demand-direction
                     (elcity-state-api-demand-at state 'commercial))
                    (elcity-player--demand-direction
                     (elcity-state-api-demand-at state 'industrial)))
            :tooltip (cdr (assq 'demand tooltip-map))
            :label-width (nth 0 slot-label-widths))
           (elcity-player-ui--render-budget-summary-item
            budget-parts budget-ctl (cdr (assq 'tax tooltip-map))
            (nth 1 slot-label-widths))))
         (health-items
          (list
           (elcity-player-ui--render-item
            "Score" (format "%d" (elcity-state-api-city-score state))
            :tooltip (cdr (assq 'score tooltip-map))
            :label-width (nth 0 slot-label-widths))
           (elcity-player-ui--render-item
            "Population" (format "%d" (elcity-state-api-total-population state))
            :tooltip (cdr (assq 'population tooltip-map))
            :label-width (nth 1 slot-label-widths))
           (elcity-player-ui--render-item
            "Traffic" traffic-label
            :severity traffic-severity
            :tooltip (cdr (assq 'traffic tooltip-map))
            :label-width (nth 2 slot-label-widths))
           (elcity-player-ui--render-item
            "Issues"
            (if top-complaints
                (mapconcat #'elcity-player--complaint-display-name
                           top-complaints ", ")
              "none")
            :severity complaint-severity
            :tooltip (cdr (assq 'score tooltip-map))
            :label-width (nth 0 slot-label-widths))))
         (runtime-items
          (list
           (elcity-player-ui--render-item
            "Tool"
            (format "%s (%s)"
                    (elcity-player-tool-display-name tool)
                    (elcity-player-tool-cost-display tool))
            :active t
            :label-width (nth 0 slot-label-widths))
           (elcity-player-ui--render-item
            "Date" (elcity-time-format (elcity-state-tick state))
            :label-width (nth 1 slot-label-widths))
           (elcity-player-ui--render-item
            "Overlay"
            (if overlay-kind
                (elcity-overlay-kind-label overlay-kind)
              "none")
            :label-width (nth 2 slot-label-widths))))
         (sections (list
                    (cons "Economy" economy-items)
                    (cons "Growth & Policy" growth-items)
                    (cons "City Health" health-items)
                    (cons "Runtime" runtime-items)))
         (slot-widths (and gui-p (elcity-player-ui--slot-widths sections)))
         lines)
    (setq lines
          (mapcan (lambda (section)
                    (let* ((items (if slot-widths
                                      (elcity-player-ui--pad-items-to-slot-widths
                                       (cdr section) slot-widths)
                                    (cdr section)))
                           (primary-items (if (and gui-p (> (length items) 3))
                                              (cl-subseq items 0 3)
                                            items))
                           (continuation-items
                            (and gui-p (> (length items) 3)
                                 (nthcdr 3 items))))
                      (if continuation-items
                          (list
                           (elcity-player-ui--format-section-line
                            (car section) primary-items)
                           (elcity-player-ui--format-continuation-line
                            continuation-items))
                        (list
                         (elcity-player-ui--format-section-line
                          (car section) primary-items)))))
                  sections))
    (append lines
            (list
             (if gui-p
                 (elcity-player-ui--format-section-line
                  "Controls"
                  (list elcity-player-ui-overview--controls-hint))
               (concat "Controls: " elcity-player-ui-overview--controls-hint))))))

;;; ---------- Congestion severity helpers ----------

(defun elcity-player--congestion-severity-label (value &optional effective-threshold)
  "Return player-facing severity label for congestion VALUE.
EFFECTIVE-THRESHOLD is the infra-dependent block threshold; defaults to
`elcity-traffic-congestion-cap' (240).  Severity bands are fractions of
the effective threshold: 0=none, >0=low, >25%=moderate, >50%=heavy,
>75%=severe."
  (let ((cap (or effective-threshold elcity-traffic-congestion-cap)))
    (cond
     ((<= value 0) "none")
     ((> value (/ (* cap 3) 4)) "severe")
     ((> value (/ cap 2)) "heavy")
     ((> value (/ cap 4)) "moderate")
     (t "low"))))

;;; ---------- Demand direction helpers ----------

(defconst elcity-player--demand-threshold 5
  "Threshold for demand direction indicators.")

(defun elcity-player--demand-direction (value)
  "Return direction character for demand VALUE."
  (cond
   ((> value elcity-player--demand-threshold) "+")
   ((< value (- elcity-player--demand-threshold)) "-")
   (t "=")))

;;; ---------- Traffic status helpers ----------

(defun elcity-player--traffic-severity-label (state)
  "Return city-wide traffic severity label from STATE."
  (elcity-player--congestion-severity-label
   (elcity-state-api-traffic-congestion-road-avg state)
   (elcity-state-api-traffic-effective-congestion-cap state)))

(defun elcity-player--traffic-tooltip (state)
  "Return traffic help-echo string from STATE."
  (let* ((avg (elcity-state-api-traffic-congestion-road-avg state))
         (ecap (elcity-state-api-traffic-effective-congestion-cap state))
         (label (elcity-player--congestion-severity-label avg ecap))
         (reachable (elcity-state-api-traffic-reachable-zones state))
         (blocked (elcity-state-api-traffic-blocked-zones state))
         (no-road (elcity-state-api-traffic-no-road-zones state)))
    (format "Traffic\nCity congestion: %d/%d (%s)\nZones reachable: %d  blocked: %d  no-road: %d\nBuild more roads to reduce congestion and improve zone access."
            avg ecap label
            reachable blocked no-road)))

;;; ---------- Status tooltip helpers ----------

(defun elcity-player--tax-rate-tooltip (state)
  "Return tax rate help-echo string from STATE."
  (let ((rate (elcity-state-api-tax-rate state)))
    (format "Tax Rate: %d%%\nRange: 0-%d%%. Higher taxes increase income but reduce zone demand.\nPress [B] to select, then [+/-] to adjust."
            rate elcity-budget-tax-rate-max)))

(defun elcity-player--demand-tooltip (state)
  "Return demand help-echo string from STATE."
  (let ((r (elcity-state-api-demand-at state 'residential))
        (c (elcity-state-api-demand-at state 'commercial))
        (i (elcity-state-api-demand-at state 'industrial)))
    (format "Zone Demand\nResidential: %s (%d)  Commercial: %s (%d)  Industrial: %s (%d)\n+ means growth pressure, = mostly stable, - means decline pressure.\nDemand helps only when zones have power and road access."
            (elcity-player--demand-direction r) r
            (elcity-player--demand-direction c) c
            (elcity-player--demand-direction i) i)))

(defun elcity-player--population-tooltip (state)
  "Return population help-echo string from STATE."
  (let ((total (elcity-state-api-total-population state))
        (r (elcity-state-api-residential-population state))
        (c (elcity-state-api-commercial-population state))
        (i (elcity-state-api-industrial-population state)))
    (format "Population\nTotal: %d (R:%d C:%d I:%d)\nPopulation comes from developed zones.\nHigher population increases tax income."
            total r c i)))

(defun elcity-player--complaint-display-name (kind)
  "Return player-facing display name for complaint KIND."
  (pcase kind
    ('power "Power")
    ('traffic "Traffic")
    ('housing "Housing")
    ('jobs "Jobs")
    ('taxes "Taxes")
    ('budget-services "Services")
    (_ (symbol-name kind))))

(defun elcity-player--score-tooltip (state)
  "Return score help-echo string from STATE."
  (let* ((score (elcity-state-api-city-score state))
         (approval (elcity-state-api-approval state))
         (complaints (elcity-state-api-top-complaints state))
         (issues (mapconcat #'elcity-player--complaint-display-name
                            complaints ", ")))
    (format "City Score\nScore: %d/1000  Approval: %d%%\nTop issues: %s\nFixing top issues improves score over time."
            score (/ approval 10) issues)))

;;; ---------- Status bar rendering ----------

(defun elcity-player--render-status-lines (session)
  "Return list of status bar text lines for SESSION."
  (elcity-player-ui-overview-render-session-lines session))

;;; ---------- Controls hint ----------

(defun elcity-player--render-budget-controls-hint (_session)
  "Return persistent controls hint."
  (concat "Controls: " elcity-player-ui-overview--controls-hint))

;;; ---------- Player tooltips ----------

(defun elcity-player--format-tooltip (info)
  "Return 5-section tooltip string from tile INFO.
Sections: Zone (name), Effect, Params, Quality, Improve."
  (let ((name (elcity-inspect-explain-name info))
        (effect (elcity-inspect-explain-effect info))
        (params (elcity-inspect-explain-params info))
        (quality (elcity-inspect-explain-quality info))
        (improve (elcity-inspect-explain-improve info)))
    (string-join
     (delq nil
           (list (and name (format "Zone: %s" name))
                 (and effect (format "Effect: %s" effect))
                 (and params (format "Params: %s" params))
                 (and quality (format "Quality: %s" quality))
                 (and improve (format "Improve: %s" improve))))
     "\n")))

(defun elcity-player-ui-tile-tooltip (state _tile x y power-scan connected-grid)
  "Return player-friendly tooltip for tile at X,Y in STATE.
_TILE is accepted for call-site compatibility.
POWER-SCAN and CONNECTED-GRID provide power status context."
  (let* ((scan (or power-scan
                   (elcity-state-api-power-scan state)))
         (connected (or connected-grid
                        (elcity-inspect-connected-conductive-grid state scan)))
         (info (elcity-inspect-tile-info-at state x y scan connected)))
    (elcity-player--format-tooltip info)))

;;; ---------- Buffer rendering ----------

(defmacro elcity-player--with-saved-tile (&rest body)
  "Execute BODY, preserving the cursor's tile position.
Saves the tile coordinates before BODY and restores point to the
same tile afterward.  Falls back to `point-min' when the cursor
is not on a map tile.  Suppresses confinement during BODY."
  (declare (indent 0))
  `(let ((saved-tile (elcity-player-tile-at-point))
         (elcity-player--inhibit-confinement t))
     ,@body
     (let ((restored (and saved-tile
                          (elcity-player-tile-to-pos
                           (car saved-tile) (cdr saved-tile)))))
       (goto-char (or restored (point-min))))))

(defun elcity-player-ui-render (session)
  "Render player UI for SESSION into current buffer."
  (let ((inhibit-read-only t)
        (state (elcity-player-session-state session))
        status-lines)
    (elcity-player--with-saved-tile
      (elcity-player-clear-preview)
      (erase-buffer)
      (setq status-lines (elcity-player--render-status-lines session))
      (elcity-player-ui--apply-layout-margins
       (elcity-player-ui--content-width state status-lines))
      ;; Status bar
      (insert (mapconcat #'identity
                         status-lines
                         "\n"))
      (insert "\n\n")
      ;; Tile map
      (setq elcity-player--map-start-pos (point))
      (if (elcity-player-render-tile-view-available-p)
          (elcity-player-render-insert-tile-map
           state
           #'elcity-player-ui-tile-tooltip
           (elcity-player-session-overlay session))
        ;; Text fallback
        (insert (mapconcat #'identity
                           (elcity-player-render-world-lines state)
                           "\n"))
        (insert "\n"))
      ;; Palette marker retained for chrome offset coherence.
      (setq elcity-player--palette-start-pos (point)))))

(defun elcity-player-ui-render-chrome (session)
  "Re-render only the status chrome for SESSION.
Leaves the tile map untouched for speed.
Falls back to full render if positions are not yet initialized."
  (if (not (and elcity-player--map-start-pos elcity-player--palette-start-pos))
      (elcity-player-ui-render session)
    (let ((inhibit-read-only t))
      (elcity-player--with-saved-tile
        ;; Replace status bar (from start to map)
        (let ((old-map-start elcity-player--map-start-pos))
          (delete-region (point-min) elcity-player--map-start-pos)
          (goto-char (point-min))
          (let* ((state (elcity-player-session-state session))
                 (status-lines (elcity-player--render-status-lines session))
                 (status-text (concat (mapconcat #'identity
                                                status-lines
                                                "\n")
                                     "\n\n")))
            (elcity-player-ui--apply-layout-margins
             (elcity-player-ui--content-width state status-lines))
            (insert status-text)
            ;; Adjust map-start, palette-start, and tooltip map-start for delta
            (let ((delta (- (point) old-map-start)))
              (setq elcity-player--map-start-pos (point))
              (cl-incf elcity-player--palette-start-pos delta)
              (elcity-player-render-set-tooltip-map-start (point)))))))))

(provide 'elcity-player-ui)

;;; elcity-player-ui.el ends here
