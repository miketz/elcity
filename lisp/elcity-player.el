;;; elcity-player.el --- Player-facing interactive UI -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; Player gameplay interface for ElCity.
;;
;; Thin facade: assembles submodules, wires the keymap, defines the
;; major mode, and provides the public entrypoint.
;;
;; Submodules:
;;   elcity-player-core    — session struct, buffer-local state, tool metadata
;;   elcity-player-ui      — status bar, palette, tooltips, render orchestration
;;   elcity-player-session — timer, simulation controls (play/pause/step)
;;   elcity-player-input   — movement, preview, placement, budget, save/load, undo
;;
;; Public API:
;;   (elcity-player-start)  -> buffer
;;     Create a new-city buffer with `elcity-player-mode', displaying
;;     status bar, tile map, and tool palette.  Simulation starts paused.

;;; Code:

(require 'elcity)
(require 'elcity-player-core)
(require 'elcity-player-ui)
(require 'elcity-player-session)
(require 'elcity-player-input)

;;; ---------- Keymap ----------

(defun elcity-player--bind-tool-selection-keys (map)
  "Bind all tool selection keys into MAP.
Bindings come from `elcity-player-tool-specs'."
  (dolist (spec elcity-player-tool-specs)
    (let* ((kind (car spec))
           (key (elcity-player-tool-key kind))
           (command (elcity-player-tool-command kind)))
      (when (and key command)
        (define-key map (kbd key) command)))))

(defvar elcity-player-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    ;; Tool selection
    (elcity-player--bind-tool-selection-keys map)
    (define-key map (kbd "T") #'elcity-player-next-tool)
    (define-key map (kbd "TAB") #'elcity-player-next-tool)
    (define-key map (kbd "<backtab>") #'elcity-player-prev-tool)
    ;; Tile movement (edge-clamped)
    (define-key map (kbd "<right>") #'elcity-player-move-right)
    (define-key map (kbd "<left>") #'elcity-player-move-left)
    (define-key map (kbd "<up>") #'elcity-player-move-up)
    (define-key map (kbd "<down>") #'elcity-player-move-down)
    (define-key map (kbd "C-f") #'elcity-player-move-right)
    (define-key map (kbd "C-b") #'elcity-player-move-left)
    (define-key map (kbd "C-n") #'elcity-player-move-down)
    (define-key map (kbd "C-p") #'elcity-player-move-up)
    (define-key map (kbd "C-a") #'elcity-player-move-beginning-of-row)
    (define-key map (kbd "C-e") #'elcity-player-move-end-of-row)
    (define-key map (kbd "<home>") #'elcity-player-move-beginning-of-row)
    (define-key map (kbd "<end>") #'elcity-player-move-end-of-row)
    ;; Simulation controls
    (define-key map (kbd "SPC") #'elcity-player-toggle-run)
    (define-key map (kbd ".") #'elcity-player-single-step)
    ;; Budget controls
    (define-key map (kbd "B") #'elcity-player-cycle-budget-control)
    (define-key map (kbd "+") #'elcity-player-budget-increase)
    (define-key map (kbd "=") #'elcity-player-budget-increase)
    (define-key map (kbd "-") #'elcity-player-budget-decrease)
    ;; Overlay
    (define-key map (kbd "m") #'elcity-player-cycle-overlay)
    ;; Save/load controls
    (define-key map (kbd "S") #'elcity-player-save-game)
    (define-key map (kbd "L") #'elcity-player-load-game)
    ;; Placement
    (define-key map (kbd "RET") #'elcity-player-place-at-point)
    (define-key map (kbd "<down-mouse-1>") #'elcity-player-place-at-mouse)
    ;; Mouse immobility: suppress default point-moving behavior
    (define-key map (kbd "<mouse-1>") #'elcity-player-mouse-ignore)
    (define-key map (kbd "<mouse-2>") #'elcity-player-mouse-ignore)
    (define-key map (kbd "<mouse-3>") #'elcity-player-mouse-ignore)
    (define-key map (kbd "<down-mouse-2>") #'elcity-player-mouse-ignore)
    (define-key map (kbd "<down-mouse-3>") #'elcity-player-mouse-ignore)
    (define-key map (kbd "<drag-mouse-1>") #'elcity-player-mouse-ignore)
    ;; Undo
    (define-key map (kbd "u") #'elcity-player-undo)
    ;; Quit
    (define-key map (kbd "q") #'elcity-player-quit)
    map)
  "Keymap for `elcity-player-mode'.")

(defun elcity-player--sync-window-display-table (&optional window)
  "Apply current buffer display table to WINDOW when showing this buffer."
  (let ((window (or window (selected-window))))
    (when (and buffer-display-table
               (window-live-p window)
               (eq (window-buffer window) (current-buffer)))
      (set-window-display-table window buffer-display-table))))

;;; ---------- Major mode ----------

(define-derived-mode elcity-player-mode special-mode "ElCity"
  "Major mode for ElCity player interface."
  (setq-local truncate-lines t)
  (setq-local line-spacing 0)
  (setq-local mode-line-format nil)
  (setq-local left-fringe-width 0)
  (setq-local right-fringe-width 0)
  (setq-local fringes-outside-margins nil)
  (let ((table (copy-sequence (or buffer-display-table
                                  standard-display-table
                                  (make-display-table)))))
    (set-display-table-slot table 'truncation (vector ?\s ?\s))
    (set-display-table-slot table 'wrap (vector ?\s ?\s))
    (setq-local buffer-display-table table))
  (elcity-player--sync-window-display-table)
  (let ((indicators (copy-tree fringe-indicator-alist)))
    (setf (alist-get 'truncation indicators) '(nil nil))
    (setf (alist-get 'continuation indicators) '(nil nil))
    (setq-local fringe-indicator-alist indicators))
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (add-hook 'pre-command-hook #'elcity-player-input-pre-command nil t)
  (add-hook 'window-size-change-functions
            #'elcity-player-ui-on-window-size-change nil t)
  (add-hook 'post-command-hook #'elcity-player-input-post-command nil t))

;;; ---------- Entry point ----------

(defun elcity-player-start (&optional width height initial-tile)
  "Start a new ElCity game with optional WIDTH and HEIGHT.
When INITIAL-TILE is provided, terrain generation is skipped and all
tiles are set to INITIAL-TILE (used by test harnesses).
Creates a new-city buffer with `elcity-player-mode'.
Simulation starts paused.  Defaults to 64x64."
  (interactive)
  (let* ((state (elcity-make-initial-state nil
                                           (or width 64)
                                           (or height 64)
                                           initial-tile))
         (session (elcity-player-session-create
                   :state state
                   :active-tool 'inspect
                   :running nil))
         (buffer (get-buffer-create elcity-player-buffer-name)))
    (with-current-buffer buffer
      (elcity-player-session-stop-timer)
      (elcity-player-mode)
      (buffer-disable-undo)
      (setq-local elcity-player--session session)
      (add-hook 'kill-buffer-hook #'elcity-player-session-on-kill nil t))
    (switch-to-buffer buffer)
    (delete-other-windows)
    (with-current-buffer buffer
      (elcity-player--sync-window-display-table (selected-window))
      (elcity-player-ui-render session))
    buffer))

(provide 'elcity-player)

;;; elcity-player.el ends here
