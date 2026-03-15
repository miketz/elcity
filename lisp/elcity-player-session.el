;;; elcity-player-session.el --- Player timer and simulation controls -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; Timer run-loop, simulation play/pause controls, overlay cycling,
;; and single-step for the player subsystem.
;;
;; Depends on `elcity-player-core' for session state and
;; `elcity-player-ui' for rendering.

;;; Code:

(require 'elcity)
(require 'elcity-overlay)
(require 'elcity-player-core)
(require 'elcity-player-ui)

;;; ---------- Timer run-loop ----------

(defun elcity-player-session-stop-timer ()
  "Stop and clear current player run-loop timer."
  (when (timerp elcity-player--timer)
    (cancel-timer elcity-player--timer)
    (setq elcity-player--timer nil)))

(defun elcity-player--start-timer ()
  "Start run-loop timer for current player buffer."
  (setq elcity-player--timer
        (run-with-timer 0
                        elcity-player-tick-interval
                        #'elcity-player--timer-tick
                        (current-buffer))))

(defun elcity-player-session-sync-timer ()
  "Synchronize timer state with current session RUNNING flag."
  (if (elcity-player-session-running elcity-player--session)
      (unless (timerp elcity-player--timer)
        (elcity-player--start-timer))
    (elcity-player-session-stop-timer)))

(defun elcity-player--timer-tick (buffer)
  "Advance player session by one tick for BUFFER timer callback."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (derived-mode-p 'elcity-player-mode)
                 elcity-player--session
                 (elcity-player-session-running elcity-player--session))
        (condition-case err
            (progn
              (elcity-player-advance-state)
              (elcity-player-ui-render elcity-player--session))
          (error
           (setf (elcity-player-session-running elcity-player--session) nil)
           (elcity-player-session-stop-timer)
           (message "Simulation error (paused): %S" err)))))))

(defun elcity-player-session-on-kill ()
  "Cleanup player run-loop resources for buffer shutdown."
  (elcity-player-session-stop-timer))

;;; ---------- Simulation controls ----------

(defun elcity-player-toggle-run ()
  "Toggle simulation play/pause."
  (interactive)
  (setf (elcity-player-session-running elcity-player--session)
        (not (elcity-player-session-running elcity-player--session)))
  (elcity-player-session-sync-timer)
  (elcity-player-ui-render-chrome elcity-player--session))

(defun elcity-player-cycle-overlay ()
  "Cycle map overlay: none -> pollution -> crime -> ... -> none."
  (interactive)
  (let* ((current (elcity-player-session-overlay elcity-player--session))
         (rest (when current (cdr (memq current elcity-overlay-kinds))))
         (next (if current
                   (car rest)  ; nil wraps back to none
                 (car elcity-overlay-kinds))))
    (setf (elcity-player-session-overlay elcity-player--session) next)
    ;; Full re-render since every tile border changes
    (elcity-player-ui-render elcity-player--session)
    (message "Overlay: %s" (if next (elcity-overlay-kind-label next) "none"))))

(defun elcity-player-single-step ()
  "Advance simulation by one tick while paused."
  (interactive)
  (when (elcity-player-session-running elcity-player--session)
    (user-error "Pause simulation first"))
  (elcity-player-advance-state)
  (elcity-player-ui-render elcity-player--session))

(provide 'elcity-player-session)

;;; elcity-player-session.el ends here
