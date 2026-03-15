;;; elcity.el --- ElCity -*- lexical-binding: t; -*-
;; Version: 0.2.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: games, simulation
;; URL: https://github.com/vkazanov/elcity

;; Copyright (C) 2026

;;; Commentary:

;; Public entrypoint for ElCity simulation modules.

;;; Code:

(require 'elcity-state)
(require 'elcity-world)
(require 'elcity-tile)
(require 'elcity-state-api)
(require 'elcity-actions)
(require 'elcity-budget)
(require 'elcity-demand)
(require 'elcity-evaluation)
(require 'elcity-power)
(require 'elcity-quality)
(require 'elcity-traffic)
(require 'elcity-zones)
(require 'elcity-sim)

(provide 'elcity)

;;; elcity.el ends here
