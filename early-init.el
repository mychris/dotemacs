;;; early-init.el --- Emacs early initialization

;;; Commentary:

;; Emacs now starts the package manager before init.el is loaded.

;;; Code:

;; increase this early, decrease later on again
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

;; Initialise installed packages
(setq package-enable-at-startup t)

;; Allow loading from the package cache.
(setq package-quickstart t)

;; Do not resize the frame at this early stage.
(setq frame-inhibit-implied-resize t)

;; Some early customizations
(tool-bar-mode -1)   ;; this is too much
(menu-bar-mode 1)    ;; for colleagues
(scroll-bar-mode -1) ;; this is too much

;;; early-init.el ends here
