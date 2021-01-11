;;; early-init.el --- Emacs early initialization

;;; Commentary:

;; Emacs now starts the package manager before init.el is loaded.

;;; Code:

;; Initialise installed packages
(setq package-enable-at-startup t)

;; Allow loading from the package cache.
(setq package-quickstart t)

;; Do not resize the frame at this early stage.
(setq frame-inhibit-implied-resize t)

;; Some early customizations
(setq-default tool-bar-mode -1)  ;; this is too much
(setq-default menu-bar-mode 1)   ;; for colleagues
(setq-default scroll-bar-mode 1) ;; keep for colleagues

;;; early-init.el ends here
