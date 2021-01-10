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

;;; early-init.el ends here
