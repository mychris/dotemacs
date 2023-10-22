;;; early-init.el --- Emacs early initialization      -*- lexical-binding: t -*-

;;; Commentary:

;; Emacs now starts the package manager before init.el is loaded.
;; Requires Emacs 27.1

;;; Code:

;; increase this early, decrease later on again
(custom-set-variables
 '(gc-cons-threshold most-positive-fixnum)
 '(gc-cons-percentage 0.6))

;; Initialize installed packages automatically.
;; Is done in init.el
(setq package-enable-at-startup nil)

;; Disable loading from the package cache.
;; Will be configured in init.el
(setq package-quickstart nil)

;; Do not resize the frame at this early stage.
(setq frame-inhibit-implied-resize t)

;; Some early customizations
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

;; Do not load values from X resources
(setq inhibit-x-resources t)

;; Disable site-start.el
(setq site-run-file nil)

;;; early-init.el ends here
