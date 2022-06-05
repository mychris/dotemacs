;;; early-init.el --- Emacs early initialization      -*- lexical-binding: t -*-

;;; Commentary:

;; Emacs now starts the package manager before init.el is loaded.

;;; Code:

;; increase this early, decrease later on again
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

;; Initialise installed packages
(setq package-enable-at-startup nil)

;; Allow loading from the package cache.
(setq package-quickstart nil)

;; Do not resize the frame at this early stage.
(setq frame-inhibit-implied-resize t)

;; Some early customizations
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

;;; early-init.el ends here
