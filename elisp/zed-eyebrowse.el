;;; zed-eyebrowse.el --- Extensions to eyebrowse -*- lexical-binding: t -*-

;; Copyright (C) 2020-2021 Christoph Göttschkes

;; Author: Christoph Göttschkes
;; Keywords: extensions

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(defgroup zed-eyebrowse nil
  "Extensions for eyebrowse."
  :group 'convenience
  :prefix "zed-eyebrowse-")

(defvar zed-eyebrowse--saved-window-configuration nil
  "Saved window configuration for monocle view."
  )

;;;###autoload
(define-minor-mode zed-eyebrowse-monocle
  "Toggle between multiple windows and single window (monocle view).

This mode should not be enabled manually, but by using
'zed-eyebrowse-toggle-monocle'.

In addition, there are two different functions to disable the mode,
which must be called by specific hooks.
See the doc for 'zed-eyebrowse-monocle-window-config-change'
and 'zed-eyebrowse-monocle-window-config-switch'."
  :lighter " °M°"
  :global nil)

(defun zed-eyebrowse-toggle-monocle ()
  "Toggle monocle."
  (interactive)
  (if (bound-and-true-p zed-eyebrowse-monocle)
	  (zed-eyebrowse-monocle-window-config-switch)
	(when (not (one-window-p))
	  (setq zed-eyebrowse--saved-window-configuration (current-window-configuration))
	  (delete-other-windows)
	  (zed-eyebrowse-monocle 1))))

(defun zed-eyebrowse-monocle-window-config-change ()
  "Disable monocle - must be hooked to 'window-configuration-change-hook'.

Only disables the monocle mode if there are more than one window.  Reason for
this is, that the 'window-configuration-change-hook' is also called when the
monocle mode is being enabled."
  (interactive)
  (when (and (bound-and-true-p zed-eyebrowse-monocle) (not (one-window-p)))
    (delete-other-windows)
    (zed-eyebrowse-monocle -1)
    (set-window-configuration zed-eyebrowse--saved-window-configuration)
	(setq zed-eyebrowse--saved-window-configuration nil)))

(defun zed-eyebrowse-monocle-window-config-switch ()
  "Disable monocle - must be hooked to 'eyebrowse-pre-window-switch-hook'.

Disables the monocle mode unconditionally."
  (interactive)
  (when (bound-and-true-p zed-eyebrowse-monocle)
	(zed-eyebrowse-monocle -1)
	(delete-other-windows)
    (set-window-configuration zed-eyebrowse--saved-window-configuration)
	(setq zed-eyebrowse--saved-window-configuration nil)))

(provide 'zed-eyebrowse)
;;; zed-eyebrowse.el ends here
