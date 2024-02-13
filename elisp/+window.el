;; +window.el --- Extensions for window -*- lexical-binding: t; -*-

;; Copyright (c) 2024 Christoph Göttschkes

;; Author: Christoph Göttschkes

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
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

(require 'window)
(require 'frame)

(defun +display-buffer-right-or-below-selected (buffer alist)
  "Try displaying BUFFER to the right of the selected window.
If there is a window to the left or to the right of the selected window,
display BUFFER below the selected window.

ALIST is an association list of action symbols and values."
  (if (or (window-left (selected-window))
	  (window-right (selected-window)))
      (display-buffer-below-selected buffer alist)
    (push '(direction . right) alist)
    (display-buffer-in-direction buffer alist)))

(defun +select-window-and-fit-to-buffer (window)
  "Combination of `select-window' and `fit-window-to-buffer' for WINDOW.
To be used as `body-function' in `display-buffer-alist'."
  (select-window window)
  (let ((fit-window-to-buffer-horizontally t))
    (fit-window-to-buffer window
			  (floor (frame-height) 3)
			  10
			  (floor (frame-width) 2.2)
			  36
			  nil)))

(provide '+window)


;;; +window.el ends here
