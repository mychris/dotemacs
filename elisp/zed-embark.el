;;; zed-embark.el --- Extensions to embark -*- lexical-binding: t -*-

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

(eval-when-compile
  (require 'cl-lib))

(require 'embark)

(defun zed-embark-live-buffer-p ()
  "Answers if a linked embark live buffer exists or not."
  (when embark-collect-linked-buffer
	(window-live-p (get-buffer-window embark-collect-linked-buffer))))

(defun zed-embark-clear-all-collection-buffers ()
  "Clears all embark collection live buffers."
  (cl-mapc
   #'kill-buffer
   (cl-remove-if-not
	(lambda (buffer)
	  (string-match-p "^\\*[Ee]mbark.*[Cc]ompletions" (format "%s" buffer)))
	(buffer-list))))

(defun zed-embark-switch-to-completion-buffer ()
  "Switch to the embark completion buffer, creating one if it does not exist."
  (interactive)
  (unless (zed-embark-live-buffer-p)
	(embark-collect-completions))
  (select-window (get-buffer-window embark-collect-linked-buffer)))

(defun zed-embark-switch-to-completion-buffer-top ()
  "Switch to the top of the embark completion buffer, creating one if it does not exist."
  (interactive)
  (zed-embark-switch-to-completion-buffer)
  (goto-char (point-min)))

(provide 'zed-embark)
;;; zed-embark.el ends here
