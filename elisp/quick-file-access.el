;;; quick-file-access.el --- Quickly access important files

;; Copyright (C) 2020-2021 Christoph Göttschkes

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

;; Functions to open important files.  This package exists so it is easier
;; to group the key bindings in one use-package declaration and to give
;; make the key-binding easier to inspect by packages like which-key.

;;; Code:

(defgroup quick-file-access ()
  "Quickly access important files."
  :group 'convenience)

(defcustom quick-file-access-file-list '()
  "List of files to make available."
  :type '(repeat string)
  :group 'quick-file-access)

(defvar quick-file-access--history nil
  "The history of files which have been accessed.")

(defmacro quick-file-access--historicize (file)
  "Put the FILE into the quick-file-access completion history."
  `(or
	(called-interactively-p 'interactive)
	(setq quick-file-access--history
		  (if (seq-position file quick-file-access-file-list)
			  (cons ,file (delete ,file quick-file-access--history))
			(cons ,file quick-file-access--history)))))

(defun quick-file-access-completing-read ()
  "Completing-read the 'quick-file-access-file-list'."
  (completing-read
   "quick-file-access: "
   (lambda (string pred action)
	 (if (eq action 'metadata)
		 '(metadata (category . file))
	   (complete-with-action
		action quick-file-access-file-list string pred)))
   nil 0 nil 'quick-file-access--history nil))

(defun quick-file-access-open-file (file)
  "Open FILE, or complete it from the 'quick-file-access-file-list'."
  (interactive
   (list (quick-file-access-completing-read)))
  (unless file
	(error "No file specified"))
  (quick-file-access--historicize file)
  (find-file file))

(provide 'quick-file-access)

;;; quick-file-access.el ends here
