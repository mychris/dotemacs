;; +dired.el --- Extension for dired   -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (c) 2023 Christoph Göttschkes

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

;;   Extensions for dired
;;
;; This package extends the functionality provided by the diread package.
;; Only small additions, which are not worth their own package.

;;; Code:



;;;###autoload
(defun +dired-maybe-insert-subdir-or-kill-subdir (dirname &optional
							  switches)
  (interactive
   (list (dired-get-filename 'verbatim t)
	 (if current-prefix-arg
	     (read-string "Switches for listing: "
			  (or dired-subdir-switches dired-actual-switches)))))
  (if (and (not dirname)
	   (save-excursion
	     (beginning-of-line)
	     (looking-at-p "^[[:space:]]\\{2\\}\\(.+:\\)[[:space:]]*$")))
      (call-interactively #'dired-kill-subdir)
    (call-interactively #'dired-maybe-insert-subdir)))



(provide '+dired)

;;; +dired.el ends here
