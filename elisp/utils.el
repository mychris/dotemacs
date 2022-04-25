;; utils.el --- Utility functions

;; Copyright (c) 2020-2022 Christoph Göttschkes

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

;; Utility functions.

;;; Code:

(defun my/point-in-string-p ()
  "Return non-nil if the point is in a string."
  (interactive)
  (nth 3 (syntax-ppss)))

(defun my/point-in-comment-p ()
  "Return non-nil if the point is in a comment."
  (interactive)
  (nth 4 (syntax-ppss)))

(defun my/point-in-string-or-comment-p ()
  "Return non-nil if the point is in a comment or string."
  (interactive)
  (or (my/point-in-string-p) (my/point-in-comment-p)))

(defun my/delete-most-recent-window ()
  "Delete the most recent window and kill its buffer."
  (interactive)
  (let ((most-recent-window (next-window))
        (most-recent-buffer (next-buffer)))
    (kill-buffer most-recent-buffer)
    (delete-window most-recent-window)))

;;; utils.el ends here
