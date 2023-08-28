;; +bookmark.el --- Extensions for bookmark.el -*- coding: utf-8; lexical-binding: t; -*-

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

;;   Extensions for bookmark.el

;;; Code:


(require 'bookmark)

(defun +bookmark-prop-del (bookmark-name-or-record prop)
  "Delete the property PROP of BOOKMARK-NAME-OR-RECORD."
  (assq-delete-all prop (bookmark-get-bookmark-record bookmark-name-or-record)))

(defun +bookmark-del-position (bookmark-name-or-record)
  "Delete the position (i.e.: point) of BOOKMARK-NAME-OR-RECORD."
  (+bookmark-prop-del bookmark-name-or-record 'position))

(defun +bookmark-del-filename (bookmark-name-or-record)
  "Delete the full filename of BOOKMARK-NAME-OR-RECORD."
  (+bookmark-prop-del bookmark-name-or-record 'filename))

(defun +bookmark-del-front-context-string (bookmark-name-or-record)
  "Delete the front-context-string of BOOKMARK-NAME-OR-RECORD."
  (+bookmark-prop-del bookmark-name-or-record 'front-context-string))

(defun +bookmark-del-rear-context-string (bookmark-name-or-record)
  "Delete the rear-context-string of BOOKMARK-NAME-OR-RECORD."
  (+bookmark-prop-del bookmark-name-or-record 'rear-context-string))

(defun +bookmark-del-annotation (bookmark-name-or-record)
  "Delete the annotation of BOOKMARK-NAME-OR-RECORD."
  (+bookmark-prop-del bookmark-name-or-record 'annotation))

(defun +bookmark--bookmark-make-record-advice (bookmark-record)
  "Advice for `bookmark-make-record' altering the contents of BOOKMARK-RECORD.

If the position of the record is 1 (i.e. the beginning of the file), then the
position, front-context and rear-context are removed from the record.
This means that, when the bookmark is jumped to, the point will not be moved
to a certain position."
  (when (and bookmark-record
	     (= 1 (bookmark-get-position bookmark-record)))
    (+bookmark-del-position bookmark-record)
    (+bookmark-del-front-context-string bookmark-record)
    (+bookmark-del-rear-context-string bookmark-record))
  bookmark-record)

;;;###autoload
(defun +bookmark-load-default-contents ()
  "Load bookmarks from FILE."
  (let ((file bookmark-default-file)
	(enable-local-variables))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let ((blist (bookmark-alist-from-buffer)))
	(if (not (listp blist))
	    (error "Invalid bookmark list in %s" file)
	  (setq bookmark-alist blist
		bookmark-alist-modification-count 0)
	  (setq bookmark-bookmarks-timestamp
		(cons file (nth 5 (file-attributes file))))
	  (bookmark-bmenu-surreptitiously-rebuild-list)
	  (setq bookmark-file-coding-system buffer-file-coding-system))))))


;;;###autoload
(defun +bookmark-setup ()
  "Setup +bookmark."
  (advice-remove #'bookmark-make-record #'+bookmark--bookmark-make-record-advice)
  (advice-add #'bookmark-make-record :filter-return #'+bookmark--bookmark-make-record-advice))

(provide '+bookmark)

;;; +bookmark.el ends here
