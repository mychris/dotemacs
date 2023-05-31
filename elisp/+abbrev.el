;; +abbrev.el --- Extension for abbrev -*- lexical-binding: t; -*-

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

;;   Extensions for abbrev.
;;
;;; Abbrev hooks
;;
;; There are several abbrev-hooks available to enrich the expansion of abbrevs.
;; All hooks have the `no-self-insert' property set, which means that the
;; character, which triggered the expansion will not be inserted into the buffer.
;; The following hooks are available:
;;
;;   `+abbrev-cursor-hook'
;; The expanded abbrev should have a '█' character in it.  If so, it will be
;; deleted and the cursor will be positioned at its location.
;;
;;   `+abbrev-yas-hook'
;;
;; Calls `yas-expand' after the abbrev expansion.  If a ya-snippet is defined
;; with a key of the expanded abbrev, the ya-snippet will be expanded as well.
;;
;;   `+abbrev-yas-inline-hook'
;;
;; Calls `yas-expand-snippet' with the expanded abbrev as its argument.  The
;; abbrev should expand to a ya-snippet body, which will then be expanded by
;; yas again.

;;; Code:

(require 'abbrev)
(require 's)
(require 'yasnippet nil 'noerror)

(defun +abbrev-cursor-hook ()
  "Function to run after abbrev expansion.
Replaces the first '█' character in the expansion and places the cursor at its
position."
  (let ((expansion (buffer-substring-no-properties last-abbrev-location (point))))
    (if-let ((bar-pos (s-index-of "█" expansion)))
	(progn
	  (backward-char (- (length expansion) bar-pos))
	  (delete-char 1))))
  t)

(put '+abbrev-cursor-hook 'no-self-insert t)

(defun +abbrev-yas-hook ()
  "Function to run after abbrev expansion.
Runs `yas-expand' to expand the yasnippet."
  (yas-expand))

(put '+abbrev-yas-hook 'no-self-insert t)

(defun +abbrev-yas-inline-hook ()
  "Function to run after abbrev expansion.
Runs `yas-expand-snippet' using the expanded abbrev as a template."
  (let ((expansion (buffer-substring-no-properties last-abbrev-location (point))))
    (yas-expand-snippet expansion last-abbrev-location (point))))

(put '+abbrev-yas-inline-hook 'no-self-insert t)



(provide '+abbrev)

;;; +abbrev.el ends here
