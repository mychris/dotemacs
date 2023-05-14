;; +consult.el --- Extensions of consult -*- coding: utf-8; lexical-binding: t; -*-

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

;;   Extensions for consult
;;
;; This package extends the functionality provided by the consult package.
;; Only small additions, which are not worth their own package.

;;; Code:


(require 'consult)

(defvar +consult--major-mode-menu-history nil)

(defun +consult--major-mode-candidates ()
  "Return list of major-mode candidate string."
  (mapcar
   (pcase-lambda (`(,name . ,sym))
     (propertize
      name
      'consult--candidate sym))
   (mapcar (lambda (mm) (if (symbolp (cdr mm))
			    (cons (symbol-name (cdr mm)) (cdr mm))
			  (cons (symbol-name (cadr mm)) (cadr mm))))
	   auto-mode-alist)))

;;;###autoload
(defun +consult-major-mode-menu ()
  "Enable a major mode."
  (interactive)
  (call-interactively
   (consult--read
    (+consult--major-mode-candidates)
    :prompt "Major mode: "
    :require-match t
    :category 'major-mode
    :lookup #'consult--lookup-candidate
    :history '+consult--major-mode-menu-history)))

;;; +consult.el ends here
