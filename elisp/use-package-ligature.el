;; use-package-ligaturue.el --- ligature support for use-package -*- lexical-binding: t; -*-

;; Copyright (c) 2023 Christoph Göttschkes

;; Author: Christoph Göttschkes
;; Maintainer: Christoph Göttschkes
;; Created: 26 Jan 2023
;; Modified: 26 Jan 2023
;; Version: 0.1
;; Keywords: convenience extension tools

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

;; Provides support for the :ligatures keyword to `use-package'.  Makes it more
;; convenient to specify ligates and enabling the `ligature-mode' for certain
;; major modes.

;; (require 'use-package-ligature)
;;
;; (use-package cc-mode
;;   :ligatures

;; Provides support for the :prettify keyword to `use-package'.  Makes it more
;; convenient to specify prettifying rules and enabling `prettify-symbols-mode'
;; for major modes.

;; (require 'use-package-prettify)
;;
;; (use-package cc-mode
;;   :lagitures
;;    ((c++-mode) "==" "=>" "=<" "!="))
;;
;; The provided ligatures are either a simple list of ligatures, which will be
;; used in the mode with the same name as the package.  If the package does not
;; end with -mode, it will be appended.
;;
;; If ligatures shoulde be defined for sereval modes, or a mode with a different
;; name, a list of symbols naming the mode is to be prepended.
;;
;; To provide different ligatures for different modes within one `use-package',
;; multiple lists can be providid.
;;
;; The `ligature-mode' will be enabled automatically for each given mode.

;;; Code:

(require 'cl-lib)
(require 'use-package-core)

(defun use-package-ligature--error (msg)
  "Report an `error' with the given MSG."
  (error (format "use-package-ligature: %s" msg)))

(defun use-package-ligature--normalize-mode (mode)
  "Normalize the given MODE.
If the given mode does not end with -mode, it will be appended."
  (when (symbolp mode)
    (setq mode (symbol-name mode)))
  (intern
   (cond
    ((string-suffix-p "-mode" mode)
     mode)
    (t
     (concat mode "-mode")))))

(defun use-package-ligature--normalize-ligatures (name-symbol arg)
  ""
  (let ((ligatures nil)
	(modes nil)
	(result nil))
    (while arg
      (cond
       ((null (car arg))
	(use-package-ligature--error
	 "Nil in ligature list"))
       ((proper-list-p (car arg))
	(setq modes (append modes (car arg))))
       ((symbolp (car arg))
	(unless ligatures
	  (use-package-ligature--error
	   "Ligatures should be strings"))
	(push (car arg) (use-package-ligature--normalize-mode modes)))
       ((stringp (car arg))
	(push (car arg) ligatures))
       (t
	(use-package-ligature--error
	 "Ligatures should be strings")))
      (setq arg (cdr arg)))
    (unless modes
      (push (use-package-ligature--normalize-mode name-symbol) modes))
    (setf modes (cl-remove-duplicates modes))
    (while modes
      (push (cons (car modes) ligatures) result)
      (setq modes (cdr modes)))
  result))

(defun use-package-ligature--normalize (name-symbol keyword args)
  ""
  (let ((rest (if (null (cdr args))
		  nil
		(use-package-ligature--normalize name-symbol keyword (cdr args))))
	(head (car args)))
    (if (not head)
	rest
      (cons (use-package-ligature--normalize-ligatures name-symbol head) rest))))

(use-package-ligature--normalize-ligatures 'cc-mode '(("==" "=>")))
(use-package-ligature--normalize 'cc-mode nil '(("==" "=>") ("=<")))

(defun use-package-ligature--handler (name-symbol _keyword args rest state)
  ""
  (use-package-concat
   (use-package-process-keywords name-symbol rest state)
   (cl-loop for arg in args
	    collect
	    (let ((mode (car arg))
		  (rules (cdr arg)))
	      (backquote
	       (add-hook
		(quote ,(intern (concat (symbol-name mode) "-hook")))
		(lambda ()
		  (setq prettify-symbols-alist
			(append prettify-symbols-alist (quote ,rules)))
		  (setq-local prettify-symbols-unprettify-at-point 'right-edge)

;;;###autoload
(defalias 'use-package-normalize/:ligatures 'use-package-ligature--normalize)

;;;###autoload
(defalias 'use-package-handler/:ligatures 'use-package-ligature--handler)

(add-to-list 'use-package-keywords :ligatures t)

(provide 'use-package-ligature)


;;; use-package-ligature.el ends here
