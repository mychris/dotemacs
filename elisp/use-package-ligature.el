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
;; A hook to enable the `ligature-mode' will be added automatically for each
;; given mode.

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
  "Normalizes one list of ligature definition.
ARG holds the list of the ligatures, which can be prepended by a single mode,
or a list of modes the ligatures belong to.  If no mode is present in the ARG
list, NAME-SYMBOL is used as the mode for the given ligatures.

The result is a list of ligature lists.  Each ligature list contians the mode
the ligatures belong to as its first element.  If multiple modes are given in
the given ARG, the returned list contains multiple elements, one for each mode."
  (let ((ligatures nil)
	(modes nil))
    (cl-loop for elem in arg
	     do
	     (cond
	      ((null elem)
	       (use-package-ligature--error
		"Nil in ligature list"))
	      ((listp elem)
	       (setq modes (append elem modes)))
	      ((symbolp elem)
	       (if (null ligatures)
		   (push (use-package-ligature--normalize-mode elem) modes)
		 (use-package-ligature--error
		  "Mode symbols must be in the beginning of the ligature list")))
	      ((stringp elem)
	       (push elem ligatures))
	      (t
	       (use-package-ligature--error
		"Ligatures should be strings"))))
    (unless modes
      (push (use-package-ligature--normalize-mode name-symbol) modes))
    (setf modes (cl-remove-duplicates modes))
    (cl-loop for elem in modes
	     collect
	     (cons elem ligatures))))

(defun use-package-ligature--normalize (name-symbol _keyword args)
  "Hook for `use-package' to normalize the given ARGS for the :ligature keyword.
NAME-SYMBOL describes the package for which the ligatures are defined and will
be used, if no modes are given in a ligature list.
See `use-package-ligature--normalize-mode'."
  (let (result)
    (while args
      (let ((ligatures-for-modes (use-package-ligature--normalize-ligatures
				  name-symbol
				  (car args))))
	(while ligatures-for-modes
	  (let* ((ligatures (cdr (car ligatures-for-modes)))
		 (mode (car (car ligatures-for-modes))))
	    (if (not (assq mode result))
		(push (cons mode ligatures) result)
	      (setf (alist-get mode result)
		    (append ligatures (alist-get mode result)))))
	  (setq ligatures-for-modes (cdr ligatures-for-modes))))
      (setq args (cdr args)))
    result))

(defun use-package-ligature--handler (name-symbol _keyword args rest state)
  "Generate the `ligature-set-ligatures' invocations for the `:ligature' KEYWORD.
ARGS, REST, and STATE are prepared by `use-package-normalize/:ligatures'.
IF a ligature list does not specify any mode, NAME-SYMBOL will be used."
  (use-package-concat
   (use-package-process-keywords name-symbol rest state)
   `((eval-after-load 'ligature
       (progn
	 (require 'ligature)
	 ,@(mapcar #'(lambda (ligature-list)
		       `(ligature-set-ligatures ',(car ligature-list)
						',(cdr ligature-list)))
		   args)))
     ,@(mapcar #'(lambda (ligature-list)
		   `(add-hook ',(intern (concat (symbol-name (car ligature-list))
						"-hook"))
			      #'ligature-mode))
	       args))))

;;;###autoload
(defalias 'use-package-normalize/:ligatures 'use-package-ligature--normalize)

;;;###autoload
(defalias 'use-package-handler/:ligatures 'use-package-ligature--handler)

(add-to-list 'use-package-keywords :ligatures t)

(provide 'use-package-ligature)

;;; use-package-ligature.el ends here
