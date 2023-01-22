;; use-package-prettify.el --- prettify support for use-package -*- lexical-binding: t; -*-

;; Copyright (c) 2023 Christoph Göttschkes

;; Author: Christoph Göttschkes
;; Maintainer: Christoph Göttschkes
;; Created: 21 Jan 2023
;; Modified: 22 Jan 2023
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

;; Provides support for the :prettify keyword to `use-package'.  Makes it more
;; convenient to specify prettifying rules and enabling `prettify-symbols-mode'
;; for major modes.

;; (require 'use-package-prettify)
;;
;; (use-package cc-mode
;;   :prettify
;;   (c++-mode
;;    ("lambda" . ?λ)))

;;; Code:

(require 'use-package-core)
(require 'cl-lib)

(defun use-package-prettify--normalize-rules-list (name input)
  "Normalize the INPUT pseudo-alist describing prettifying rules.
Elements in the list which are not cons pairs use the given NAME as a key.

For each element in the input, a normalized key is computed in the following
way:

If the key is \"c-mode-common\", use it as given.
If the key ends with \"-hook\", this suffix is removed.
If the key ends with \"-mode\", the key is used as given.
Otherwise, \"-mode\" is appended to the key.

This is also true for the given NAME, which is used if an element does not
define a key."
  (let (result)
    (cl-loop for element in input
             do
             (let* ((mode
                     (if (symbolp (car element))
                         (car element)
                       name))
                    (norm-mode
                     (cond
                      ((string= (symbol-name mode) "c-mode-common")
                       mode)
                      ((string-suffix-p "-hook" (symbol-name mode))
                       (intern
                        (string-remove-suffix "-hook" (symbol-name mode))))
                      ((string-suffix-p "-mode" (symbol-name mode))
                       mode)
                      (t
                       (intern (concat (symbol-name mode) "-mode")))))
                    (rules
                     (if (symbolp (car element))
                         (cdr element)
                       element)))
               (setf (alist-get norm-mode result)
                     (append rules (alist-get norm-mode result)))))
    result))

(defun use-package-prettify--normalize (name-symbol _keyword args)
  "Normalizer for the :prettify `use-package' keyword.
Normalizes the ARGS to be a list of prettifying rules.  Each set of rules can
have the mode for which it should be used as its first element.  If the mode is
missing, the given NAME-SYMBOL is used for those rules."
  (let ((result (use-package-prettify--normalize-rules-list name-symbol args)))
    result))

(defun use-package-prettify--handler (name-symbol _keyword args rest state)
  "Generate prettifying hooks for the `:prettify' KEYWORD.
ARGS, REST, and STATE are prepared by `use-package-normalize/:prettify'.  If the
prettifying rules specified in ARGS do not define a `mode' or `hook' they are
associated with, NAME-SYMBOL is used."
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
                  (prettify-symbols-mode t))))))))

;;;###autoload
(defalias 'use-package-normalize/:prettify 'use-package-prettify--normalize)

;;;###autoload
(defalias 'use-package-handler/:prettify 'use-package-prettify--handler)

(add-to-list 'use-package-keywords :prettify t)

(provide 'use-package-prettify)

;;; use-package-prettify.el ends here
