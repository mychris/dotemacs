;;; lsp-maybe.el --- Mabye start LSP                 -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Christoph Goettschkes

;; Author: Christoph Goettschkes

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'lsp-mode)

(defgroup lsp-maybe nil
  "Maybe start the Language Server Protocol client."
  :group 'tools
  :group 'convenience)

(defvar lsp-maybe-predicate-list
  nil
  "Lsp-maybe predicate list.
Use `lsp-maybe-add-predicate' to add predicates to the list."
  )

(defvar lsp-maybe-start-if-no-predicates
  t
  "Make `lsp-maybe' start `lsp' if the predicates list for the mode is empty.")

(defun lsp-maybe (&optional mode)
  "Maybe start the lsp client by checking `MODE' predicates.
Checks all the predicates in `lsp-maybe-predicate-list' and if all evaluate to
non-nil, calls `lsp'.  If `lsp-maybe-predicate-list' is empty for `MODE',
`lsp-maybe-start-if-no-predicates' is used to determine if the `lsp' should be
started or not."
  (interactive)
  (let* ((predicates (assq (or mode major-mode) lsp-maybe-predicate-list)))
    (when (or (and (not predicates) lsp-maybe-start-if-no-predicates)
              (and predicates (seq-reduce
                               (lambda (acc pred) (and acc (funcall pred)))
                               (car (cdr predicates))
                               t)))
      (lsp))))

(defun lsp-maybe-add-predicate (mode predicate)
  "Check the given `PREDICATE' to decide if lsp should be started in `MODE'."
  (let ((predicates (assq mode lsp-maybe-predicate-list)))
    (if (not predicates)
        (push `(,mode . (,(list predicate))) lsp-maybe-predicate-list)
      (setq lsp-maybe-predicate-list
            (cons (cons mode `(,(push predicate (car (cdr predicates)))))
                  (assq-delete-all mode lsp-maybe-predicate-list))))))

(provide 'lsp-maybe)

;;; lsp-maybe.el ends here
