;; yamfile-mode.el --- Major mode for yam files -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (c) Christoph Göttschkes

;; Author: Christoph Göttschkes
;; Keywords: unix, tools
;; Version: 0.1

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

;;; Code:

(eval-when-compile
  (require 'rx))
(require 'regexp-opt)

(defvar yamfile-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    table)
  "Syntax table used in Yamfile mode buffers.")

(defconst yamfile-mode--macro-keywords-with-variable
  '("def" "cdef" "DEF" "CDEF"
    "add" "ADD"
    "undef"
    "for"))

(defconst yamfile-mode--macro-keywords-without-variable
  '("setenv" "include" "stop" "print" "if" "elif" "else" "endif"
    "switch" "case" "default" "endswitch"
    "break" "continue" "endfor" "nop"))

(defconst yamfile-mode--directive-regexp
  (rx line-start ?$ (zero-or-more whitespace)))

(defconst yamfile-mode--dependency-regexp
  (rx line-start
      (group-n 1
	(not (any ?# ?$ ?: whitespace ?\n))
	(+? any))
      ?:
      (or
       (any ?+ ?- whitespace)
       line-end)))

(defun yamfile-mode--var-use-match-from-to (from limit)
  "Search for variable use from FROM up to LIMIT for fontification."
  (when (and (> limit 0)
	     (> from 0)
	     (> limit from))
    (let* ((data (buffer-substring-no-properties from limit))
	   (var-start (string-search "{" data)))
      (when (not (null var-start))
	(let ((var-end var-start)
	      (counter 0))
	  (cl-loop for char across (substring data var-start) do
		   (cond
		    ((= char ?{)
		     (setq counter (1+ counter)))
		    ((= char ?})
		     (setq counter (1- counter)))
		    ((or (= char ?\r)
			 (= char ?\n)
			 (= char ?\t)
			 (= char ?\s))
		     (cl-return (yamfile-mode--var-use-match-from-to (+ from var-end) limit))))
		   (when (= 0 counter)
		     (goto-char from)
		     (cl-return (re-search-forward (concat "{\\(.\\{"
							   (number-to-string
							    (- var-end var-start 1))
							   "\\}\\)}")
						   nil t)))
		   (setq var-end (1+ var-end))))))))

(defun yamfile-mode--var-use-match (limit)
  "Search for variable use up to LIMIT for fontification."
  (yamfile-mode--var-use-match-from-to (point) limit))

(defvar yamfile-mode-font-lock-keywords
  `(
    ;; Macro expansion
    (yamfile-mode--var-use-match
     (1 font-lock-variable-name-face prepend))
    ;; Dependency rules
    (,yamfile-mode--dependency-regexp
     (1 font-lock-function-name-face append))
    ;; Preprocessing directives
    (,(rx (group-n 1
	    (regexp yamfile-mode--directive-regexp)
	    (regexp (regexp-opt yamfile-mode--macro-keywords-without-variable)))
	  (or word-boundary line-end))
     (1 font-lock-keyword-face))
    ;; Macro definition or undef, highlight variable name
    (,(rx (group-n 1
	    (regexp yamfile-mode--directive-regexp)
	    (regexp (regexp-opt yamfile-mode--macro-keywords-with-variable)))
	  (1+ whitespace)
	  (group-n 2 (1+ (not (any whitespace ?\( ?\) ))))
	  (or line-end ?\( ?\) whitespace))
     (1 font-lock-keyword-face)
     (2 font-lock-variable-name-face))))

(defvar yamfile-mode-imenu-generic-expression
  `(("Dependencies" yamfile-mode-previous-dependency 1)
    ("Macro definitions" ,(rx (group-n 1
				(regexp yamfile-mode--directive-regexp)
				(regexp (regexp-opt yamfile-mode--macro-keywords-with-variable)))
			      (1+ whitespace)
			      (group-n 2 (1+ (not (any whitespace ?\( ?\) ))))
			      (or line-end ?\( ?\) whitespace))
     2))
  "Imenu generic expression for Yamfile mode.  See `imenu-generic-expression'.")

;;;###autoload
(defun yamfile-mode-next-dependency ()
  "Move point to the beginning of the next dependency line."
  (interactive)
  (forward-line 1)
  (if (re-search-forward yamfile-mode--dependency-regexp nil t)
      (progn (beginning-of-line)
	     t)
    (progn (goto-char (point-max))
	   (beginning-of-line)
	   nil)))

;;;###autoload
(defun yamfile-mode-previous-dependency ()
  "Move point to the beginning of the previous dependency line."
  (interactive)
  (if (eq (point) (line-beginning-position))
      (beginning-of-line -1)
    (beginning-of-line))
  (when (not (looking-at-p yamfile-mode--dependency-regexp))
    (if (re-search-backward yamfile-mode--dependency-regexp nil t)
	(progn (beginning-of-line)
	       t)
      (progn (goto-char (point-min))
	     (beginning-of-line)
	     nil))))

(define-abbrev-table 'yamfile-mode-abbrev-table ()
  "Abbrev table in use in Yamfile buffers.")

(defvar-keymap yamfile-mode-map
  :doc "The keymap that is used in Yamfile mode."
  "C-c C-c" #'comment-dwim
  "C-M-i"   #'completion-at-point
  "M-n"     #'yamfile-mode-next-dependency
  "M-p"     #'yamfile-mode-previous-dependency
  )

(defalias 'yamfile-mode--parent-mode
  (if (fboundp 'prog-mode) 'prog-mode 'fundamental-mode))

;;;###autoload
(define-derived-mode yamfile-mode yamfile-mode--parent-mode "Yamfile"
  "Major mode for editing YAM files.

The hook `yamfile-mode-hook' is run with no args at mode
initiliazation."
  ;; Font lock.
  (set (make-local-variable 'font-lock-defaults)
       '(yamfile-mode-font-lock-keywords))
  ;; Comment stuff.
  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-start-skip) "#+[ \t]*")
  ;; Imenu.
  (set (make-local-variable 'imenu-generic-expression) yamfile-mode-imenu-generic-expression)
  ;; Other abbrevs.
  (setq local-abbrev-table yamfile-mode-abbrev-table)
  ;; TODO: Filling?
  ;; TODO: competion-at-point-functions?
  ;; TODO: write-file-functions?
  ;; Syntax table.
  (set-syntax-table yamfile-mode-syntax-table)
  ;; Make sure TAB really inserts \t.
  (set (make-local-variable 'indent-line-function) #'indent-to-left-margin)
  (set (make-local-variable 'indent-tabs-mode) t))

;;;###autoload
(add-to-list 'auto-mode-alist (cons (rx (or (seq
					     (or ?/ string-start)
					     (or ?Y ?y)
					     "amfile"
					     string-end)
					    (seq
					     ?. ?y ?a ?m
					     string-end)))
				    'yamfile-mode))

(provide 'yamfile-mode)


;;; yamfile-mode.el ends here
