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

;; A major mode for editing yamfiles.  The mode is highly influenced by the
;; `makefile-mode'.

;;; Code:

(eval-when-compile
  (require 'rx)
  (require 'cl-lib))
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
  (rx line-start ?$ (zero-or-more blank)))

(defconst yamfile-mode--dependency-regexp
  (rx line-start
      (group-n 1
	(not (any ?# ?$ ?: blank ?\n))
	(+? nonl))
      ?:
      (or
       (any ?+ ?- blank)
       line-end)))

(defvar yamfile-mode--macro-table nil
  "Table of all macro names known for this buffer.")
(put 'yamfile-mode--macro-table 'risky-local-variable t)

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
	  (+ blank)
	  (group-n 2 (+? nonl))
	  (or line-end ?\( ?\) blank))
     (1 font-lock-keyword-face)
     (2 font-lock-variable-name-face))))

(defvar yamfile-mode-imenu-generic-expression
  `(("Dependencies" yamfile-mode-previous-dependency 1)
    ("Macro definitions" ,(rx (group-n 1
				(regexp yamfile-mode--directive-regexp)
				(regexp (regexp-opt yamfile-mode--macro-keywords-with-variable)))
			      (+ blank)
			      (group-n 2 (+? nonl))
			      (or line-end ?\( ?\) blank))
     2)
    ("Includes" ,(rx line-start ?$ "include"
			   (+ blank)
			   (group-n 1 (+? nonl))
			   (* blank)
			   line-end)
     1))
  "Imenu generic expression for Yamfile mode.  See `imenu-generic-expression'.")

;;;###autoload
(defun yamfile-mode-next-dependency (&optional arg)
  "Move point to the beginning of the next dependency line.
With an argument, do it ARG times.  If the argument is negative, the result is
equivalent to `yamfile-mode-previous-dependency' with -ARG as an argument."
  (interactive "p")
  (setq arg (or arg 1))
  (if (< arg 0)
      (yamfile-mode-previous-dependency (- arg))
    (cl-loop while (> arg 0) do
	     (cl-decf arg)
	     (forward-line 1)
	     (if (re-search-forward yamfile-mode--dependency-regexp nil t)
		 (beginning-of-line)
	       (progn
		 ;; First move forward to get an error
		 ;; if point is at the end of the buffer.
		 (forward-char 1)
		 (goto-char (point-max)))))))

;;;###autoload
(defun yamfile-mode-previous-dependency (&optional arg)
  "Move point to the beginning of the previous dependency line.
With an argument, do it ARG times.  If the argument is negative, the result is
equivalent to `yamfile-mode-next-dependency' with -ARG as an argument."
  (interactive "p")
  (setq arg (or arg 1))
  (if (< arg 0)
      (yamfile-mode-next-dependency (- arg))
    (cl-loop while (> arg 0) do
	     (cl-decf arg)
	     ;; First move backward to get to the previous line,
	     ;; if point is at the beginning of the line.
	     (backward-char 1)
	     (beginning-of-line)
	     (when (not (looking-at-p yamfile-mode--dependency-regexp))
	       (if (re-search-backward yamfile-mode--dependency-regexp nil t)
		   (beginning-of-line)
		 (progn
		   ;; First move backward to get an error
		   ;; if point is at the beginning of the buffer.
		   (backward-char 1)
		   (goto-char (point-min))))))))

(defun yamfile-mode--remember-macro (macro-name)
  "Store the given MACRO-NAME in `yamfile-mode--macro-table'."
  (when (not (zerop (length macro-name)))
    (when (not (assoc macro-name yamfile-mode--macro-table))
      (push (list macro-name) yamfile-mode--macro-table))))

(defun yamfile-mode-pickup-macros ()
  "Scan the buffer for all yam macro definitions.
The macro definitions are stored in `yamfile-mode--macro-table'."
  (interactive)
  (setq yamfile-mode--macro-table nil)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward (rx (group-n 1
				    (regexp yamfile-mode--directive-regexp)
				    (regexp (regexp-opt yamfile-mode--macro-keywords-with-variable)))
				  (+ blank)
				  (group-n 2 (+? nonl))
				  (or line-end ?\( ?\) blank))
			      nil t)
      (yamfile-mode--remember-macro (match-string-no-properties 2))
      (goto-char (match-end 2)))))

(defun yamfile-completions-at-point ()
  "Function used for `completion-at-point-functions' in `yamfile-mode'."
  (let ((beg (save-excursion
	       (skip-chars-backward "^{")
	       (point))))
    (when (char-equal ?{ (char-before beg))
      (list beg (point)
	    yamfile-mode--macro-table
	    :exit-function
	    (lambda (_s finished)
	      (when (memq finished '(sole finished))
		(if (looking-at (regexp-quote "}"))
		    (goto-char (match-end 0))
		  (insert "}"))))))))

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
  (add-hook 'completion-at-point-functions
	    #'yamfile-completions-at-point nil t)
  (make-local-variable 'yamfile-mode--macro-table)
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
