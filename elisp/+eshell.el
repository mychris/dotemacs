;; +eshell --- Extensions for eshell.el -*- coding:utf-8; lexical-binding: t; -*-

;; Copyright (c) Christoph Göttschkes

;; Author: Christoph Göttschkes
;; Maintainer: Christoph Göttschkes
;; Version: 0.1
;; Keywords: convenience

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

;;   Extensions for eshell.el

;;; Code:

(require 'my-utils)
(require 'eshell)
(require 'em-alias)
(require 'em-dirs)

;;;###autoload
(defun +eshell-toggle ()
  "Toggle an eshell buffer.

Switch to the most recently used eshell buffer, or to the most recently used
buffer, if currently in an eshell buffer."
  (interactive)
  (require 'eshell)
  (when (not (+switch-to-buffer-or-most-recent
	      (lambda (buffer)
		(string-prefix-p eshell-buffer-name (buffer-name buffer)))
	      (lambda (buffer)
		(switch-to-buffer buffer nil nil))))
    (eshell)))



(defun eshell/cd. ()
  "Alias for cd .."
  (eshell/cd ".."))

(defun eshell/cd.. ()
  "Alias for cd ../.."
  (eshell/cd.)
  (eshell/cd ".."))

(defun eshell/cd... ()
  "Alias for cd ../../.."
  (eshell/cd..)
  (eshell/cd ".."))

(defun eshell/cd.... ()
  "Alias for cd ../../../.."
  (eshell/cd...)
  (eshell/cd ".."))

(defun eshell/cd..... ()
  "Alias for cd ../../../../.."
  (eshell/cd....)
  (eshell/cd ".."))

(defun eshell/cd...... ()
  "Alias for cd ../../../../../.."
  (eshell/cd.....)
  (eshell/cd ".."))

(defun eshell/cd....... ()
  "Alias for cd ../../../../../../.."
  (eshell/cd......)
  (eshell/cd ".."))

(defun eshell/cd........ ()
  "Alias for cd ../../../../../../../.."
  (eshell/cd.......)
  (eshell/cd ".."))

(defun eshell/command (&rest args)
  "Execute a simple command or display information about commands.

Runs COMMAND with ARGS suppressing eshell function lookup, or display
information about the specified COMMANDs.  Can be used to invoke commands
on disk when a function with the same name exists.

Options:
      -v    print a description of COMMAND
      -V    print a more verbose description of each COMMAND"
  (let ((opt-v nil)
	(opt-V nil)
	(command nil)
	(command-args nil))
    ;; parse the arguments
    (while args
      (let ((arg (car args)))
	(setq args (cdr args))
	(cond
	 ((string= "--" arg)
	  ;; stop parsing options
	  (if (null command)
	      (setq command (car args)
		    command-args (cdr args))
	    (setq command-args (append command-args args)))
	  (setq args nil))
	 ((string-prefix-p "-" arg)
	  (let ((options (string-to-list (substring arg 1))))
	    (while options
	      (cond
	       ((equal (car options) ?v)
		(setq opt-v t))
	       ((equal (car options) ?V)
		(setq opt-V t))
	       (t
		(error (format "command: -%s: invalid option" (car options)))))
	      (setq options (cdr options)))))
	 (t
	  (if (null command)
	      (setq command arg)
	    (push arg command-args))))))
    ;; process the options and print command/execute command
    (cond
     ((or opt-V opt-v)
      (let* ((alias (eshell-lookup-alias command))
	     (command-symbol (intern (concat "eshell/" command)))
	     (fun (symbol-function command-symbol))
	     (fun-def fun))
	(cond
	 ((and alias opt-v)
	  (format "alias %s '%s'" (car alias) (string-join (cdr alias) " ")))
	 ((and alias opt-V)
	  (format "%s is aliased to `%s'" (car alias) (string-join (cdr alias) " ")))
	 ((and fun opt-v)
	  (format "%s" command))
	 ((and fun opt-V )
	  (while (symbolp fun-def)
	    (setq fun-def (symbol-function fun-def)))
	  (format "%s is a %s defined in %s\n%s"
		  command
		  (if (symbolp fun)
		      (format "function aliased to %s" (symbol-name fun))
		    "function")
		  (symbol-file command-symbol)
		  (if (not (listp fun-def))
		      (pp fun-def)
		    (with-temp-buffer
		      (pp-emacs-lisp-code fun-def)
		      (buffer-substring-no-properties (point-min) (point-max))))))
	 (t
	  (let ((external (locate-file command (eshell-get-path))))
	    (if (not external)
		(error (format "command: %s: not found" command))
	      (if opt-v
		  (format "%s" external)
		(format "%s is %s" command external))))))))
     (t
      (eshell-command-result (concat (char-to-string eshell-explicit-command-char)
				     command)
			     command-args)))))

(provide '+eshell)

;;; +eshell.el ends here
