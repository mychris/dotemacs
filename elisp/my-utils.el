;; my-utils.el --- Utility functions   -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (c) Christoph Göttschkes

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

;; Utility functions.

;;; Code:

(require 'subr-x)

(defun +point-in-string-p ()
  "Return non-nil if the point is in a string."
  (interactive)
  (nth 3 (syntax-ppss)))

(defun +point-in-comment-p ()
  "Return non-nil if the point is in a comment."
  (interactive)
  (nth 4 (syntax-ppss)))

(defun +point-in-string-or-comment-p ()
  "Return non-nil if the point is in a comment or a string."
  (interactive)
  (or (+point-in-string-p) (+point-in-comment-p)))

;;;###autoload
(defun +delete-most-recent-window ()
  "Delete the most recent window and kill its buffer."
  (interactive)
  (let ((most-recent-window (next-window))
	(most-recent-buffer (next-buffer)))
    (kill-buffer most-recent-buffer)
    (delete-window most-recent-window)))

;; Detect OS and distribution, at least for systems I use.
;; TODO: There should be some sort of caching here.

(defun +linux-p ()
  "Return non-nil if the OS is any GNU/Linux distribution."
  (eq system-type 'gnu/linux))

(defun +linux-arch-p ()
  "Return non-nil if the OS is the Arch Linux GNU/Linux distribution."
  (and (+linux-p)
       (condition-case nil
	   ;; Check the output of lsb_release
	   (with-temp-buffer
	     (let ((exit-code (call-process "lsb_release" nil (current-buffer) nil "-i"))
		   (output (string-trim (buffer-string))))
	       (and (= 0 exit-code)
		    (string-match-p "ID:[[:space:]]+Arch$" output))))
	 (error
	  (condition-case nil
	      ;; Check the content of os-release
	      (with-temp-buffer
		(insert-file-contents "/etc/os-release")
		(if (not (re-search-forward "^ID="))
		    nil
		  (string= "arch"
			   (string-trim (buffer-substring-no-properties
					 (point)
					 (line-end-position))))))
	    (error nil))))
       t))

(defun +linux-debian-p ()
  "Return non-nil if the OS is the Debian GNU/Linux distribution."
  (and (+linux-p)
       (condition-case nil
	   ;; Check the output of lsb_release
	   (with-temp-buffer
	     (let ((exit-code (call-process "lsb_release" nil (current-buffer) nil "-i"))
		   (output (string-trim (buffer-string))))
	       (and (= 0 exit-code)
		    (string-match-p "ID:[[:space:]]+Debian$" output))))
	 (error
	  (condition-case nil
	      ;; Check the content of os-release
	      (with-temp-buffer
		(insert-file-contents "/etc/os-release")
		(if (not (re-search-forward "^ID="))
		    nil
		  (string= "debian"
			   (string-trim (buffer-substring-no-properties
					 (point)
					 (line-end-position))))))
	    (error nil))))
       t))

(defun +linux-ubuntu-p ()
  "Return non-nil if the OS is the Ubuntu GNU/Linux distribution."
  (and (+linux-p)
       (condition-case nil
	   ;; Check the output of lsb_release
	   (with-temp-buffer
	     (let ((exit-code (call-process "lsb_release" nil (current-buffer) nil "-i"))
		   (output (string-trim (buffer-string))))
	       (and (= 0 exit-code)
		    (string-match-p "ID:[[:space:]]+Ubuntu$" output))))
	 (error
	  (condition-case nil
	      ;; Check the content of os-release
	      (with-temp-buffer
		(insert-file-contents "/etc/os-release")
		(if (not (re-search-forward "^ID="))
		    nil
		  (string= "ubuntu"
			   (string-trim (buffer-substring-no-properties
					 (point)
					 (line-end-position))))))
	    (error nil))))
       t))

(defun +bsd-p ()
  "Return non-nil if the OS is a Berkeley Unix distribution."
  (eq system-type 'berkeley-unix))

(defun +freebsd-p ()
  "Return non-nil if the OS is FreeBSD."
  (and (+bsd-p)
       (condition-case nil
	   ;; Check the content of /var/run/os-release
	   (with-temp-buffer
	     (insert-file-contents "/var/run/os-release")
	     (if (not (re-search-forward "^ID="))
		 nil
	       (string= "freebsd"
			(string-trim (buffer-substring-no-properties
				      (point)
				      (line-end-position))))))
	 (error
	  (condition-case nil
	      ;; Check the content of /etc/os-release
	      (with-temp-buffer
		(insert-file-contents "/etc/os-release")
		(if (not (re-search-forward "^ID="))
		    nil
		  (string= "freebsd"
			   (string-trim (buffer-substring-no-properties
					 (point)
					 (line-end-position))))))
	    (error
	     (condition-case nil
		 ;; Check if freebsd-version exits successfully
		 (let ((exit-code (call-process "freebsd-version" nil nil nil)))
		   (eq exit-code 0))
	       (error nil))))))
       t))

(defmacro +with-system (type &rest body)
  "Execute BODY if running on the system TYPE.
TYPE can be one of:
* `linux'         for any GNU/Linux distribution.
* `linux-arch'    for the GNU/Linux distribution Arch Linux.
* `linux-debian'  for the GNU/Linux distribution Debian.
* `linux-ubuntu'  for the GNU/Linux distribution Debian.
* `bsd'           for any Berkeley Unix distribution.
* `freebsd'       for FreeBSD."
  (let ((test-fun (intern (concat "+" (symbol-name type) "-p"))))
    `(when (and (fboundp #',test-fun) (funcall #',test-fun))
       ,@body)))

;; Window urgency hint

(defun +frame-urgency-hint-set-x11 (frame arg)
  "Set the X11 urgency hint for the given FRAME to ARG.
If ARG is nil, unset the urgency hint, otherwise set it."
  (let* ((wm-flag-urgent #x100)
	 (wm-hints (append (x-window-property "WM_HINTS" frame "WM_HINTS" nil nil t) nil))
	 (flags (or (car wm-hints) 0))
	 (flags (if arg (logior flags wm-flag-urgent)
		  (logand flags (lognot wm-flag-urgent)))))
    (if wm-hints
	(setcar wm-hints flags)
      (setq wm-hints (list flags)))
    (x-change-window-property "WM_HINTS" wm-hints frame "WM_HINTS" 32 t nil)))

;;;###autoload
(defun +frame-urgency-hint (&optional arg)
  "Mark/Unmark the current Emacs frame as urgent.

Without any ARG, or if ARG is nil, mark the frame, otherwise unmark it."
  (interactive "P")
  (let* ((frame (selected-frame))
	 (current-window-system (window-system frame)))
    (cond
     ((null current-window-system)
      ;; There is such thing for a real terminal
      )
     ((eq current-window-system 'x)
      (+frame-urgency-hint-set-x11 frame arg))
     (t (warn "+frame-urgency-hint unavailable for the %s window system"
	      (symbol-name current-window-system))))))

(defun +switch-to-buffer-or-most-recent (predicate &optional switch-fn)
  "Switch to the buffer satisfying PREDICATE using SWITCH-FN.

If the current buffer satisfies the given PREDICATE, switch to the most recent
buffer.  Otherwise, try to find a buffer which satisfies the PREDICATE and
switch to that one.

Returns the buffer switched to, or nil if no target buffer was found.

If SWITCH-FN is not given, `switch-to-buffer' is used."
  (setq switch-fn (or switch-fn #'switch-to-buffer))
  (let ((buffers (buffer-list)))
    (if (funcall predicate (current-buffer))
	(if (nth 2 buffers)
	    (funcall switch-fn (nth 2 buffers))
	  (current-buffer))
      (let ((target-buffer (cl-find-if
			    predicate
			    buffers)))
	(if target-buffer
	    (funcall switch-fn target-buffer)
	  nil)))))

(defun +transform-thing-at-point (thing fun)
  "Transform the THING at point using FUN.
FUN being a function accepting one argument, which is the string representation
of the THING at point.
Returns the transformed THING at point, or nil, if THING could not be found.
See `bounds-of-thing-at-point' for more information about THING."
  (if-let ((boundaries (bounds-of-thing-at-point thing)))
      (letrec ((start (car boundaries))
	       (end (cdr boundaries))
	       (transformed-thing
		(funcall fun (buffer-substring-no-properties
			      start
			      end))))
	(delete-region start end)
	(goto-char start)
	(insert transformed-thing)
	transformed-thing)))

(defun +apply-thing-at-point (thing fun)
  "Apply the given FUN to THING at point.
See `bounds-of-thing-at-point' for more infromation about THING."
  (if-let ((boundaries (bounds-of-thing-at-point thing)))
      (funcall fun (buffer-substring-no-properties
		    (car boundaries) (cdr boundaries)))))

(provide 'my-utils)

;;; my-utils.el ends here
