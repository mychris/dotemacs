;; my-utils.el --- Utility functions                 -*- lexical-binding: t; -*-

;; Copyright (c) 2020-2023 Christoph Göttschkes

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
	    (error
	     ;; Finally, check for distro specific release file
	     (file-exists-p "/etc/arch-release")))))
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
	    (error
	     ;; Finally, check for distro specific version file
	     (file-exists-p "/etc/debian_version")))))
       t))

(+linux-debian-p)

(defmacro +with-system (type &rest body)
  "Execute BODY if running an the system TYPE.
TYPE can be one of:
* `linux'       for any GNU/Linux distribution.
* `linux-arch'  for the GNU/Linux distribution Arch Linux."
  (let ((test-fun (intern (concat "+" (symbol-name type) "-p"))))
    `(when (and (fboundp #',test-fun) (funcall #',test-fun))
       ,@body)))

(provide 'my-utils)

;;; my-utils.el ends here
