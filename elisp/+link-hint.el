;; +link-hint.el --- Extensions for link-hint -*- coding: utf-8; lexical-binding: t; -*-

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

;;   Extensions for link-hint.
;;
;; This package extends the functionalities provided by the link-hint package.
;; See https://github.com/noctuid/link-hint.el
;;
;; Currently, two new link types are defined:
;;
;;   c-include
;;   polarion-reference

;;; Code:


(require 'link-hint)

(defcustom +link-hint-polarion-url
  nil
  "Url of polarion."
  :type '(string)
  :group 'link-hint)

(defcustom +link-hint-jira-url
  nil
  "Url of Jira."
  :type '(string)
  :group 'link-hint)

(defcustom +link-hint-jira-project-keys
  nil
  "List of all Jira project keys."
  :type '(list string)
  :group 'link-hint)

(defconst +link-hint--c-include-regexp
  "^[[:space:]]*#[[:space:]]*include[[:space:]]+[\"<]\\([[:alnum:][:punct:]]+\\)[\">]"
  "Regular expression used to find C includes for `link-hint'.")


(defun +link-hint--next-c-include (bound)
  "Find the next C include.
Only search the range between just after the point and BOUND.

The search for C include is a bit fuzzy and assumes that the include is
well-formed."
  (save-excursion
    (let ((pt (point)))
      (ignore-errors (beginning-of-line))
      (if (and (re-search-forward +link-hint--c-include-regexp bound t)
	       (> (match-beginning 1) pt))
	  (match-beginning 1)
	(ignore-errors (goto-char pt))
	(when (and (re-search-forward +link-hint--c-include-regexp bound t)
		   (> (match-beginning 1) pt))
	  (match-beginning 1))))))

(defun +link-hint--c-include-at-point-p ()
  "Return the C include at the point or nil.

The search for C incude is a bit fuzzy and assumes that the include is
well-formed."
  (save-excursion
    (let ((pt (point)))
      (ignore-errors (beginning-of-line))
      (when (looking-at-p +link-hint--c-include-regexp)
	(re-search-forward +link-hint--c-include-regexp (point-max) t)
	(when (and (= (match-beginning 1) pt)
		   (button-at (match-beginning 1)))
	  (button-label (button-at (match-beginning 1))))))))

(link-hint-define-type 'c-include
  :next #'+link-hint--next-c-include
  :at-point-p #'+link-hint--c-include-at-point-p
  :open #'push-button
  :copy #'kill-new)


(defun +link-hint--next-polarion-reference (bound)
  "Find the next polarion reference.
Only search the range between just after the point and BOUND."
  (save-excursion
    (ignore-errors (forward-char))
    (when (re-search-forward "{[[:alpha:]]+-[[:digit:]]+}" bound t)
      (match-beginning 0))))

(defun +link-hint--polarion-reference-at-point-p ()
  "Return the polarion reference at the point or nil."
  (when (looking-at-p "{[[:alpha:]]+-[[:digit:]]+}")
    (match-string-no-properties 0)))

(defun +link-hint--parse-polarion-reference (reference action)
  "Alter polarion REFERENCE so that it can be passed to the ACTION function."
  (cond
   ((eq :describe action)
    reference)
   ((or (eq :open action)
	(eq :copy action))
    (let ((work-item-id (substring reference 1 -1)))
      (format "%s/#/search?query=id%%3A%s"
	      "http://polarion.tasking.nl/polarion"
	      work-item-id)))
   (t
    reference)))

(link-hint-define-type 'polarion-reference
  :next #'+link-hint--next-polarion-reference
  :at-point-p #'+link-hint--polarion-reference-at-point-p
  :parse #'+link-hint--parse-polarion-reference
  :predicates '((lambda () +link-hint-polarion-url))
  :vars '(c-mode)
  :open #'browse-url
  :copy #'kill-new)


(defun +link-hint--next-jira-reference (bound)
  "Find the next jira referenc.
Only search the range between just after the point and BOUND."
  (save-excursion
    (ignore-errors (forward-char))
    (when (re-search-forward
	   (concat "\\(?:"
		   (mapconcat #'identity +link-hint-jira-project-keys "|")
		   "\\)-[[:digit:]]+")
	   bound
	   t)
      (match-beginning 0))))

(defun +link-hint--jira-reference-at-point-p ()
  "Return the Jira reference at the point or nil."
  (when (looking-at-p
	 (concat "\\(?:"
		 (mapconcat #'identity +link-hint-jira-project-keys "|")
		 "\\)-[[:digit:]]+"))
    (match-string-no-properties 0)))

(defun +link-hint--parse-jira-reference (reference action)
  "Alter Jira REFERENCE so that it can be passed to the ACTION function."
  (cond
   ((eq :describe action)
    reference)
   ((or (eq :open action)
	(eq :copy action))
    (format "%s/browse/%s" +link-hint-jira-url reference))
   (t
    reference)))

(link-hint-define-type 'jira-reference
  :next #'+link-hint--next-jira-reference
  :at-point-p #'+link-hint--jira-reference-at-point-p
  :parse #'+link-hint--parse-jira-reference
  :predicates '((lambda () (and +link-hint-jira-url
				+link-hint-jira-project-keys)))
  :open #'browse-url
  :copy #'kill-new)


;;;###autoload
(defun +link-hint-setup ()
  "Setup `link-hint-types' provided by +link-hint."
  (dolist (type '(link-hint-c-include
		  link-hint-polarion-reference
		  link-hint-jira-reference))
    (delete type link-hint-types)
    (push type link-hint-types)))

(provide '+link-hint)

;;; +link-hint.el ends here
