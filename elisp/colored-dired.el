;; colored-dired --- More colors for dired -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (c) 2023 Christoph Göttschkes

;; Author: Christoph Göttschkes
;; Maintainer: Christoph Göttschkes
;; Created: 29 Jan 2023
;; Modified: 08 Mai 2023
;; Version: 0.2
;; Keywords: dired files

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

;; Enhances the DIRED output with more colors.


;;; Code:

(require 'dired)
(eval-when-compile
  (require 'rx))

(defgroup colored-dired nil
  "Enhances DIRED with more font-lock options."
  :prefix "colored-dired-"
  :group 'dired)


;;; Faces

(defgroup colored-dired-faces nil
  "Faces used by colored-dired."
  :group 'colored-dired
  :group 'faces)

(defface colored-dired-file-type
  '((t (:inherit font-lock-variable-name-face)))
  "Face for the file type."
  :group 'colored-dired-faces)
(defvar colored-dired-file-type-face 'colored-dired-file-type
  "Face name used for the file type information.")

(defface colored-dired-user-read
  '((t (:inherit font-lock-function-name-face)))
  "Face for the user read permission bit."
  :group 'colored-dired-faces)
(defvar colored-dired-user-read-face 'colored-dired-user-read
  "Face name used for the user read permission bit.")

(defface colored-dired-user-write
  '((t (:inherit font-lock-keyword-face)))
  "Face for the user write permission bit."
  :group 'colored-dired-faces)
(defvar colored-dired-user-write-face 'colored-dired-user-write
  "Face name used for the user write permission bit.")

(defface colored-dired-user-execute
  '((t (:inherit font-lock-string-face)))
  "Face for the user execute permission bit."
  :group 'colored-dired-faces)
(defvar colored-dired-user-execute-face 'colored-dired-user-execute
  "Face name used for the user execute permission bit.")

(defface colored-dired-user-sticky
  '((t (:inherit font-lock-type-face)))
  "Face for the user sticky permission bit."
  :group 'colored-dired-faces)
(defvar colored-dired-user-sticky-face 'colored-dired-user-sticky
  "Face name used for the user sticky permission bit.")

(defface colored-dired-group-read
  '((t (:inherit font-lock-function-name-face)))
  "Face for the group read permission bit."
  :group 'colored-dired-faces)
(defvar colored-dired-group-read-face 'colored-dired-group-read
  "Face name used for the group read permission bit.")

(defface colored-dired-group-write
  '((t (:inherit font-lock-keyword-face)))
  "Face for the group write permission bit."
  :group 'colored-dired-faces)
(defvar colored-dired-group-write-face 'colored-dired-group-write
  "Face name used for the group write permission bit.")

(defface colored-dired-group-execute
  '((t (:inherit font-lock-string-face)))
  "Face for the group execute permission bit."
  :group 'colored-dired-faces)
(defvar colored-dired-group-execute-face 'colored-dired-group-execute
  "Face name used for the group execute permission bit.")

(defface colored-dired-group-sticky
  '((t (:inherit font-lock-type-face)))
  "Face for the group sticky permission bit."
  :group 'colored-dired-faces)
(defvar colored-dired-group-sticky-face 'colored-dired-group-sticky
  "Face name used for the group sticky permission bit.")

(defface colored-dired-other-read
  '((t (:inherit font-lock-function-name-face)))
  "Face for the other read permission bit."
  :group 'colored-dired-faces)
(defvar colored-dired-other-read-face 'colored-dired-other-read
  "Face name used for the other read permission bit.")

(defface colored-dired-other-write
  '((t (:inherit font-lock-keyword-face)))
  "Face for the other write permission bit."
  :group 'colored-dired-faces)
(defvar colored-dired-other-write-face 'colored-dired-other-write
  "Face name used for the other write permission bit.")

(defface colored-dired-other-execute
  '((t (:inherit font-lock-string-face)))
  "Face for the other execute permission bit."
  :group 'colored-dired-faces)
(defvar colored-dired-other-execute-face 'colored-dired-other-execute
  "Face name used for the other execute permission bit.")

(defface colored-dired-other-sticky
  '((t (:inherit font-lock-type-face)))
  "Face for the other sticky permission bit."
  :group 'colored-dired-faces)
(defvar colored-dired-other-sticky-face 'colored-dired-other-sticky
  "Face name used for the other sticky permission bit.")

(defface colored-dired-links
  '((t (:inherit font-lock-comment-face)))
  "Face for the number of links."
  :group 'colored-dired-faces)
(defvar colored-dired-links-face 'colored-dired-links
  "Face name used for the number of links.")

(defface colored-dired-owner
  '((t (:inherit font-lock-property-name-face)))
  "Face for the owner."
  :group 'colored-dired-faces)
(defvar colored-dired-owner-face 'colored-dired-owner
  "Face name used for the owner of the file.")

(defface colored-dired-group
  '((t (:inherit font-lock-property-name-face)))
  "Face for the owner."
  :group 'colored-dired-faces)
(defvar colored-dired-group-face 'colored-dired-group
  "Face name used for group of the file.")

(defface colored-dired-file-size
  '((t (:inherit font-lock-string-face)))
  "Face for the file size."
  :group 'colored-dired-faces)
(defvar colored-dired-file-size-face 'colored-dired-file-size
  "Face name used for the file size.")

(defface colored-dired-modified
  '((t (:inherit font-lock-variable-name-face)))
  "Face for the last modification time."
  :group 'colored-dired-faces)
(defvar colored-dired-modified-face 'colored-dired-modified
  "Face name used for the last modification time.")


;;; Font-lock

(defconst colored-dired--re-mode
  (rx (regexp dired-re-maybe-mark)
      (regexp dired-re-inode-size)
      (or (group (any ?d ?l ?p ?s ?c ?b ?D)) ?-)
      (or (group ?r) nonl)
      (or (group ?w) nonl)
      (or (group ?x) (group ?s) nonl)
      (or (group ?r) nonl)
      (or (group ?w) nonl)
      (or (group ?x) (group ?s) nonl)
      (or (group ?r) nonl)
      (or (group ?w) nonl)
      (or (group ?x) (group ?t) nonl)
      (1+ whitespace)
      (group (1+ digit)) ;; links
      (1+ whitespace)
      (group alphabetic (0+ (not whitespace))) ;; owner
      (1+ whitespace)
      (group (? alphabetic (0+ (not whitespace)))) ;; group
      (1+ whitespace)
      (group digit (0+ (not whitespace))) ;; file size
      (1+ whitespace)
      (group (1+ (not whitespace)) ;; modification
	     (1+ whitespace)
	     (1+ (not whitespace)) ;; modification day
	     (1+ whitespace)
	     (1+ (not whitespace)))) ;; modification time
  "")

(defconst colored-dired-font-lock-keywords
  `(
    ;; File type
    (,colored-dired--re-mode
     (1 colored-dired-file-type-face keep t)
     (2 colored-dired-user-read-face keep t)
     (3 colored-dired-user-write-face keep t)
     (4 colored-dired-user-execute-face keep t)
     (5 colored-dired-user-sticky-face keep t)
     (6 colored-dired-group-read-face keep t)
     (7 colored-dired-group-write-face keep t)
     (8 colored-dired-group-execute-face keep t)
     (9 colored-dired-group-sticky-face keep t)
     (10 colored-dired-other-read-face keep t)
     (11 colored-dired-other-write-face keep t)
     (12 colored-dired-other-execute-face keep t)
     (13 colored-dired-other-sticky-face keep t)
     (14 colored-dired-links-face)
     (15 colored-dired-owner-face)
     (16 colored-dired-group-face)
     (17 colored-dired-file-size-face)
     (18 colored-dired-modified-face))))


;;; Commands

;;;###autoload
(defun colored-dired-setup-font-locking ()
  "Setup `colored-dired' font locking."
  (set (make-local-variable 'font-lock-defaults)
       ;; Two levels.  Use 3-element list, since it is standard to have one more
       ;; than the number of levels.  This is necessary for it to work with
       ;; `font(-lock)-menus.el'.
       '((dired-font-lock-keywords
          dired-font-lock-keywords
	  colored-dired-font-lock-keywords)
	 t nil nil beginning-of-line))
  ;; Refresh `font-lock-keywords' from `font-lock-defaults'
  (when (fboundp 'font-lock-refresh-defaults) (font-lock-refresh-defaults)))

(provide 'colored-dired)

;;; colored-dired.el ends here
