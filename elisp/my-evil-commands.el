;; utils.el --- Utility functions                    -*- lexical-binding: t; -*-

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

;;; Code:

(require 'evil)
(require 'cl-lib)

(evil-define-command evil-goto-mark-center (char &optional noerror)
  "Go to the marker specified by CHAR and scroll to the center."
  :keep-visual t
  :repeat nil
  :type exclusive
  :jump t
  (interactive (list (read-char)))
  (let ((stored-point (point)))
    (evil-goto-mark char noerror)
    (when (not (equal (point) stored-point))
      (evil-scroll-line-to-center nil))))

(evil-define-command evil-goto-mark-top (char &optional noerror)
  "Go to the marker specified by CHAR and scroll to the top."
  :keep-visual t
  :repeat nil
  :type exclusive
  :jump t
  (interactive (list (read-char)))
  (let ((stored-point (point)))
    (evil-goto-mark char noerror)
    (when (not (equal (point) stored-point))
      (evil-scroll-line-to-top nil))))

(evil-define-command evil-goto-mark-bottom (char &optional noerror)
  "Go to the marker specified by CHAR and scroll to the bottom."
  :keep-visual t
  :repeat nil
  :type exclusive
  :jump t
  (interactive (list (read-char)))
  (let ((stored-point (point)))
    (evil-goto-mark char noerror)
    (when (not (equal (point) stored-point))
      (evil-scroll-line-to-bottom nil))))

(evil-define-command evil-goto-mark-line-center (char &optional noerror)
  "Go to the line of the marker specified by CHAR and scroll to the center."
  :keep-visual t
  :repeat nil
  :type line
  :jump t
  (interactive (list (read-char)))
  (let ((stored-point (point)))
    (evil-goto-mark-line char noerror)
    (when (not (equal (point) stored-point))
      (evil-scroll-line-to-center nil))))

(evil-define-command evil-goto-mark-line-top (char &optional noerror)
  "Go to the line of the marker specified by CHAR and scroll to the top."
  :keep-visual t
  :repeat nil
  :type line
  :jump t
  (interactive (list (read-char)))
  (let ((stored-point (point)))
    (evil-goto-mark-line char noerror)
    (when (not (equal (point) stored-point))
      (evil-scroll-line-to-top nil))))

(evil-define-command evil-goto-mark-line-bottom (char &optional noerror)
  "Go to the line of the marker specified by CHAR and scroll to the bottom."
  :keep-visual t
  :repeat nil
  :type line
  :jump t
  (interactive (list (read-char)))
  (let ((stored-point (point)))
    (evil-goto-mark-line char noerror)
    (when (not (equal (point) stored-point))
      (evil-scroll-line-to-bottom nil))))

(evil-define-motion evil-goto-last-change-center (count)
  "Like `evil-goto-last-change' but also scroll the line to the center."
  (let ((stored-point (point)))
    (evil-goto-last-change count)
    (when (not (equal (point) stored-point))
      (evil-scroll-line-to-center nil))))

(evil-define-motion evil-goto-last-change-top (count)
  "Like `evil-goto-last-change' but also scroll the line to the top."
  (let ((stored-point (point)))
    (evil-goto-last-change count)
    (when (not (equal (point) stored-point))
      (evil-scroll-line-to-top nil))))

(evil-define-motion evil-goto-last-change-bottom (count)
  "Like `evil-goto-last-change' but also scroll the line to the bottom."
  (let ((stored-point (point)))
    (evil-goto-last-change count)
    (when (not (equal (point) stored-point))
      (evil-scroll-line-to-bottom nil))))

(evil-define-motion evil-goto-last-change-reverse-center (count)
  "Like `evil-goto-last-change-reverse' but also scroll the line to the center."
  (let ((stored-point (point)))
    (evil-goto-last-change-reverse count)
    (when (not (equal (point) stored-point))
      (evil-scroll-line-to-center nil))))

(evil-define-motion evil-goto-last-change-reverse-top (count)
  "Like `evil-goto-last-change-reverse' but also scroll the line to the top."
  (let ((stored-point (point)))
    (evil-goto-last-change-reverse count)
    (when (not (equal (point) stored-point))
      (evil-scroll-line-to-top nil))))

(evil-define-motion evil-goto-last-change-reverse-bottom (count)
  "Like `evil-goto-last-change-reverse' but also scroll the line to the bottom."
  (let ((stored-point (point)))
    (evil-goto-last-change-reverse count)
    (when (not (equal (point) stored-point))
      (evil-scroll-line-to-bottom nil))))

;;; my-evil-commands.el ends here
