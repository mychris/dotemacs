;; +evil.el --- Extensions for evil    -*- coding: utf-8; lexical-binding: t; -*-

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

;;   Extensions for evil
;;
;; This package extends the functionality provided by the evil package.
;; Only small additions, which are not worth their own package.

;;; Code:


(eval-and-compile
  (require 'nadvice)
  (require 'evil-common)
  (require 'evil-macros)
  (require 'evil-commands)
  (require 'cl-lib))

(evil-define-command +evil-goto-mark-center (char &optional noerror)
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

(evil-define-command +evil-goto-mark-top (char &optional noerror)
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

(evil-define-command +evil-goto-mark-bottom (char &optional noerror)
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

(evil-define-command +evil-goto-mark-line-center (char &optional noerror)
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

(evil-define-command +evil-goto-mark-line-top (char &optional noerror)
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

(evil-define-command +evil-goto-mark-line-bottom (char &optional noerror)
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


(eval-and-compile
  (unless (fboundp 'evil-goto-last-change)
    ;; This implementation is copied from emacs-evil master branch
    ;; For copyright information, see:
    ;; https://github.com/emacs-evil/evil/blob/9f25e190c360dc65dbcdfaef3d6408eedd5921d9/COPYING
    (evil-define-motion evil-goto-last-change (count)
      "Like `goto-last-change' but takes a COUNT rather than a span."
      (eval-and-compile (require 'goto-chg))
      (setq this-command 'goto-last-change)
      (dotimes (_ (or count 1))
        (goto-last-change nil))))
  (unless (fboundp 'evil-goto-last-change-reverse)
    ;; This implementation is copied from emacs-evil master branch
    ;; For copyright information, see:
    ;; https://github.com/emacs-evil/evil/blob/9f25e190c360dc65dbcdfaef3d6408eedd5921d9/COPYING
    (evil-define-motion evil-goto-last-change-reverse (count)
      "Like `goto-last-change-reverse' but takes a COUNT rather than a span."
      (eval-and-compile (require 'goto-chg))
      (setq this-command 'goto-last-change-reverse)
      (dotimes (_ (or count 1))
                  (goto-last-change-reverse nil)))))

(evil-define-motion +evil-goto-last-change-center (count)
  "Like `evil-goto-last-change' but also scroll the line to the center."
  (let ((stored-point (point)))
    (evil-goto-last-change count)
    (when (not (equal (point) stored-point))
      (evil-scroll-line-to-center nil))))

(evil-define-motion +evil-goto-last-change-top (count)
  "Like `evil-goto-last-change' but also scroll the line to the top."
  (let ((stored-point (point)))
    (evil-goto-last-change count)
    (when (not (equal (point) stored-point))
      (evil-scroll-line-to-top nil))))

(evil-define-motion +evil-goto-last-change-bottom (count)
  "Like `evil-goto-last-change' but also scroll the line to the bottom."
  (let ((stored-point (point)))
    (evil-goto-last-change count)
    (when (not (equal (point) stored-point))
      (evil-scroll-line-to-bottom nil))))

(evil-define-motion +evil-goto-last-change-reverse-center (count)
  "Like `evil-goto-last-change-reverse' but also scroll the line to the center."
  (let ((stored-point (point)))
    (evil-goto-last-change-reverse count)
    (when (not (equal (point) stored-point))
      (evil-scroll-line-to-center nil))))

(evil-define-motion +evil-goto-last-change-reverse-top (count)
  "Like `evil-goto-last-change-reverse' but also scroll the line to the top."
  (let ((stored-point (point)))
    (evil-goto-last-change-reverse count)
    (when (not (equal (point) stored-point))
      (evil-scroll-line-to-top nil))))

(evil-define-motion +evil-goto-last-change-reverse-bottom (count)
  "Like `evil-goto-last-change-reverse' but also scroll the line to the bottom."
  (let ((stored-point (point)))
    (evil-goto-last-change-reverse count)
    (when (not (equal (point) stored-point))
      (evil-scroll-line-to-bottom nil))))


(evil-define-text-object +evil-whole-buffer (_count &optional _beg _end _type)
  "Select the whole buffer."
  :extend-selection t
  (list (point-min) (point-max)))

;;;###autoload
(defun +evil-setup ()
  "Setup `+evil' package."
  (define-key evil-outer-text-objects-map "h" '+evil-whole-buffer)
  (define-key evil-inner-text-objects-map "h" '+evil-whole-buffer))



(with-eval-after-load 'sly
  (eval-and-compile (require 'sly))
  ;; Advice modeled after pp-last-sexp advice in evil-integration.el
  (defun +evil--sly-macroexpand-advice (orig-fun &rest args)
    "In normal-state or motion-state, form ends at point."
    (if (and (not evil-move-beyond-eol)
	     (or (evil-normal-state-p) (evil-motion-state-p)))
	(save-excursion
	  (unless (or (eobp) (eolp)) (forward-char))
	  (apply orig-fun args))
      (apply orig-fun args)))
  (advice-add #'sly-macroexpand-1           :around '+evil--sly-macroexpand-advice '((name . +evil)))
  (advice-add #'sly-macroexpand-1-inplace   :around '+evil--sly-macroexpand-advice '((name . +evil)))
  (advice-add #'sly-macroexpand-all         :around '+evil--sly-macroexpand-advice '((name . +evil)))
  (advice-add #'sly-macroexpand-all-inplace :around '+evil--sly-macroexpand-advice '((name . +evil))))


(provide '+evil)

;;; +evil.el ends here
