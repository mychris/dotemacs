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


(require 'evil-core)

(evil-define-text-object +evil-whole-buffer (_count &optional _beg _end _type)
  "Select the whole buffer."
  :extend-selection t
  (list (point-min) (point-max)))

;;;###autoload
(defun +evil-setup ()
  "Setup `+evil' package."
  (define-key evil-outer-text-objects-map "h" '+evil-whole-buffer)
  (define-key evil-inner-text-objects-map "h" '+evil-whole-buffer))


(provide '+evil)

;;; +evil.el ends here
