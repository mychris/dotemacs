;; eshell-cmds --- additional eshell commands        -*- lexical-binding: t; -*-

;; Copyright (c) 2023 Christoph Göttschkes

;; Author: Christoph Göttschkes
;; Maintainer: Christoph Göttschkes
;; Created: 27 Jan 2023
;; Modified: 27 Jan 2023
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

;;; Code:

(require 'em-dirs)

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

(provide 'eshell-cmds)

;;; eshell-cmds.el ends here
