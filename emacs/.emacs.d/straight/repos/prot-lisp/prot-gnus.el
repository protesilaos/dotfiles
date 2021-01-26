;;; prot-gnus.el --- Gnus tweaks for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2021  Protesilaos Stavrou

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos.com/dotemacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; XXX 2021-01-06: work-in-progress
;;
;; This covers my Gnus tweaks, for use in my Emacs setup:
;; https://protesilaos.com/dotemacs.

;;; Code:

(require 'gnus)
(require 'prot-common)

(defgroup prot-gnus ()
  "Extensions for ignus and flygnus."
  :group 'gnus)

(autoload 'gnus-group-next-group "gnus-group")

(defun prot-gnus-group-previous-group (&optional arg)
  "Move to the ARGth previous group.
If no numeric prefix ARG is supplied move by one."
  (interactive "p")
  (let ((num (prot-common-number-negative arg))) ; from `prot-common.el'
    (gnus-group-next-group (or num -1) t)))

(provide 'prot-gnus)
;;; prot-gnus.el ends here
