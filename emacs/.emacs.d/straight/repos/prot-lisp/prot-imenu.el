;;; prot-imenu.el --- Extend imenu.el for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2020  Protesilaos Stavrou

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos.com/dotemacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
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
;; Extensions to the built-in `imenu.el' library for my Emacs
;; configuration: <https://protesilaos.com/dotemacs/>.

;;; Code:

(require 'prot-pulse)

;;;###autoload
(defun prot-imenu-recentre-pulse-top ()
  "Recentre `imenu' position at the top.
Add this to `imenu-after-jump-hook' or equivalent."
  (let ((pulse-delay .05))
    (recenter 0)
    (prot-pulse-pulse-line)))

;;;###autoload
(defun prot-imenu-recentre-pulse-centre ()
  "Recentre `imenu' position at the top.
Add this to `imenu-after-jump-hook' or equivalent."
  (let ((pulse-delay .05))
    (recenter nil)
    (prot-pulse-pulse-line)))

(declare-function org-at-heading-p "org")
(declare-function org-show-entry "org")
(declare-function org-reveal "org")
(declare-function outline-show-entry "outline")

;;;###autoload
(defun prot-imenu-show-entry ()
  "Reveal index at point after successful `imenu' execution.
To be used with `imenu-after-jump-hook' or equivalent."
  (cond
   ((and (eq major-mode 'org-mode)
         (org-at-heading-p))
    (org-show-entry)
    (org-reveal t))
   ((bound-and-true-p prot-outline-minor-mode)
    (outline-show-entry))))

(provide 'prot-imenu)
;;; prot-imenu.el ends here
