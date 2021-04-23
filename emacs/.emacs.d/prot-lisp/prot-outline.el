;;; prot-outline.el --- Extend outline.el for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2020-2021  Protesilaos Stavrou

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos.com/dotemacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

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
;; Extensions to the built-in `outline.el' library for my Emacs
;; configuration: <https://protesilaos.com/dotemacs/>.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

(require 'outline)
(require 'prot-common)

;;;; Commands

;;;###autoload
(defun prot-outline-move-major-heading-down (&optional arg)
  "Move Outline major heading down one or, optionally, ARG times.
A major heading is one that has subheadings."
  (interactive "p")
  (if (or (and (outline-on-heading-p) (outline-has-subheading-p))
          (outline-invisible-p))
      (outline-move-subtree-down (or arg 1))
    (forward-line (or arg 1))))

;;;###autoload
(defun prot-outline-move-major-heading-up (&optional arg)
  "Move Outline major heading up one or, optionally, ARG times.
A major heading is one that has subheadings."
  (interactive "p")
  (if (or (and (outline-on-heading-p) (outline-has-subheading-p))
          (outline-invisible-p))
      (outline-move-subtree-up (or arg 1))
    (forward-line (- (or arg 1)))))

;;;###autoload
(defun prot-outline-narrow-to-subtree ()
  "Narrow to current Outline subtree."
  (interactive)
  (let ((start)
        (end)
        (point (point)))
    (when (and (prot-common-line-regexp-p 'empty)
               (not (eobp)))
      (forward-char 1))
    (when (or (outline-up-heading 1)
              (outline-back-to-heading))
      (setq start (point)))
    (if (outline-get-next-sibling)
        (forward-line -1)
      (goto-char (point-max)))
    (setq end (point))
    (narrow-to-region start end)
    (goto-char point)))

;;;; Minor mode setup

(defvar prot-outline-major-modes-blocklist '(org-mode outline-mode markdown-mode))

;;;###autoload
(defun prot-outline-minor-mode-safe ()
  "Test to set variable `outline-minor-mode' to non-nil."
  (interactive)
  (let ((blocklist prot-outline-major-modes-blocklist)
        (mode major-mode))
    (when (derived-mode-p (car (member mode blocklist)))
      (error "Don't use `prot-outline-minor-mode' with `%s'" mode))
    (if (eq outline-minor-mode nil)
        (progn
          (outline-minor-mode 1)
          (message "Enabled `outline-minor-mode'"))
      (outline-minor-mode -1)
      (message "Disabled `outline-minor-mode'"))))

(provide 'prot-outline)
;;; prot-outline.el ends here
