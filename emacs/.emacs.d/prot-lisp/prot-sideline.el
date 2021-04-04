;;; prot-sideline.el --- Extensions for line numbers and relevant indicators -*- lexical-binding: t -*-

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
;; Extensions for line numbers and relevant indicators, intended to be
;; used as part of my Emacs setup: https://protesilaos.com/dotemacs.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

(defgroup prot-sideline ()
  "Setup for reading and presenting text-heavy buffers."
  :group 'files)

;;;###autoload
(define-minor-mode prot-sideline-mode
  "Buffer-local wrapper mode for presentations."
  :init-value nil
  :global nil)

(autoload 'diff-hl-mode "diff-hl")

(defun prot-sideline--diff-hl-toggle ()
  "Toggle buffer local diff indicators in the fringe."
  (if (or (bound-and-true-p diff-hl-mode)
          (not (bound-and-true-p prot-sideline-mode)))
      (diff-hl-mode -1)
    (diff-hl-mode 1)))

(add-hook 'prot-sideline-mode-hook #'prot-sideline--diff-hl-toggle)

(defun prot-sideline--numbers-toggle ()
  "Toggle line numbers."
  (if (or (bound-and-true-p display-line-numbers-mode)
          (not (bound-and-true-p prot-sideline-mode)))
      (display-line-numbers-mode -1)
    (display-line-numbers-mode 1)))

(add-hook 'prot-sideline-mode-hook #'prot-sideline--numbers-toggle)

(defun prot-sideline--hl-line-toggle ()
  "Toggle line highlight."
  (if (or (bound-and-true-p hl-line-mode)
          (not (bound-and-true-p prot-sideline-mode)))
      (hl-line-mode -1)
    (hl-line-mode 1)))

(add-hook 'prot-sideline-mode-hook #'prot-sideline--hl-line-toggle)

(autoload 'whitespace-mode "whitespace")

;; We keep this separate, as I do not want it bundled up together with
;; the rest of the functionality included here.
;;;###autoload
(defun prot-sideline-negative-space-toggle ()
  "Toggle the display of indentation and space characters."
  (interactive)
  (if (bound-and-true-p whitespace-mode)
      (whitespace-mode -1)
    (whitespace-mode)))

(provide 'prot-sideline)
;;; prot-sideline.el ends here
