;;; prot-vertico.el --- Custom Vertico extras -*- lexical-binding: t -*-

;; Copyright (C) 2023  Protesilaos Stavrou

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos.com/emacs/dotemacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))

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
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

(require 'vertico)

(defvar prot-vertico-multiform-minimal
  '(unobtrusive
    (vertico-flat-format . ( :multiple  ""
                             :single    ""
                             :prompt    ""
                             :separator ""
                             :ellipsis  ""
                             :no-match  "")))
  "List of configurations for minimal Vertico multiform.
The minimal view is intended to be more private or less
revealing.  This is important when, for example, a prompt shows
names of people.  Of course, such a view also provides a minimal
style for general usage.

Toggle the vertical view with the `vertico-multiform-vertical'
command or use the commands `prot-vertico-private-next' and
`prot-vertico-private-previous', which toggle the vertical view
automatically.")

(defvar prot-vertico-multiform-maximal
  '((vertico-count . 10)
    (vertico-resize . t))
  "List of configurations for maximal Vertico multiform.")

(defun prot-vertico--match-directory (str)
  "Match directory delimiter in STR."
  (string-suffix-p "/" str))

;; From the Vertico documentation.
(defun prot-vertico-sort-directories-first (files)
  "Sort directories before FILES."
  (setq files (vertico-sort-alpha files))
  (nconc (seq-filter #'prot-vertico--match-directory files)
         (seq-remove #'prot-vertico--match-directory files)))

(defun prot-vertico-private-next ()
  "Like `vertico-next' but toggle vertical view if needed.
This is done to accommodate `prot-vertico-multiform-minimal'."
  (interactive)
  (if vertico-unobtrusive-mode
      (let ((vertico--index 0))
        (vertico-multiform-vertical)
        (vertico-next 1))
    (vertico-next 1)))

(defun prot-vertico-private-previous ()
  "Like `vertico-previous' but toggle vertical view if needed.
This is done to accommodate `prot-vertico-multiform-minimal'."
  (interactive)
  (if vertico-unobtrusive-mode
      (progn
        (vertico-multiform-vertical)
        (vertico-previous 1))
    (vertico-previous 1)))

(defun prot-vertico-private-complete ()
  "Expand contents and show remaining candidates, if needed.
This is done to accommodate `prot-vertico-multiform-minimal'."
  (interactive)
  (if (and vertico-unobtrusive-mode (> vertico--total 1))
      (progn
        (minibuffer-complete)
        (vertico-multiform-vertical))
    (vertico-insert)))

(provide 'prot-vertico)
;;; prot-vertico.el ends here
