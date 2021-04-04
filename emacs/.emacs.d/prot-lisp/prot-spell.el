;;; prot-spell.el --- Spelling-related extensions for my dotemacs -*- lexical-binding: t -*-

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
;; This covers my spelling-related extensions, for use in my Emacs
;; setup: https://protesilaos.com/dotemacs.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

(defgroup prot-spell ()
  "Extensions for ispell and flyspell."
  :group 'ispell)

(defcustom prot-spell-dictionaries
  '(("EN English" . "en")
    ("EL Ελληνικά" . "el")
    ("FR Français" . "fr")
    ("ES Espanõl" . "es"))
  "Alist of strings with descriptions and dictionary keys.
Used by `prot-spell-change-dictionary'."
  :type 'alist
  :group 'prot-spell)

(defvar prot-spell--dictionary-hist '()
  "Input history for `prot-spell-change-dictionary'.")

(defun prot-spell--dictionary-prompt ()
  "Helper prompt to select from `prot-spell-dictionaries'."
  (let ((def (car prot-spell--dictionary-hist)))
    (completing-read
     (format "Select dictionary [%s]: " def)
     (mapcar #'car prot-spell-dictionaries)
     nil t nil 'prot-spell--dictionary-hist def)))

;;;###autoload
(defun prot-spell-change-dictionary (dictionary)
  "Select a DICTIONARY from `prot-spell-dictionaries'."
  (interactive
   (list (prot-spell--dictionary-prompt)))
  (let* ((key (cdr (assoc dictionary prot-spell-dictionaries)))
         (desc (car (assoc dictionary prot-spell-dictionaries))))
    (ispell-change-dictionary key)
    (message "Switched dictionary to %s" (propertize desc 'face 'bold))))

(autoload 'flyspell-region "flyspell")
(autoload 'thing-at-point "thingatpt")
(autoload 'ispell-word "ispell")

;;;###autoload
(defun prot-spell-spell-dwim (beg end)
  "Spellcheck between BEG END, current word, or select dictionary.

Use `flyspell-region' on the active region.  With point over a
word and no active region invoke `ispell-word'.  Else call
`prot-spell-change-dictionary'."
  (interactive "r")
  (cond
   ((use-region-p)
    (flyspell-region beg end))
   ((thing-at-point 'word)
    (call-interactively 'ispell-word))
   (t
    (call-interactively 'prot-spell-change-dictionary))))

(provide 'prot-spell)
;;; prot-spell.el ends here
