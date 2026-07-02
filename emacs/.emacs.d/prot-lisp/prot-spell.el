;;; prot-spell.el --- Spelling-related extensions for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2021-2026  Protesilaos

;; Author: Protesilaos <info@protesilaos.com>
;; URL: https://protesilaos.com/emacs/dotemacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))

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
;; setup: https://protesilaos.com/emacs/dotemacs.

;;; Code:

(require 'ispell)

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

;;;###autoload
(defun prot-spell-spell-dwim (beg end)
  "Spell check between BEG END, current word, or select a dictionary.

Use `flyspell-region' on the active region and deactivate the
mark.

With point over a word and no active region invoke `ispell-word'.

Else call `prot-spell-change-dictionary'."
  (interactive "r")
  (cond
   ((use-region-p)
    (flyspell-region beg end)
    (deactivate-mark))
   ((thing-at-point 'word)
    (call-interactively 'ispell-word))
   (t
    (call-interactively 'prot-spell-change-dictionary))))

(defun prot-spell-ispell-display-buffer (buffer)
  "Function to override `ispell-display-buffer' for BUFFER.
Use this as `advice-add' to override the aforementioned Ispell
function.  Then you can control the buffer's specifics via
`display-buffer-alist' (how it ought to be!)."
  (pop-to-buffer buffer)
  (set-window-point (get-buffer-window buffer) (point-min)))

(advice-add #'ispell-display-buffer :override #'prot-spell-ispell-display-buffer)

;;;; French punctuation

;; TODO 2026-07-01: Maybe extend the command to check for all sorts of
;; rules, in accordance with the Charte orthotypographique du Journal
;; officiel:
;; <https://www.legifrance.gouv.fr/contenu/Media/Files/autour-de-la-loi/legislatif-et-reglementaire/charte_typographique_jo_janvier_2021.pdf?__cf_chl_f_tk=YcOBi.sMMWmrN5FxCQMM8YDz.OyTC62mXKuZnjZ2rvs-1782901834-1.0.1.1-IzZmLEEkwksUf1sSm5dljuFqSZjDPjXfLMYkIdbOa.Y>.
;;
;; Though that is not a simple search+replace operation...
;;
;; For the following:
;;
;; «L’espace insécable permet d’éviter la séparation entre deux
;; éléments. Elle s’utilisera, entre autres, à l’intérieur des
;; guillemets français, au sein des nombres, après les dito, avec les
;; tirets d’incise.»
(defvar prot-spell-french-grammar-replacements
  '(("\\([^[:punct:][:space:]]\\)\\?" . "\\1 ?")
    ("\\([^[:punct:][:space:]]\\):" . "\\1 :")
    ("\\([^[:punct:][:space:]]\\);" . "\\1 ;")
    ("\\([^[:punct:][:space:]]\\)!" . "\\1 !")
    ("«\\([^[:punct:][:space:]]\\)" . "« \\1")
    ("\\([^[:punct:][:space:]]\\)»" . "\\1 »")
    ("\\([^[:punct:][:space:]]\\)%" . "\\1 %"))
  "Add no-break space to relevant punctuation.")

;;;###autoload
(defun prot-spell-french-punctuation ()
  "Apply the `prot-spell-french-grammar-replacements' across the buffer."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (pcase-dolist (`(,target . ,replacement) prot-spell-french-grammar-replacements)
        (goto-char (point-min))
        (while (re-search-forward target nil t)
          (replace-match replacement))))))

(provide 'prot-spell)
;;; prot-spell.el ends here
