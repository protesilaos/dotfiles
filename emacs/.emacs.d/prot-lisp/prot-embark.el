;;; prot-embark.el --- Custom Embark keymaps -*- lexical-binding: t -*-

;; Copyright (C) 2023  Protesilaos Stavrou

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos.com/emacs/dotemacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (embark "0.23"))

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

(require 'embark)

(defvar-keymap prot-embark-general-map
  :parent embark-general-map
  "i" #'embark-insert
  "w" #'embark-copy-as-kill
  "E" #'embark-export
  "S" #'embark-collect
  "A" #'embark-act-all
  "DEL" #'delete-region)

(defvar-keymap prot-embark-url-map
  :parent embark-general-map
  "b" #'browse-url
  "d" #'embark-download-url
  "e" #'eww)

(defvar-keymap prot-embark-buffer-map
  :parent embark-general-map
  "k" #'prot-simple-kill-buffer
  "o" #'switch-to-buffer-other-window
  "e" #'ediff-buffers)

(add-to-list 'embark-post-action-hooks (list 'prot-simple-kill-buffer 'embark--restart))

(defvar-keymap prot-embark-file-map
  :parent embark-general-map
  "f" #'find-file
  "j" #'embark-dired-jump
  "c" #'copy-file
  "e" #'ediff-files)

(defvar-keymap prot-embark-identifier-map
  :parent embark-general-map
  "h" #'display-local-help
  "." #'xref-find-definitions
  "o" #'occur)

(defvar-keymap prot-embark-command-map
  :parent embark-general-map
  "h" #'describe-command
  "." #'embark-find-definition)

(defvar-keymap prot-embark-expression-map
  :parent embark-general-map
  "e" #'pp-eval-expression
  "m" #'pp-macroexpand-expression)

(defvar-keymap prot-embark-function-map
  :parent embark-general-map
  "h" #'describe-function
  "." #'embark-find-definition)

(defvar-keymap prot-embark-package-map
  :parent embark-general-map
  "h" #'describe-package
  "i" #'package-install
  "d" #'package-delete
  "r" #'package-reinstall
  "u" #'embark-browse-package-url
  "w" #'embark-save-package-url)

(defvar-keymap prot-embark-symbol-map
  :parent embark-general-map
  "h" #'describe-symbol
  "." #'embark-find-definition)

(defvar-keymap prot-embark-variable-map
  :parent embark-general-map
  "h" #'describe-variable
  "." #'embark-find-definition)

(defvar-keymap prot-embark-region-map
  :parent embark-general-map
  "a" #'align-regexp
  "D" #'delete-duplicate-lines
  "f" #'flush-lines
  "i" #'epa-import-keys-region
  "d" #'epa-decrypt-armor-in-region
  "r" #'repunctuate-sentences
  "s" #'sort-lines
  "u" #'untabify)

;; The minimal indicator shows cycling options, but I have no use
;; for those.  I want it to be silent.
(defun prot-embark-no-minimal-indicator ())
(advice-add #'embark-minimal-indicator :override #'prot-embark-no-minimal-indicator)

(defun prot-embark-act-no-quit ()
  "Call `embark-act' but do not quit after the action."
  (interactive)
  (let ((embark-quit-after-action nil))
    (call-interactively #'embark-act)))

(defun prot-embark-act-quit ()
  "Call `embark-act' and quit after the action."
  (interactive)
  (let ((embark-quit-after-action t))
    (call-interactively #'embark-act))
  (when (and (> (minibuffer-depth) 0)
             (derived-mode-p 'completion-list-mode))
    (abort-recursive-edit)))

(provide 'prot-embark)
;;; prot-embark.el ends here
