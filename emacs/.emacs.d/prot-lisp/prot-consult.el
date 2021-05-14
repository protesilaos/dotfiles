;;; prot-consult.el --- Tweak consult.el for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2020-2021  Protesilaos Stavrou

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
;; Tweaks for `consult.el' intended for my Emacs configuration:
;; <https://protesilaos.com/dotemacs/>.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

(require 'consult nil t)
(require 'consult-imenu nil t)
(require 'prot-pulse)

(defgroup prot-consult ()
  "Tweaks for consult.el."
  :group 'minibuffer)

(defcustom prot-consult-command-centre-list '(consult-line consult-mark)
  "Commands to run `prot-consult-jump-recentre-hook'.
You must restart function `prot-consult-set-up-hooks-mode' for
changes to take effect."
  :group 'prot-consult
  :type 'list)

(defcustom prot-consult-command-top-list '(consult-outline)
  "Commands to run `prot-consult-jump-top-hook'.
You must restart function `prot-consult-set-up-hooks-mode' for
changes to take effect."
  :group 'prot-consult
  :type 'list)

;;;; Setup for some consult commands (TODO: needs review)

(defvar prot-consult-jump-recentre-hook nil
  "Hook that runs after select Consult commands.
To be used with `advice-add'.")

(defun prot-consult-after-jump-recentre (&rest _)
  "Run `prot-consult-jump-recentre-hook'."
  (run-hooks 'prot-consult-jump-recentre-hook))

(defvar prot-consult-jump-top-hook nil
  "Hook that runs after select Consult commands.
To be used with `advice-add'.")

(defun prot-consult-after-jump-top (&rest _)
  "Run `prot-consult-jump-top-hook'."
  (run-hooks 'prot-consult-jump-top-hook))

;;;###autoload
(define-minor-mode prot-consult-set-up-hooks-mode
  "Set up hooks for Consult."
  :init-value nil
  :global t
  (if prot-consult-set-up-hooks-mode
      (progn
        (dolist (fn prot-consult-command-centre-list)
          (advice-add fn :after #'prot-consult-after-jump-recentre))
        (dolist (fn prot-consult-command-top-list)
          (advice-add fn :after #'prot-consult-after-jump-top))
        (add-hook 'prot-consult-jump-recentre-hook #'prot-pulse-recentre-centre)
        (add-hook 'prot-consult-jump-top-hook #'prot-pulse-recentre-top)
        (add-hook 'prot-consult-jump-top-hook #'prot-pulse-show-entry))
    (dolist (fn prot-consult-command-centre-list)
      (advice-remove fn #'prot-consult-after-jump-recentre))
    (dolist (fn prot-consult-command-top-list)
      (advice-remove fn #'prot-consult-after-jump-top))
    (remove-hook 'prot-consult-jump-recentre-hook #'prot-pulse-recentre-centre)
    (remove-hook 'prot-consult-jump-top-hook #'prot-pulse-recentre-top)
    (remove-hook 'prot-consult-jump-top-hook #'prot-pulse-show-entry)))

;;;; Commands

(defvar consult--find-cmd)
(defvar consult--directory-prompt)
(declare-function consult--find "consult")
(autoload 'prot-orderless-with-styles "prot-orderless")

;;;###autoload
(defun prot-consult-project-root ()
  "Return path to project or `default-directory'.
Intended to be assigned to `consult-project-root-function'."
  (or (vc-root-dir)
      (locate-dominating-file "." ".git")
      default-directory))

;;;###autoload
(defun prot-consult-outline ()
  "Run `consult-outline' through `prot-orderless-with-styles'."
  (interactive)
  (prot-orderless-with-styles 'consult-outline))

;;;###autoload
(defun prot-consult-imenu ()
  "Run `consult-imenu' through `prot-orderless-with-styles'."
  (interactive)
  (prot-orderless-with-styles 'consult-imenu))

;;;###autoload
(defun prot-consult-line ()
  "Run `consult-line' through `prot-orderless-with-styles'."
  (interactive)
  (prot-orderless-with-styles 'consult-line))

;;;###autoload
(defun prot-consult-yank ()
  "Run `consult-yank' through `prot-orderless-with-styles'."
  (interactive)
  (prot-orderless-with-styles 'consult-yank))

(provide 'prot-consult)
;;; prot-consult.el ends here
