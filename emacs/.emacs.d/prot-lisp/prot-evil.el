;;; prot-evil.el --- Extras for Evil mode -*- lexical-binding: t -*-

;; Copyright (C) 2023  Protesilaos Stavrou

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos.com/emacs/dotemacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (evil "1.15.0"))

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

(require 'evil)

;;;; The "basic" state

(defvar prot-evil-basic-tag " <PB> "
  "Mode line tag for the prot-basic state.")

(defvar prot-evil-basic-message "-- PROT BASIC --"
  "Echo area message when entering the prot basic state.")

(evil-define-state prot-basic
  "Basic Vim keys to work in most (?) read-only major modes."
  :tag 'prot-evil-basic-tag
  :message 'prot-evil-basic-message)

(evil-define-command evil-force-prot-basic-state ()
  "Switch to  state without recording current command."
  :repeat abort
  :suppress-operator t
  (evil-prot-basic-state))

(evil-define-key 'prot-basic global-map
  "0" #'evil-beginning-of-line
  "$" #'evil-end-of-line
  "h" #'evil-backward-char
  "j" #'evil-next-line
  "k" #'evil-previous-line
  "l" #'evil-forward-char
  "i" #'evil-insert
  "v" #'evil-visual-char
  "V" #'evil-visual-line)

(defun prot-evil-need-basic-p ()
  "Return non-nil if the basic state should be used."
  (or buffer-read-only
      (memq major-mode evil-prot-basic-state-modes)))

(defun prot-evil-normal-or-basic-state ()
  "Return to normal or basic state per `prot-evil-need-basic-p'."
  (interactive)
  (if (prot-evil-need-basic-p)
      (evil-force-prot-basic-state)
    (evil-force-normal-state)))

;;;; The "erase" operator

(evil-define-operator prot-evil-erase (beg end type &rest _)
  "Erase text from BEG to END with TYPE.
Unlike the delete operator, do not store the erased text anywhere."
  (interactive "<R><x><y>")
  (when (and (memq type '(inclusive exclusive))
             (not (evil-visual-state-p))
             (eq 'prot-evil-erase evil-this-operator)
             (save-excursion (goto-char beg) (bolp))
             (save-excursion (goto-char end) (eolp))
             (<= 1 (evil-count-lines beg end)))
    ;; Imitate Vi strangeness: if motion meets above criteria,
    ;; delete linewise. Not for change operator or visual state.
    (let ((new-range (evil-line-expand beg end)))
      (setq beg (car new-range)
            end (cadr new-range)
            type 'line)))
  (cond
   ((eq type 'block)
    (evil-apply-on-block #'delete-region beg end nil))
   ((and (eq type 'line)
         (= end (point-max))
         (or (= beg end)
             (/= (char-before end) ?\n))
         (/= beg (point-min))
         (= (char-before beg) ?\n))
    (delete-region (1- beg) end))
   (t (delete-region beg end)))
  (when (and (eq type 'line)
             (called-interactively-p 'any))
    (evil-first-non-blank)
    (when (and (not evil-start-of-line)
               evil-operator-start-col
               ;; Special exceptions to ever saving column:
               (not (memq evil-this-motion '(evil-forward-word-begin
                                             evil-forward-WORD-begin))))
      (move-to-column evil-operator-start-col))))

;;;; Integration with `prot-prefix' (leader key)

;; The `prot-prefix' feature is loaded by the `prot-emacs-essentials'
;; module.

(defun prot-evil-prefix-or-self-insert ()
  "Self-insert key or return `prot-prefix-map'.
For use as a leader key in the emacs/insert evil state when the
buffer is not writable."
  (interactive)
  (if (prot-evil-need-basic-p)
      (set-transient-map prot-prefix-map)
    (self-insert-command 1)))

;;;; Do not pollute the kill-ring in visual state

(defun prot-evil-visual-paste-no-kill (&rest args)
  "Do not add visual selection to the `kill-ring' while pasting.
Add as :around advice to `evil-paste-after' and `evil-paste-before',
applying its ARGS."
  (if (evil-visual-state-p)
      (cl-letf (((symbol-function 'evil-yank) #'ignore))
        (apply args))
    (apply args)))

(advice-add #'evil-paste-after :around #'prot-evil-visual-paste-no-kill)
(advice-add #'evil-paste-before :around #'prot-evil-visual-paste-no-kill)

(provide 'prot-evil)
;;; prot-evil.el ends here
