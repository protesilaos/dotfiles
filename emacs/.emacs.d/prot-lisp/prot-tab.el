;;; prot-tab.el --- Tab bar (tab-bar.el) extras for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2020-2021  Protesilaos Stavrou

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos.com/emacs/dotemacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

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
;; This set of configurations pertains to my tab-bar.el extensions, for
;; use in my Emacs setup: https://protesilaos.com/emacs/dotemacs.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

(require 'tab-bar)

(defgroup prot-tab ()
  "Extensions for tab-bar.el."
  :group 'tab-bar)

(defcustom prot-tab-tab-select-num-threshold 3
  "Minimum number of tabs to prompt for numeric selection.
This is used by `prot-tab-select-tab-dwim' to determine whether
it should prompt for completion, or to ask for just a tab number
to switch to.  If the number of open tabs is greater than this
variable's value, then the command will prompt for a number."
  :type 'integer
  :group 'prot-tab)

;;;; General commands

(defun prot-tab--tab-bar-tabs ()
  "Return a list of `tab-bar' tabs, minus the current one."
  (mapcar (lambda (tab)
            (alist-get 'name tab))
          (tab-bar--tabs-recent)))

;;;###autoload
(defun prot-tab-select-tab-dwim (&optional arg)
  "Do-What-I-Mean function for getting to a `tab-bar' tab.
If no other tab exists, or with optional prefix argument
ARG (\\[universal-argument]), create one and switch to it.

If there is one other tab (so two in total) switch to it without
further questions.

If the tabs are more than `prot-tab-tab-select-num-threshold',
show numeric hints (`tab-bar-tab-hints') and prompt for a number
to switch to.  Else prompt for full text completion."
  (interactive "P")
  (let ((tabs (prot-tab--tab-bar-tabs)))
    (cond
     ((or arg (null tabs))
      (tab-new))
     ((length= tabs 1)
      (tab-next))
     ((length> tabs (1- prot-tab-tab-select-num-threshold))
      (let ((tab-bar-tab-hints t)
            (bar tab-bar-mode))
        (unwind-protect
            (progn
              (unless bar
                (prot-tab-bar-toggle 1))
              (tab-bar-select-tab
               (read-number "Go to tab NUM: ")))
          (unless bar
            (prot-tab-bar-toggle -1)))))
     (t
      (tab-bar-switch-to-tab
       (completing-read "Select tab: " tabs nil t))))))

;;;###autoload
(define-minor-mode prot-tab-bar-toggle
  "Toggle `tab-bar' presentation."
  :init-value nil
  :global t
  (if (or prot-tab-bar-toggle
          (not (bound-and-true-p tab-bar-mode)))
      (progn
        (setq tab-bar-show t)
        (tab-bar-mode 1))
    (setq tab-bar-show nil)
    (tab-bar-mode -1)))

;;;; Window layout history

(declare-function winner-undo "winner")
(declare-function winner-redo "winner")

;;;###autoload
(defun prot-tab-winner-undo ()
  "Go to previous window layout in the history.
When Tab-Bar-Mode and Tab-Bar-History-Mode are active, use
history that is specific to the current tab.  Else try to call
`winner-undo' if Winner-Mode is active.  Signal an error
otherwise."
  (interactive)
  (if (and (bound-and-true-p tab-bar-mode)
           (bound-and-true-p tab-bar-history-mode))
      (progn
        (tab-bar-history-back)
        (setq this-command 'tab-bar-history-back))
    (if (bound-and-true-p winner-mode)
        (progn
          (winner-undo)
          (setq this-command 'winner-undo))
      (user-error "No `tab-bar-history-mode' or `winner-mode' active"))))

;;;###autoload
(defun prot-tab-winner-redo ()
  "Go to next window layout in the history.
When Tab-Bar-Mode and Tab-Bar-History-Mode are active, use
history that is specific to the current tab.  Else try to call
`winner-redo' if Winner-Mode is active.  Signal an error
otherwise."
  (interactive)
  (if (and (bound-and-true-p tab-bar-mode)
           (bound-and-true-p tab-bar-history-mode))
      (progn
        (tab-bar-history-forward)
        (setq this-command 'tab-bar-history-forward))
    (if (bound-and-true-p winner-mode)
        (progn
          (winner-redo)
          (setq this-command 'winner-redo))
      (user-error "No `tab-bar-history-mode' or `winner-mode' active"))))

;;;; Indicators for `tab-bar-format' --- EXPERIMENTAL

(defun prot-tab-format-mule-info ()
  "Format `mode-line-mule-info' for the tab bar."
  `((global menu-item ,(string-trim-right (format-mode-line mode-line-mule-info)) ignore)))

(defun prot-tab-format-modified ()
  "Format `mode-line-modified' for the tab bar."
  `((global menu-item ,(string-trim-right (format-mode-line mode-line-modified)) ignore)))

(defun prot-tab-format-modes ()
  "Format `mode-line-modes' for the tab bar."
  `((global menu-item ,(string-trim-right (format-mode-line mode-line-modes)) ignore)))

;; FIXME 2021-07-30: This does not update unless some other event takes
;; place, such as an ELDOC update.  Otherwise it updates every second.
(defun prot-tab-format-position ()
  "Format `mode-line-position' for the tab bar."
  `((global menu-item ,(string-trim-right (format-mode-line mode-line-position)) ignore)))

(defun prot-tab-format-vc ()
  "Format VC status for the tab bar."
  `((global menu-item ,(string-trim-right (format-mode-line vc-mode)) ignore)))

(defun prot-tab-format-misc-info ()
  "Format `mode-line-misc-info' for the tab bar."
  `((global menu-item ,(string-trim-right (format-mode-line mode-line-misc-info)) ignore)))

(defun prot-tab-format-space-single ()
  "Format space for the tab bar."
  `((global menu-item " " ignore)))

(defun prot-tab-format-space-double ()
  "Format double space for the tab bar."
  `((global menu-item "  " ignore)))

(defvar prot-tab--window-divider-place (default-value 'window-divider-default-places)
  "Last value of `window-divider-default-places'.
For use in Prot-Tab-Status-Line.")

(declare-function prot-notmuch-mail-indicator "prot-notmuch")

;; NOTE 2021-07-30: This is experimental and subject to review.
;;;###autoload
(define-minor-mode prot-tab-status-line
  "Make Tab bar a status line and configure the extras.
Hide the mode lines and change their colors."
  :global t
  :group 'prot-tab
  (if prot-tab-status-line
      (progn
        (setq tab-bar-show t)
        (tab-bar-mode 1)
        (tab-bar-history-mode 1)
        (setq window-divider-default-places t)
        (window-divider-mode 1)
        (display-time-mode 1)
        (when (featurep 'prot-notmuch)
          (prot-notmuch-mail-indicator 1))
        (custom-set-faces
         `(mode-line ((default :height 1 :box nil :overline nil :underline nil)
                      (((class color) (min-colors 88) (background light))
                       :background "#0000c0" ; OR ,@(list (face-attribute 'default :foreground))
                       :foreground "#0000c0")
                      (((class color) (min-colors 88) (background dark))
                       :background "#00bcff"
                       :foreground "#00bcff")
                      (t :inverse-video t)))
         `(mode-line-inactive ((default :height 1 :box nil :overline nil :underline nil)
                      (((class color) (min-colors 88) (background light))
                       :background "white"
                       :foreground "white")
                      (((class color) (min-colors 88) (background dark))
                       :background "black"
                       :foreground "black")))))
    (setq tab-bar-show nil)
    (tab-bar-mode -1)
    (tab-bar-history-mode -1)
    (setq window-divider-default-places prot-tab--window-divider-place)
    (window-divider-mode -1)
    (display-time-mode -1)
    (when (featurep 'prot-notmuch)
      (prot-notmuch-mail-indicator -1))
    (custom-set-faces
     `(mode-line (( )))
     `(mode-line-inactive (( ))))))

(provide 'prot-tab)
;;; prot-tab.el ends here
