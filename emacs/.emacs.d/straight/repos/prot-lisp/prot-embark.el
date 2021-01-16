;;; prot-embark.el --- Extensions to embark.el for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2020-2021  Protesilaos Stavrou

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos.com/dotemacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))

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
;; Extensions to `embark.el' for my Emacs configuration:
;; <https://protesilaos.com/dotemacs/>.

;;; Code:

(require 'cl-lib)
(when (featurep 'embark)
  (require 'embark))
(require 'prot-common)
(require 'prot-minibuffer)

(defgroup prot-embark ()
  "Extensions for `embark'."
  :group 'editing)

;;;###autoload
(defun prot-embark-clear-live-buffers ()
  "Remove lingering Embark Collect Completions' buffers.
Add this to `minibuffer-exit-hook'."
  (let* ((buffers (buffer-list))
         (case-fold-search nil)
         (completions
          (cl-remove-if-not (lambda (buf)
                              (string-match "\\*Embark.*Completions.*"
                                            (format "%s" buf)))
                            buffers)))
    (mapc #'kill-buffer completions)))

;; Thanks to Omar Antolín Camarena for providing a variant of this!
;; (mistakes are always my own).
;;;###autoload
(defun prot-embark-collect-fit-window (&rest _)
  "Fit Embark's live occur window to its buffer.
To be added to `embark-occur-post-revert-hook'."
  (when (derived-mode-p 'embark-collect-mode)
    (fit-window-to-buffer (get-buffer-window)
                          (floor (frame-height) 2) 1)))

(defvar embark-collect-linked-buffer)

(defun prot-embark--live-buffer-p ()
  "Determine presence of a linked live occur buffer."
  (let ((buf embark-collect-linked-buffer))
    (when buf
      (window-live-p (get-buffer-window buf)))))

;; Thanks to Karthik Chikmagalur for providing an earlier version of
;; `prot-embark-keyboard-quit' command!  Sources to Karthik's work:
;;
;; + https://karthinks.com/
;; + https://github.com/karthink/.emacs.d/tree/clean
;;
;;;###autoload
(defun prot-embark-keyboard-quit ()
  "Control the exit behaviour for Embark collect buffers.

If in a live Embark collect/completions buffer, run
`abort-recursive-edit'.  Otherwise run `keyboard-quit'.

This is meant to be bound in `embark-collect-mode-map'."
  (interactive)
  (if (and (derived-mode-p 'embark-collect-mode)
           (prot-embark--live-buffer-p))
      (progn
        (kill-buffer)
        (abort-recursive-edit))
    (keyboard-quit)))

(declare-function embark-collect-completions "embark")

;;;###autoload
(defun prot-embark-completions-toggle ()
  "Toggle `embark-collect-completions'."
  (interactive)
  (if (prot-embark--live-buffer-p)
      (kill-buffer embark-collect-linked-buffer)
    (embark-collect-completions)))

(declare-function embark--act "embark")
(declare-function embark-default-action "embark")
(declare-function embark--target "embark")

(defun prot-embark--completions-act (arg)
  "Move ARG lines and perform `embark-default-action'."
  (forward-line arg)
  (embark--act #'embark-default-action (cdr (embark--target))))

;;;###autoload
(defun prot-embark-completions-act-next (&optional arg)
  "Run default action on next or ARGth Embark target.
This calls `prot-embark--completions-act' and is meant to be
assigned to a key in `embark-collect-mode-map'."
  (interactive "p")
  (prot-embark--completions-act (or arg 1)))

;;;###autoload
(defun prot-embark-completions-act-previous (&optional arg)
  "Run default action on previous or ARGth Embark target.
This calls `prot-embark--completions-act' and is meant to be
assigned to a key in `embark-collect-mode-map'."
  (interactive "p")
  (let ((num (prot-common-number-negative arg))) ; from `prot-common.el'
    (prot-embark--completions-act (or num -1))))

;;;###autoload
(defun prot-embark-completions-act-current ()
  "Run default action on Embark target without exiting.
Meant to be assigned to a key in `embark-collect-mode-map'."
  (interactive)
  (embark--act #'embark-default-action (cdr (embark--target))))

(defun prot-embark--switch-to-completions ()
  "Subroutine for switching to the Embark completions buffer."
  (unless (prot-embark--live-buffer-p)
    (prot-embark-completions-toggle))
  (let ((win (get-buffer-window embark-collect-linked-buffer)))
    (select-window win)))

;;;###autoload
(defun prot-embark-switch-to-completions-top ()
  "Switch to the top of Embark's completions buffer.
Meant to be bound in `minibuffer-local-completion-map'."
  (interactive)
  (prot-embark--switch-to-completions)
  (goto-char (point-min)))

;;;###autoload
(defun prot-embark-switch-to-completions-bottom ()
  "Switch to the bottom of Embark's completions buffer.
Meant to be bound in `minibuffer-local-completion-map'."
  (interactive)
  (prot-embark--switch-to-completions)
  (goto-char (point-max))
  (forward-line -1)
  (goto-char (point-at-bol))
  (recenter
   (- -1
      (min (max 0 scroll-margin)
           (truncate (/ (window-body-height) 4.0))))
      t))

;;;###autoload
(defun prot-embark-next-line-or-mini (&optional arg)
  "Move to the next line or switch to the minibuffer.
This performs a regular motion for optional ARG lines, but when
point can no longer move in that direction, then it switches to
the minibuffer."
  (interactive "p")
  (if (or (eobp) (eq (point-max) (save-excursion (forward-line 1) (point))))
      (prot-minibuffer-focus-mini)    ; from `prot-minibuffer.el'
    (forward-line (or arg 1)))
  (setq this-command 'next-line))

;;;###autoload
(defun prot-embark-previous-line-or-mini (&optional arg)
  "Move to the next line or switch to the minibuffer.
This performs a regular motion for optional ARG lines, but when
point can no longer move in that direction, then it switches to
the minibuffer."
  (interactive "p")
  (let ((num (prot-common-number-negative arg))) ; from `prot-common.el'
    (if (bobp)
        (prot-minibuffer-focus-mini)    ; from `prot-minibuffer.el'
      (forward-line (or num 1)))))

;;;###autoload
(defun prot-embark-collection-kill-line ()
  "Delete line from Embark collect buffer."
  (interactive)
  (let* ((inhibit-read-only t)
         (eol (point-at-eol))
         (eol-dwim (if (= eol (point-max)) eol (1+ eol))))
    (save-excursion
      (goto-char (point-at-bol))
      (delete-region (point) eol-dwim))))

;;;###autoload
(defun prot-embark-collection-flush-lines (regexp)
  "`flush-lines' matching REGEXP in Embark collect buffers."
  (interactive
   (list (read-regexp "Flush lines matching regexp: ")))
  (let ((inhibit-read-only t))
    (if (derived-mode-p 'embark-collect-mode)
        (with-current-buffer (current-buffer)
            (save-excursion
              (goto-char (point-min))
              (flush-lines regexp)))
      (user-error "Not in an Embark collect buffer"))))

;;;###autoload
(defun prot-embark-collection-keep-lines (regexp)
  "`keep-lines' matching REGEXP in Embark collect buffers."
  (interactive
   (list (read-regexp "Keep lines matching regexp: ")))
  (let ((inhibit-read-only t))
    (if (derived-mode-p 'embark-collect-mode)
        (with-current-buffer (current-buffer)
            (save-excursion
              (goto-char (point-min))
              (keep-lines regexp)))
      (user-error "Not in an Embark collect buffer"))))

(declare-function embark-consult-preview-minor-mode "embark-consult")
(defvar embark-consult-preview-minor-mode)

;;;###autoload
(defun prot-embark-consult-preview-toggle ()
  "Toggle preview mode for Embark's Consult collections."
  (interactive)
  (when (featurep 'embark-consult)
    (require 'embark-consult)
    (if (and (bound-and-true-p embark-consult-preview-minor-mode)
             (derived-mode-p 'embark-collect-mode))
        (progn
          (remove-hook 'embark-collect-mode-hook #'embark-consult-preview-minor-mode)
          (embark-consult-preview-minor-mode -1))
      (add-hook 'embark-collect-mode-hook #'embark-consult-preview-minor-mode)
      (embark-consult-preview-minor-mode 1))))

(provide 'prot-embark)
;;; prot-embark.el ends here
