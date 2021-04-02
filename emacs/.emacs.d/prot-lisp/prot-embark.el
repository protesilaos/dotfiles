;;; prot-embark.el --- Extensions to embark.el for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2020-2021  Protesilaos Stavrou

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos.com/dotemacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))

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
;;
;; NOTE 2021-04-02: Everything pertaining to the completions' buffer has
;; been moved to `prot-minibuffer.el'.  What I have here about that
;; niche was useful because I was using Embark's live updating
;; completions' collection buffer.  However, Emacs28 provides a
;; one-column layout for the default Completions' buffer, so it is easy
;; to move that code there and adapt it to work without the otherwise
;; minor Embark extras.
;;
;; As such, most of this file's contents are kept here for posterity
;; and/or are subject to review/removal.

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

(defvar embark-collect--kind)

;; Thanks to Omar Antolín Camarena for sharing this:
;; <https://github.com/oantolin/embark/issues/114#issuecomment-761583395>
(defun prot-embark--live-completions-p ()
  "Determine whether current collection is for live completions."
  (and (derived-mode-p 'embark-collect-mode)
       (eq embark-collect--kind :completions)))

(defface prot-embark-hl-line
  '((default :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#b0d8ff" :foreground "#000000")
    (((class color) (min-colors 88) (background dark))
     :background "#103265" :foreground "#ffffff")
    (t :inherit (font-lock-string-face elfeed-search-title-face)))
  "Face for current line in Embark completions."
  :group 'prot-embark)

(defface prot-embark-line-number
  '((default :inherit default)
    (((class color) (min-colors 88) (background light))
     :background "#f2eff3" :foreground "#252525")
    (((class color) (min-colors 88) (background dark))
     :background "#151823" :foreground "#dddddd")
    (t :inverse-video t))
  "Face for line numbers in Embark completions."
  :group 'prot-embark)

(defface prot-embark-line-number-current-line
  '((default :inherit default)
    (((class color) (min-colors 88) (background light))
     :background "#8ac7ff" :foreground "#000000")
    (((class color) (min-colors 88) (background dark))
     :background "#142a79" :foreground "#ffffff")
    (t :inverse-video t))
  "Face for current line number in Embark completions."
  :group 'prot-embark)

(autoload 'display-line-numbers-mode "display-line-numbers")
(autoload 'face-remap-remove-relative "face-remap")

;;;###autoload
(defun prot-embark-display-line-numbers ()
  "Set up line numbers for live Embark collect buffers.
Add this to `embark-collect-mode-hook'."
  (if (prot-embark--live-completions-p)
      (progn
        (face-remap-add-relative 'line-number 'prot-embark-line-number)
        (face-remap-add-relative 'line-number-current-line
                                 'prot-embark-line-number-current-line)
        (display-line-numbers-mode 1))
    (display-line-numbers-mode -1)
    ;; TODO: can we avoid `face-remap-add-relative' and just use the
    ;; value it previously returned?
    (face-remap-remove-relative
     (face-remap-add-relative 'line-number
                              'prot-embark-line-number))
    (face-remap-remove-relative
     (face-remap-add-relative 'line-number-current-line
                              'prot-embark-line-number-current-line))))

;;;###autoload
(defun prot-embark-hl-line ()
  "Set up line highlighting for live Embark collect buffers.
Add this to `embark-collect-mode-hook'."
  (if (prot-embark--live-completions-p)
      (progn
        (face-remap-add-relative 'hl-line 'prot-embark-hl-line)
        (hl-line-mode 1))
    (hl-line-mode -1)
    ;; TODO: same as above with regard to `face-remap-add-relative'.
    (face-remap-remove-relative
     (face-remap-add-relative 'hl-line 'prot-embark-hl-line))))

;;;###autoload
(defun prot-embark-completions-cursor ()
  "`prot-minibuffer-completions-cursor' for Embark completions.
Add this to `embark-collect-mode-hook'."
  (if (prot-embark--live-completions-p)
      (prot-minibuffer-completions-cursor) ; from `prot-minibuffer.el'
    (kill-local-variable 'cursor-type)))

(autoload 'embark-quit "embark")

;; Thanks to Karthik Chikmagalur for providing an earlier version of
;; `prot-embark-keyboard-quit' command!  Sources to Karthik's work:
;;
;; + https://karthinks.com/
;; + https://github.com/karthink/.emacs.d/tree/clean
;;
;;;###autoload
(defun prot-embark-keyboard-quit ()
  "Control the exit behaviour for Embark collect buffers.

If in a live Embark collect/completions buffer and unless the
region is active, run `abort-recursive-edit'.  Otherwise run
`keyboard-quit'.

If the region is active, deactivate it.  A second invocation of
this command is then required to abort the session.

This is meant to be bound in `embark-collect-mode-map'."
  (interactive)
  (if (prot-embark--live-completions-p)
      (if (use-region-p)
          (keyboard-quit)
        (kill-buffer)
        (abort-recursive-edit))
    (keyboard-quit)))

(autoload 'embark-collect-completions "embark")

;;;###autoload
(defun prot-embark-completions-toggle ()
  "Toggle `embark-collect-completions'."
  (interactive)
  (if (prot-embark--live-buffer-p)
      (kill-buffer embark-collect-linked-buffer)
    (embark-collect-completions)))

(declare-function embark--act "embark")
(declare-function embark--target "embark")
(autoload 'embark-default-action "embark")

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

;; NOTE 2021-02-06: De facto deprecated in favour of Consult's variants
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

;; NOTE 2021-02-06: De facto deprecated in favour of Consult's variants
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
  (when (require 'embark-consult nil t)
    (if (and (bound-and-true-p embark-consult-preview-minor-mode)
             (derived-mode-p 'embark-collect-mode))
        (progn
          (remove-hook 'embark-collect-mode-hook #'embark-consult-preview-minor-mode)
          (embark-consult-preview-minor-mode -1))
      (add-hook 'embark-collect-mode-hook #'embark-consult-preview-minor-mode)
      (embark-consult-preview-minor-mode 1))))

;; NOTE: I keep this around for when I do videos, otherwise I do not use
;; it.  It requires `which-key' to display key hints.
(defvar embark-action-indicator)
(defvar embark-become-indicator)
(declare-function which-key--show-keymap "which-key")
(declare-function which-key--hide-popup-ignore-command "which-key")

(defvar prot-embark--which-key-state nil
  "Store state of Embark's `which-key' hints.")

;;;###autoload
(defun prot-embark-toggle-which-key ()
  "Toggle `which-key' hints for Embark actions."
  (interactive)
  (if prot-embark--which-key-state
      (progn
        (setq embark-action-indicator
                   (let ((act (propertize "Act" 'face 'highlight)))
                     (cons act (concat act " on '%s'"))))
        (setq prot-embark--which-key-state nil))
    (setq embark-action-indicator
          (lambda (map _target)
            (which-key--show-keymap "Embark" map nil nil 'no-paging)
            #'which-key--hide-popup-ignore-command)
          embark-become-indicator embark-action-indicator)
    (setq prot-embark--which-key-state t)))

(provide 'prot-embark)
;;; prot-embark.el ends here
