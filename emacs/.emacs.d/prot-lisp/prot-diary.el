;;; prot-diary.el --- Extensions for the diary and calendar -*- lexical-binding: t -*-

;; Copyright (C) 2021  Protesilaos Stavrou

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
;; Extensions for the diary and calendar, intended for my Emacs setup:
;; <https://protesilaos.com/dotemacs/>.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

(require 'calendar)
(require 'diary-lib)
(require 'prot-common)

(defgroup prot-diary ()
  "Tweaks for the calendar and diary."
  :group 'diary)

;;;; Commands and utilities

(defun prot-diary--list-entries (n inhibit)
  "Check for N days with diary entries.
When optional INHIBIT is non-nil, do not show thediary buffer."
  (let ((inhibit-message t)
        (hide (if inhibit t nil)))
    (diary-list-entries (calendar-current-date) n hide)))

(defvar prot-diary--current-window-configuration nil
  "Current window configuration.")

(defvar prot-diary--current-window-configuration-point nil
  "Point in current window configuration.")

(defun prot-diary--store-window-configuration ()
  "Store current window configuration and point."
  (setq prot-diary--current-window-configuration (current-window-configuration))
  (setq prot-diary--current-window-configuration-point (point)))

(defun prot-diary--restore-window-configuration ()
  "Restore `prot-diary--store-window-configuration'."
  (when prot-diary--current-window-configuration
    (set-window-configuration prot-diary--current-window-configuration))
  (when prot-diary--current-window-configuration-point
    (goto-char prot-diary--current-window-configuration-point)))

(autoload 'message-goto-body "message")

;;;###autoload
(defun prot-diary-mail-entries (&optional ndays)
  "Email diary entries for NDAYS or `diary-mail-days'.

With optional DAYS as a positive integer, produce a list for N
days including the current one (so 2 is today and tomorrow).
Otherwise use `diary-mail-days'.

Alternative of `diary-mail-entries'.  Does not show the diary
buffer after sending the email and does not send a mail when no
entries are present (what is the point of first notifying me at
my inbox and then telling me 'Oh, nothing of interest here'?)."
  (interactive "p")
  (if (or (string-equal diary-mail-addr "")
          (eq diary-mail-addr nil))
      (user-error "You must set `diary-mail-addr' to use this command")
    (let ((entries)
          (diary-display-function #'diary-fancy-display)
          (diary-mail-addr user-mail-address)
          (mail-user-agent 'message-user-agent)
          (n (or ndays diary-mail-days)))
      (prot-common-number-interger-positive-p n)
      (prot-diary--store-window-configuration)
      (diary-list-entries (calendar-current-date) (or n diary-mail-days))
      (if (prot-diary--list-entries n t)
          (progn
            (with-current-buffer (get-buffer diary-fancy-buffer)
              (setq entries (buffer-string))
              (kill-buffer) ; FIXME 2021-04-13: `bury-buffer' does not bury it...
              (prot-diary--restore-window-configuration))
            (compose-mail diary-mail-addr
                          (concat "Diary entries generated "
                                  (calendar-date-string (calendar-current-date)))
                          nil)
            (message-goto-body)
            (insert entries)
            (funcall (get mail-user-agent 'sendfunc)))
        (message "No diary entries; skipping email delivery")))))

;;;###autoload
(defun prot-diary-display-entries (&optional days)
  "Display diary entries, if any.
With optional DAYS as a positive integer, produce a list for N
days including the current one (so 2 is today and tomorrow).
Otherwise use `diary-mail-days'."
  (interactive "p")
  (let ((n (or days diary-mail-days)))
    (prot-common-number-interger-positive-p n)
    (unless (prot-diary--list-entries n nil)
      (message "No diary entries; skipping display"))))

;;;###autoload
(defun prot-diary-edit-diary ()
  "Visit `diary-file'."
  (interactive)
  (let ((diary diary-file))
    (if (and (boundp 'diary-file)
             (file-regular-p diary))
        (find-file (expand-file-name diary))
      (error "No regular file `diary-file' is available"))))

;;;###autoload
(defun prot-diary-insert-entry (date)
  "Insert diary entry for DATE formatted as plain text."
  (interactive
   (list (read-string "Input date: " (format-time-string "%F"))))
  (diary-make-entry
   (concat (or date (format-time-string "%F")) "\s")))

(defvar align-default-spacing)

;;;###autoload
(defun prot-diary-align-timestamped-entries (beg end)
  "Align and indent region between BEG and END."
  (interactive "r")
  (let ((align-default-spacing 3))
    (align-regexp beg end (concat diary-time-regexp "\\( \\)") 2)
    (indent-rigidly beg end 4)))

;;;###autoload
(defun prot-diary-newline-indent ()
  "Insert newline and indent by four spaces."
  (interactive)
  (delete-horizontal-space)
  (newline)
  (insert (make-string 4 ?\s)))

;;;; Fontification extras

(defface prot-diary-calendar-anniversary-mark
  '((((class color) (min-colors 88) (background light))
     :background "#fff1f0" :foreground "#a60000")
    (((class color) (min-colors 88) (background dark))
     :background "#2c0614" :foreground "#ff8059")
    (t :foreground "red"))
  "Face to mark anniversaries in the calendar.")

(defface prot-diary-calendar-administrative-mark
  '((((class color) (min-colors 88) (background light))
     :background "#fff3da" :foreground "#813e00")
    (((class color) (min-colors 88) (background dark))
     :background "#221000" :foreground "#eecc00")
    (t :foreground "yellow"))
  "Face to mark administrative tasks in the calendar.")

(defface prot-diary-calendar-event-mark
  '((((class color) (min-colors 88) (background light))
     :background "#aceaac" :foreground "#004c00")
    (((class color) (min-colors 88) (background dark))
     :background "#00422a" :foreground "#9ff0cf")
    (t :foreground "green"))
  "Face to mark events in the calendar.")

(defface prot-diary-calendar-mundane-mark
  '((((class color) (min-colors 88) (background light))
     :background "#f0f0f0" :foreground "#505050")
    (((class color) (min-colors 88) (background dark))
     :background "#191a1b" :foreground "#a8a8a8")
    (t :inherit shadow))
  "Face to mark mundane tasks in the calendar.")

;; I might expand this further, depending on my usage patterns and the
;; conventions I establish over time.
(defconst prot-diary-font-lock-keywords
  `((,(format "^%s?\\(%s\\)" (regexp-quote diary-nonmarking-symbol)
             (regexp-quote diary-sexp-entry-symbol))
     (1 'font-lock-constant-face t))
    (diary-font-lock-sexps
     (0 'font-lock-function-name-face t))
    (,(format "^%s" (regexp-quote diary-nonmarking-symbol))
     (0 'font-lock-negation-char-face t))
    (,(format "%s.*" diary-comment-start)
     (0 'font-lock-comment-face)))
  "Rules for extra Diary fontification.")

(defvar outline-regexp)
(defvar outline-heading-end-regexp)

(defun prot-diary--outline-level ()
  "Determine Outline heading level.
To be assigned to the variable `outline-level'."
  (let ((regexp "\\(;;+\\{2,\\}\\) [^ \t\n]"))
    (looking-at regexp)
    (- (- (match-end 1) (match-beginning 1)) 2)))

(defun prot-diary--extras-setup ()
  "Additional setup for Diary mode buffers.
Applies `prot-diary-font-lock-keywords' and specifies what
constitutes a heading for the purposes of Outline minor mode."
  (when (derived-mode-p 'diary-mode)
    (font-lock-flush (point-min) (point-max))
    (font-lock-add-keywords nil prot-diary-font-lock-keywords t)
    (setq outline-regexp (format "%s+\\{2,\\} [^ \t\n]" diary-comment-start))
    (setq outline-level #'prot-diary--outline-level)
    (setq outline-heading-end-regexp (format "%s$" diary-comment-end))))

(add-hook 'diary-mode-hook #'prot-diary--extras-setup)

(defconst prot-diary-date-pattern
  "^!?\\(\\([0-9]+\\|\\*\\)[-/]\\([0-9]+\\|\\*\\)[-/]\\([0-9]+\\|\\*\\)\\|%%\\)"
  "Date pattern found in my diary (NOT ALL POSSIBLE PATTERNS).")

;;;###autoload
(defun prot-diary-heading-next (&optional arg)
  "Move to next or optional ARGth Dired subdirectory heading.
For more on such headings, read `dired-maybe-insert-subdir'."
  (interactive "p")
  (let ((heading prot-diary-date-pattern))
    (goto-char (point-at-eol))
    (re-search-forward heading nil t (or arg nil))
    (goto-char (match-beginning 1))
    (goto-char (point-at-bol))))

;;;###autoload
(defun prot-diary-heading-previous (&optional arg)
  "Move to previous or optional ARGth Dired subdirectory heading.
For more on such headings, read `dired-maybe-insert-subdir'."
  (interactive "p")
  (let ((heading prot-diary-date-pattern))
    (goto-char (point-at-bol))
    (re-search-backward heading nil t (or arg nil))
    (goto-char (point-at-bol))))

;;;; Holidays

(defvar prot-diary-local-holidays
  '((holiday-greek-orthodox-easter 0 "Easter")
    (holiday-greek-orthodox-easter -48 "Green Monday") ; REVIEW: is this correct?
    (holiday-fixed 5 9 "Day of Europe"))
  "I don't care about any of those---EXPERIMENTAL.")

(provide 'prot-diary)
;;; prot-diary.el ends here
