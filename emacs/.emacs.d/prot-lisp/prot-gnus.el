;;; prot-gnus.el --- Gnus tweaks for my dotemacs -*- lexical-binding: t -*-

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
;; This covers my Gnus tweaks, for use in my Emacs setup:
;; https://protesilaos.com/dotemacs.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

(require 'gnus)
(require 'prot-common)

(defgroup prot-gnus ()
  "Extensions for gnus and mail-related extras."
  :group 'gnus)

(defcustom prot-gnus-maildir-path-regexp "~/.mail/*/Inbox/new/"
  "Path passed to 'find' for checking new mail in maildir.
As this is passed to a shell command, one can use glob patterns."
  :type 'string
  :group 'prot-gnus)

(defcustom prot-gnus-message-gcc-header "Gcc: nnmaildir+pub:Sent"
  "Text of mail header for Gnus' Gcc.
This is used to prepopulate a new message composition buffer with
the appropriate paraphernalia."
  :type 'string
  :group 'prot-gnus)

;;;; Helper functions

(autoload 'auth-source-search "auth-source")

;;;###autoload
(defun prot-gnus-auth-get-field (host prop)
  "Find PROP in `auth-sources' for HOST entry."
  (let* ((source (auth-source-search :host host))
         (field (plist-get
                 (flatten-list source)
                 prop)))
    (if source
        field
      (user-error "No entry in auth sources"))))

(autoload 'message-fetch-field "message")
(autoload 'message-remove-header "message")
(autoload 'message-add-header "message")

;;;###autoload
(defun prot-gnus-message-header-gcc ()
  "While `gnus' is running, add pre-populated Gcc header.

The Gcc header places a copy of the outgoing message to the
appropriate directory of the IMAP server, as per the contents of
~/.authinfo.gpg.

In the absence of a Gcc header, the outgoing message will not
appear in the appropriate maildir directory, though it will still
be sent.

Add this function to `message-header-setup-hook'."
  (if (gnus-alive-p)
      (progn
        (when (message-fetch-field "Gcc")
          (message-remove-header "Gcc"))
        (message-add-header prot-gnus-message-gcc-header))
    (message "Gnus is not running. No GCC field inserted.")))

;;;; Mode line indicator

(defface prot-gnus-mail-count
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#61284f")
    (((class color) (min-colors 88) (background dark))
     :foreground "#fbd6f4")
    (t :foreground "magenta"))
  "Face for mode line indicator that shows a new mail count.")

(defvar prot-gnus-new-mail-string nil
  "New maildir count number for the mode line.")

(defun prot-gnus--new-mail ()
  "Search for new mail in personal maildir paths."
  (with-temp-buffer
    (shell-command
     (format "find %s -type f | wc -l" prot-gnus-maildir-path-regexp) t)
    (buffer-substring-no-properties (point-min) (1- (point-max)))))

(defun prot-gnus--mode-string (count)
  "Add properties to COUNT string."
  (when (not (string= count "0"))
    (propertize (format "@%s " count)
                'face 'prot-gnus-mail-count
                'help-echo "Number of new items in maildirs")))

(defvar prot-gnus--mode-line-mail-indicator nil
  "Internal variable used to store the state of new mails.")

(defun prot-gnus--mode-line-mail-indicator ()
  "Prepare mail count mode line indicator."
  (let* ((count (prot-gnus--new-mail))
         (indicator (prot-gnus--mode-string count))
         (old-indicator prot-gnus--mode-line-mail-indicator))
    ;; TODO 2021-04-19: Is appending to `global-mode-string' the best
    ;; way to show this information?
    (cond
     (count
      (setq global-mode-string (delete old-indicator global-mode-string))
      (setq global-mode-string (push indicator global-mode-string))
      (setq prot-gnus--mode-line-mail-indicator indicator))
     ((string= count "0")
      (setq global-mode-string (delete old-indicator global-mode-string))
      (setq prot-gnus--mode-line-mail-indicator nil)))))

(autoload 'gnus-topic-get-new-news-this-topic "gnus-topic")

(defun prot-gnus--topic-hook (&rest _)
  "Run `gnus-get-new-news-hook' when updating topics."
  (run-hooks 'gnus-get-new-news-hook))

(advice-add #'gnus-topic-get-new-news-this-topic :after #'prot-gnus--topic-hook)

;;;###autoload
(define-minor-mode prot-gnus-mail-indicator
  "Enable mode line indicator with counter for new mail."
  :init-value nil
  :global t
  (if prot-gnus-mail-indicator
      (progn
        (run-at-time t 60 #'prot-gnus--mode-line-mail-indicator)
        (add-hook 'gnus-get-new-news-hook #'prot-gnus--mode-line-mail-indicator)
        (force-mode-line-update t))
    (cancel-function-timers #'prot-gnus--mode-line-mail-indicator)
    (setq global-mode-string (delete prot-gnus--mode-line-mail-indicator global-mode-string))
    (remove-hook 'gnus-get-new-news-hook #'prot-gnus--mode-line-mail-indicator)
    (force-mode-line-update t)))

(provide 'prot-gnus)
;;; prot-gnus.el ends here
