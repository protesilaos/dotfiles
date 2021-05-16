;;; prot-mail.el --- Mail tweaks for my dotemacs -*- lexical-binding: t -*-

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
;; This covers my email tweaks, for use in my Emacs setup:
;; https://protesilaos.com/dotemacs.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

(require 'prot-common)

(defgroup prot-mail ()
  "Extensions for mail."
  :group 'mail)

(defcustom prot-mail-maildir-path-regexp "~/.mail/*/Inbox/new/"
  "Path passed to 'find' for checking new mail in maildir.
As this is passed to a shell command, one can use glob patterns.

The user must ensure that this path or regexp matches the one
specified in the mail syncing program (e.g. mbsync)."
  :type 'string
  :group 'prot-mail)

(defcustom prot-mail-mode-line-indicator-commands '(notmuch-refresh-this-buffer)
  "List of commands that will be advised to update the mode line.
The advice is designed to run a hook which is used internally by
the function `prot-mail-mail-indicator'."
  :type 'list
  :group 'prot-mail)

;;;; Helper functions

(autoload 'auth-source-search "auth-source")

;;;###autoload
(defun prot-mail-auth-get-field (host prop)
  "Find PROP in `auth-sources' for HOST entry."
  (let* ((source (auth-source-search :host host))
         (field (plist-get (flatten-list source) prop)))
    (if source
        field
      (user-error "No `%s' host with `%s' entry in auth sources" host prop))))

(defvar ebdb-db-list)
(autoload 'ebdb-load "ebdb")

(when (require 'ebdb nil t)
  (defun prot-mail-ebdb-message-setup ()
    "Load EBDB if not done already.
Meant to be assigned to a hook, such as `message-setup-hook'."
    (unless ebdb-db-list
      (ebdb-load))))

;;;; Mode line indicator

;; NOTE 2021-05-14: The following is a more generic approach that uses
;; find to search for new mail.  In my prot-notmuch.el I define an
;; alternative that checks for the "unread" tag, which works better for
;; my particular setup (refer to my prot-emacs.org for the relevant
;; commentary).

(defface prot-mail-mail-count
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#61284f")
    (((class color) (min-colors 88) (background dark))
     :foreground "#fbd6f4")
    (t :foreground "magenta"))
  "Face for mode line indicator that shows a new mail count.")

(defvar prot-mail-new-mail-string nil
  "New maildir count number for the mode line.")

(defun prot-mail--new-mail ()
  "Search for new mail in personal maildir paths."
  (with-temp-buffer
    (shell-command
     (format "find %s -type f | wc -l" prot-mail-maildir-path-regexp) t)
    (buffer-substring-no-properties (point-min) (1- (point-max)))))

(defun prot-mail--mode-string (count)
  "Add properties to COUNT string."
  (when (not (string= count "0"))
    (propertize (format "@%s " count)
                'face 'prot-mail-mail-count
                'help-echo "Number of new items in maildirs")))

(defvar prot-mail--mode-line-mail-indicator nil
  "Internal variable used to store the state of new mails.")

(defun prot-mail--mode-line-mail-indicator ()
  "Prepare new mail count mode line indicator."
  (let* ((count (prot-mail--new-mail))
         (indicator (prot-mail--mode-string count))
         (old-indicator prot-mail--mode-line-mail-indicator))
    (when old-indicator
      (setq global-mode-string (delete old-indicator global-mode-string)))
    (cond
     ((>= (string-to-number count) 1)
      (setq global-mode-string (push indicator global-mode-string))
      (setq prot-mail--mode-line-mail-indicator indicator))
     (t
      (setq prot-mail--mode-line-mail-indicator nil)))))

(defvar prot-mail--mode-line-mail-sync-hook nil
  "Hook to refresh the mode line for the mail indicator.")

(defun prot-mail--add-hook (&rest _)
  "Run `prot-mail--mode-line-mail-sync-hook'.
Meant to be used as advice after specified commands that should
update the mode line indicator with the new mail count."
  (run-hooks 'prot-mail--mode-line-mail-sync-hook))

;;;###autoload
(define-minor-mode prot-mail-mail-indicator
  "Enable mode line indicator with counter for new mail."
  :init-value nil
  :global t
  (if prot-mail-mail-indicator
      (progn
        (run-at-time t 60 #'prot-mail--mode-line-mail-indicator)
        (when prot-mail-mode-line-indicator-commands
          (dolist (fn prot-mail-mode-line-indicator-commands)
            (advice-add fn :after #'prot-mail--add-hook)))
        (add-hook 'prot-mail--mode-line-mail-sync-hook #'prot-mail--mode-line-mail-indicator)
        (force-mode-line-update t))
    (cancel-function-timers #'prot-mail--mode-line-mail-indicator)
    (setq global-mode-string (delete prot-mail--mode-line-mail-indicator global-mode-string))
    (remove-hook 'prot-mail--mode-line-mail-sync-hook #'prot-mail--mode-line-mail-indicator)
    (when prot-mail-mode-line-indicator-commands
      (dolist (fn prot-mail-mode-line-indicator-commands)
        (advice-remove fn #'prot-mail--add-hook)))
    (force-mode-line-update t)))

(provide 'prot-mail)
;;; prot-mail.el ends here
