;;; prot-notmuch.el --- Tweaks for my notmuch.el configurations -*- lexical-binding: t -*-

;; Copyright (C) 2021  Protesilaos Stavrou

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos.com/dotemacs
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
;; This covers my tweaks for notmuch.el that are meant for use in my
;; Emacs setup: https://protesilaos.com/dotemacs.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

(require 'prot-common)

(defgroup prot-notmuch ()
  "Extensions for notmuch.el."
  :group 'notmuch)

(defcustom prot-notmuch-search-field-width 100 ; Or use something like (/ (frame-width) 2)
  "Number of characters for the width of search files.
Those fields appear in the Notmuch hello buffer.  See
`prot-notmuch-hello-insert-recent-searches'."
  :type 'integer
  :group 'prot-notmuch)

;;;; Utilities

(defface prot-notmuch-encrypted-tag
  '((default :inherit italic)
    (((class color) (min-colors 88) (background light))
     :foreground "#5d3026")
    (((class color) (min-colors 88) (background dark))
     :foreground "#f8dec0"))
  "Face for the 'encrypted' tag or related in Notmuch.
Refer to the variable `notmuch-tag-formats' for how to assign
those.")

(defface prot-notmuch-sent-tag
  '((default :inherit italic)
    (((class color) (min-colors 88) (background light))
     :foreground "#005e00")
    (((class color) (min-colors 88) (background dark))
     :foreground "#44bc44"))
  "Face for the 'sent' tag or related in Notmuch.
Refer to the variable `notmuch-tag-formats' for how to assign
those.")

(defface prot-notmuch-spam-tag
  '((default :inherit italic)
    (((class color) (min-colors 88) (background light))
     :foreground "#70480f")
    (((class color) (min-colors 88) (background dark))
     :foreground "#c4d030"))
  "Face for the 'spam' tag or related in Notmuch.
Refer to the variable `notmuch-tag-formats' for how to assign
those.")

(defface prot-notmuch-ref-tag
  '((default :inherit italic)
    (((class color) (min-colors 88) (background light))
     :foreground "#005a5f")
    (((class color) (min-colors 88) (background dark))
     :foreground "#6ae4b9"))
  "Face for the 'ref' tag or related in Notmuch.
Refer to the variable `notmuch-tag-formats' for how to assign
those.")

(defface prot-notmuch-todo-tag
  '((default :inherit italic)
    (((class color) (min-colors 88) (background light))
     :foreground "#a60000")
    (((class color) (min-colors 88) (background dark))
     :foreground "#ff8059"))
  "Face for the 'todo' tag or related in Notmuch.
Refer to the variable `notmuch-tag-formats' for how to assign
those.")

(defface prot-notmuch-widget-field
  '((((class color) (min-colors 88) (background light))
     :underline "#d7d7d7")
    (((class color) (min-colors 88) (background dark))
     :underline "#323232")
    (t :underline t))
  "Face for search fields in the Notmuch hello buffer.")

(declare-function message-fetch-field "message")
(declare-function message-remove-header "message")
(declare-function message-add-header "message")
(declare-function message-sort-headers "message")
(declare-function notmuch-fcc-header-setup "notmuch")

;; NOTE 2021-05-18: I used to have something like this when I was using
;; Gnus and thought it would be useful here, but it ultimately isn't.  I
;; just use `notmuch-mua-new-mail'.

;; ;;;###autoload
;; (defun prot-notmuch-message-headers ()
;;   "While `notmuch' is running, configure From header.
;; Add this function to `message-header-setup-hook'."
;;   (when (and (eq mail-user-agent 'notmuch-user-agent)
;;              (eq last-command 'compose-mail))
;;     (when (message-fetch-field "From")
;;       (message-remove-header "From"))
;;     (message-add-header (format "From: %s <%s>" user-full-name user-mail-address))
;;     (notmuch-fcc-header-setup)
;;     (message-sort-headers)))

(defun prot-notmuch-widget-field-face-remap ()
  "Set up extra highlighting for widget fields in Notmuch hello.
Add this to `notmuch-hello-mode-hook'."
  (when (derived-mode-p 'notmuch-hello-mode)
    (face-remap-add-relative 'widget-field 'prot-notmuch-widget-field)))

(defvar notmuch-saved-searches)
(defvar notmuch-show-empty-saved-searches)
(defvar notmuch-search-oldest-first)
(defvar notmuch-saved-search-sort-function)

(declare-function notmuch-hello-query-counts "notmuch")
(declare-function notmuch-saved-search-sort-function "notmuch")
(declare-function notmuch-hello-nice-number "notmuch")
(declare-function notmuch-hello-reflect "notmuch")
(declare-function notmuch-hello-widget-search "notmuch")

;; Simplified variant of what is available in `notmuch-hello.el'.
(defun prot-notmuch-hello-insert-saved-searches ()
  "Single column saved search buttons for Notmuch hello.
Add this to `notmuch-hello-sections'."
  (let ((searches (notmuch-hello-query-counts
		           (if notmuch-saved-search-sort-function
		               (funcall notmuch-saved-search-sort-function
				                notmuch-saved-searches)
		             notmuch-saved-searches)
		           :show-empty-searches notmuch-show-empty-saved-searches))
        (count 0))
    (mapc (lambda (elem)
	        (when elem
	          (let* ((name (plist-get elem :name))
		             (query (plist-get elem :query))
		             (oldest-first (cl-case (plist-get elem :sort-order)
				                     (newest-first nil)
				                     (oldest-first t)
				                     (otherwise notmuch-search-oldest-first)))
		             (search-type (plist-get elem :search-type))
		             (msg-count (plist-get elem :count)))
		        (widget-insert (format "\n%8s "
				                       (notmuch-hello-nice-number msg-count)))
		        (widget-create 'push-button
			                   :notify #'notmuch-hello-widget-search
			                   :notmuch-search-terms query
			                   :notmuch-search-oldest-first oldest-first
			                   :notmuch-search-type search-type
			                   name)))
	        (cl-incf count))
	      (notmuch-hello-reflect searches 1))))
                                               
(defvar notmuch-hello-indent)
(defvar notmuch-search-history)
(defvar notmuch-hello-recent-searches-max)
(declare-function notmuch-hello-search "notmuch")

;; Adapted from `notmuch-hello.el'.
(define-widget 'prot-notmuch-search-item 'item
  "A widget that shows recent search queries."
  :format "%v\n"
  :value-create 'prot-notmuch-search-item-value-create)

;; Adapted from `notmuch-hello.el'.
(defun prot-notmuch-search-item-value-create (widget)
  "Specify value of search WIDGET."
  (let ((value (widget-get widget :value)))
    (widget-insert (make-string notmuch-hello-indent ?\s))
    (widget-create 'editable-field
		   :size (widget-get widget :size)
		   :parent widget
		   :action #'notmuch-hello-search
		   value)))

(defun prot-notmuch--search-width ()
  "Confirm `prot-notmuch-search-field-width' is positive integer."
  (let ((width prot-notmuch-search-field-width))
    (if (prot-common-number-interger-positive-p width)
        width
      (error "Search field width must be a positive integer"))))

;; Adapted from `notmuch-hello.el'.
(defun prot-notmuch-hello-insert-recent-searches ()
  "Insert widget with recent search terms.
Add this to `notmuch-hello-sections'."
  (when notmuch-search-history
    (widget-insert "\n\n")
    (widget-insert "Recent searches: ")
    (widget-insert "\n\n")
    (let ((width (prot-notmuch--search-width)))
      (dolist (search (seq-take notmuch-search-history
				notmuch-hello-recent-searches-max))
	(widget-create 'prot-notmuch-search-item :value search :size width)))))

;;;; Commands

(autoload 'notmuch-refresh-this-buffer "notmuch")
(autoload 'notmuch-refresh-all-buffers "notmuch")

;;;###autoload
(defun prot-notmuch-refresh-buffer (&optional arg)
  "Run `notmuch-refresh-this-buffer'.
With optional prefix ARG (\\[universal-argument]) call
`notmuch-refresh-all-buffers'."
  (interactive "P")
  (if arg
      (notmuch-refresh-all-buffers)
    (notmuch-refresh-this-buffer)))

;;;; Mode line unread indicator

;; NOTE 2021-05-14: I have an alternative to this in prot-mail.el which
;; does not rely on notmuch as it uses find instead.  The following
;; approach is specific to my setup and is what I prefer now.

(defcustom prot-notmuch-mode-line-count-args "tag:unread and tag:inbox"
  "Arguments to pass to 'notmuch count' for counting new mail."
  :type 'string
  :group 'prot-notmuch)

(defcustom prot-notmuch-mode-line-indicator-commands '(notmuch-refresh-this-buffer)
  "List of commands that will be advised to update the mode line.
The advice is designed to run a hook which is used internally by
the function `prot-notmuch-mail-indicator'."
  :type 'list
  :group 'prot-notmuch)

(defface prot-notmuch-mail-count
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#61284f")
    (((class color) (min-colors 88) (background dark))
     :foreground "#fbd6f4")
    (t :foreground "magenta"))
  "Face for mode line indicator that shows a new mail count.")

(defvar prot-notmuch-new-mail-string nil
  "New maildir count number for the mode line.")

(defun prot-notmuch--new-mail ()
  "Search for new mail in personal maildir paths."
  (with-temp-buffer
    (shell-command
     (format "notmuch count %s" prot-notmuch-mode-line-count-args) t)
    (buffer-substring-no-properties (point-min) (1- (point-max)))))

(defun prot-notmuch--mode-string (count)
  "Add properties to COUNT string."
  (when (not (string= count "0"))
    (propertize (format "@%s " count)
                'face 'prot-notmuch-mail-count
                'help-echo "New mails matching `prot-notmuch-mode-line-count-args'")))

(defvar prot-notmuch--mode-line-mail-indicator nil
  "Internal variable used to store the state of new mails.")

(defun prot-notmuch--mode-line-mail-indicator ()
  "Prepare new mail count mode line indicator."
  (let* ((count (prot-notmuch--new-mail))
         (indicator (prot-notmuch--mode-string count))
         (old-indicator prot-notmuch--mode-line-mail-indicator))
    (when old-indicator
      (setq global-mode-string (delete old-indicator global-mode-string)))
    (cond
     ((>= (string-to-number count) 1)
      (setq global-mode-string (push indicator global-mode-string))
      (setq prot-notmuch--mode-line-mail-indicator indicator))
     (t
      (setq prot-notmuch--mode-line-mail-indicator nil)))))

(defvar prot-notmuch--mode-line-mail-sync-hook nil
  "Hook to refresh the mode line for the mail indicator.")

(defun prot-notmuch--add-hook (&rest _)
  "Run `prot-notmuch--mode-line-mail-sync-hook'.
Meant to be used as advice after specified commands that should
update the mode line indicator with the new mail count."
  (run-hooks 'prot-notmuch--mode-line-mail-sync-hook))

;;;###autoload
(define-minor-mode prot-notmuch-mail-indicator
  "Enable mode line indicator with counter for new mail."
  :init-value nil
  :global t
  (if prot-notmuch-mail-indicator
      (progn
        (run-at-time t 60 #'prot-notmuch--mode-line-mail-indicator)
        (when prot-notmuch-mode-line-indicator-commands
          (dolist (fn prot-notmuch-mode-line-indicator-commands)
            (advice-add fn :after #'prot-notmuch--add-hook)))
        (add-hook 'prot-notmuch--mode-line-mail-sync-hook #'prot-notmuch--mode-line-mail-indicator)
        (force-mode-line-update t))
    (cancel-function-timers #'prot-notmuch--mode-line-mail-indicator)
    (setq global-mode-string (delete prot-notmuch--mode-line-mail-indicator global-mode-string))
    (remove-hook 'prot-notmuch--mode-line-mail-sync-hook #'prot-notmuch--mode-line-mail-indicator)
    (when prot-notmuch-mode-line-indicator-commands
      (dolist (fn prot-notmuch-mode-line-indicator-commands)
        (advice-remove fn #'prot-notmuch--add-hook)))
    (force-mode-line-update t)))

(provide 'prot-notmuch)
;;; prot-notmuch.el ends here
