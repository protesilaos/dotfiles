;;; prot-notmuch.el --- Tweaks for my notmuch.el configurations -*- lexical-binding: t -*-

;; Copyright (C) 2021-2025  Protesilaos Stavrou

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos.com/emacs/dotemacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))

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
;; Emacs setup: https://protesilaos.com/emacs/dotemacs.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

(require 'prot-common)
(eval-when-compile (require 'cl-lib))

(defgroup prot-notmuch ()
  "Extensions for notmuch.el."
  :group 'notmuch)

(defcustom prot-notmuch-delete-tag "del"
  "Single tag that applies to mail marked for deletion.
This is used by `prot-notmuch-delete-mail'."
  :type 'string
  :group 'prot-notmuch)

(defcustom prot-notmuch-mark-delete-tags
  `(,(format "+%s" prot-notmuch-delete-tag) "-inbox" "-unread")
  "List of tags to mark for deletion.
To actually delete email, refer to `prot-notmuch-delete-mail'."
  :type '(repeat string)
  :group 'prot-notmuch)

(defcustom prot-notmuch-mark-flag-tags '("+flag" "-unread")
  "List of tags to mark as important (flagged).
This gets the `notmuch-tag-flagged' face, if that is specified in
`notmuch-tag-formats'."
  :type '(repeat string)
  :group 'prot-notmuch)

(defcustom prot-notmuch-mark-spam-tags '("+spam" "-inbox" "-unread")
  "List of tags to mark as spam."
  :type '(repeat string)
  :group 'prot-notmuch)

;;;; Commands

(autoload 'notmuch-interactive-region "notmuch")
(autoload 'notmuch-tag-change-list "notmuch")
(autoload 'notmuch-search-next-thread "notmuch")
(autoload 'notmuch-search-tag "notmuch")

(defmacro prot-notmuch-search-tag-thread (name tags)
  "Produce NAME function parsing TAGS."
  (declare (indent defun))
  `(defun ,name (&optional untag beg end)
     ,(format
       "Mark with `%s' the currently selected thread.

Operate on each message in the currently selected thread.  With
optional BEG and END as points delimiting a region that
encompasses multiple threads, operate on all those messages
instead.

With optional prefix argument (\\[universal-argument]) as UNTAG,
reverse the application of the tags.

This function advances to the next thread when finished."
       tags)
     (interactive (cons current-prefix-arg (notmuch-interactive-region)))
     (when ,tags
       (notmuch-search-tag
        (notmuch-tag-change-list ,tags untag) beg end))
     (when (eq beg end)
       (notmuch-search-next-thread))))

(prot-notmuch-search-tag-thread
  prot-notmuch-search-delete-thread
  prot-notmuch-mark-delete-tags)

(prot-notmuch-search-tag-thread
  prot-notmuch-search-flag-thread
  prot-notmuch-mark-flag-tags)

(prot-notmuch-search-tag-thread
  prot-notmuch-search-spam-thread
  prot-notmuch-mark-spam-tags)

(defmacro prot-notmuch-show-tag-message (name tags)
  "Produce NAME function parsing TAGS."
  (declare (indent defun))
  `(defun ,name (&optional untag)
     ,(format
       "Apply `%s' to message.

With optional prefix argument (\\[universal-argument]) as UNTAG,
reverse the application of the tags."
       tags)
     (interactive "P")
     (when ,tags
       (apply 'notmuch-show-tag-message
	          (notmuch-tag-change-list ,tags untag)))))

(prot-notmuch-show-tag-message
  prot-notmuch-show-delete-message
  prot-notmuch-mark-delete-tags)

(prot-notmuch-show-tag-message
  prot-notmuch-show-flag-message
  prot-notmuch-mark-flag-tags)

(prot-notmuch-show-tag-message
  prot-notmuch-show-spam-message
  prot-notmuch-mark-spam-tags)

(autoload 'notmuch-refresh-this-buffer "notmuch")
(autoload 'notmuch-refresh-all-buffers "notmuch")

(defun prot-notmuch-refresh-buffer (&optional arg)
  "Run `notmuch-refresh-this-buffer'.
With optional prefix ARG (\\[universal-argument]) call
`notmuch-refresh-all-buffers'."
  (interactive "P")
  (if arg
      (notmuch-refresh-all-buffers)
    (notmuch-refresh-this-buffer)))

;;;###autoload
(defun prot-notmuch-delete-mail ()
  "Permanently delete mail marked as `prot-notmuch-delete-mail'.
Prompt for confirmation before carrying out the operation.

Do not attempt to refresh the index.  This will be done upon the
next invocation of 'notmuch new'."
  (interactive)
  (let* ((del-tag prot-notmuch-delete-tag)
         (count
          (string-to-number
           (with-temp-buffer
             (shell-command
              (format "notmuch count tag:%s" prot-notmuch-delete-tag) t)
             (buffer-substring-no-properties (point-min) (1- (point-max))))))
         (mail (if (> count 1) "mails" "mail")))
    (unless (> count 0)
      (user-error "No mail marked as `%s'" del-tag))
    (when (yes-or-no-p
           (format "Delete %d %s marked as `%s'?" count mail del-tag))
      (shell-command
       (format "notmuch search --output=files --format=text0 tag:%s | xargs -r0 rm" del-tag)
       t))))

;;;; SourceHut-related setup

(defconst prot-notmuch-patch-control-codes
  '("PROPOSED" "NEEDS_REVISION" "SUPERSEDED" "APPROVED" "REJECTED" "APPLIED")
  "Control codes for SourceHut patches.
See `prot-notmuch-patch-add-email-control-code' for how to apply
them.")

(defun prot-notmuch--rx-in-sourcehut-mail (rx-group string)
  "Return RX-GROUP of SourceHut mail in STRING."
  (when (string-match-p "lists\\.sr\\.ht" string)
    (string-clean-whitespace
     (replace-regexp-in-string
      ".*?[<]?\\(\\([-a-zA-Z0-9=._+~/]+\\)@\\(lists\\.sr\\.ht\\)\\)[>]?.*?"
      (format "\\%s" rx-group) string))))

(declare-function notmuch-show-get-header "notmuch-show" (header &optional props))
(declare-function message-fetch-field "message" (header &optional first))

(defun prot-notmuch--get-to-or-cc-header ()
  "Get appropriate To or Cc header."
  (cond
   ((derived-mode-p 'notmuch-message-mode)
    (concat (message-fetch-field "To") " " (message-fetch-field "Cc")))
   ((derived-mode-p 'notmuch-show-mode)
    (concat (notmuch-show-get-header :To) " " (notmuch-show-get-header :Cc)))))

;; NOTE 2022-04-19: This assumes that we only have one list...  I think
;; that is okay, but it might cause problems.
(defun prot-notmuch--extract-sourcehut-mail (rx-group)
  "Extract RX-GROUP from SourceHut mailing list address.
1 is the full email address, 2 is the local part, while 3 is the
domain."
  (prot-notmuch--rx-in-sourcehut-mail
   rx-group (prot-notmuch--get-to-or-cc-header)))

(declare-function message-add-header "message" (&rest headers))

;; Read: <https://man.sr.ht/lists.sr.ht/#email-controls>.
;;;###autoload
(defun prot-notmuch-patch-add-email-control-code (control-code)
  "Add custom header for SourceHut email controls.
The CONTROL-CODE is among `prot-notmuch-patch-control-codes'."
  (interactive
   (list (completing-read "Select control code: " prot-notmuch-patch-control-codes nil t)))
  (if (member control-code prot-notmuch-patch-control-codes)
    (unless (message-fetch-field "X-Sourcehut-Patchset-Update")
      (message-add-header (format "X-Sourcehut-Patchset-Update: %s" control-code)))
    (user-error "%s is not specified in `prot-notmuch-patch-control-codes'" control-code)))

;;;###autoload
(defun prot-notmuch-ask-sourcehut-control-code ()
  "Use `prot-notmuch-patch-add-email-control-code' programmatically.
Add this to `notmuch-mua-send-hook'."
  (when-let* ((header (message-fetch-field "Subject"))
              (subject (when (>= (length header) 6) (substring header 0 6)))
              ((string= "[PATCH" subject)) ; Is [ always there?
              ((prot-notmuch--extract-sourcehut-mail 1))
              ((not (message-fetch-field "X-Sourcehut-Patchset-Update")))
              ((y-or-n-p "Add control code for SourceHut PATCH?")))
    (call-interactively #'prot-notmuch-patch-add-email-control-code)))

;; NOTE 2022-04-19: Ideally we should be able to use the
;; `notmuch-show-stash-mlarchive-link-alist' for
;; `prot-notmuch-stash-sourcehut-link', but it assumes that the base URL
;; is fixed for all message IDs, whereas those on SourceHut are not.

(declare-function notmuch-show-get-message-id "notmuch-show" (&optional bare))
(declare-function notmuch-show-message-top "notmuch-show")
(declare-function notmuch-common-do-stash "notmuch-lib" (text))

;;;###autoload
(defun prot-notmuch-stash-sourcehut-link (&optional current)
  "Stash web link to current SourceHut thread.
With optional CURRENT argument, produce a link to the current
message, else use the topmost message (start of the thread).

Note that the topmost message is assumed to hold the id of the
base URL, though this is not necessarily true."
  (interactive "P")
  (let* ((ml (prot-notmuch--extract-sourcehut-mail 2))
         (base-id (save-excursion (goto-char (point-min))
                                  (notmuch-show-message-top)
                                  (notmuch-show-get-message-id t)))
         (current-id (notmuch-show-get-message-id t)))
    (notmuch-common-do-stash
     (if current
         (format "https://lists.sr.ht/%s/<%s>#<%s>" ml base-id current-id)
       (format "https://lists.sr.ht/%s/<%s>" ml base-id)))))

;;;###autoload
(defun prot-notmuch-check-valid-sourcehut-email ()
  "Check if SourceHut address is correct.
Add this to `notmuch-mua-send-hook'."
  (when-let* ((ml (prot-notmuch--extract-sourcehut-mail 1))
              ((not (string-match-p "^\\(~\\|\\.\\)" ml)))
              ((not (y-or-n-p "SourceHut address looks wrong.  Send anyway?"))))
    (user-error "Incorrect SourceHut address")))

(provide 'prot-notmuch)
;;; prot-notmuch.el ends here
