;;; prot-elfeed.el --- Elfeed extensions for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2021-2023  Protesilaos Stavrou

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos.com/emacs/dotemacs
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

;; NOTE 2022-06-08: This is old code.  There are things I would like to
;; improve.

;;
;; Extensions for Elfeed, intended for use in my Emacs setup:
;; https://protesilaos.com/emacs/dotemacs.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

(eval-when-compile (require 'subr-x))
(require 'elfeed nil t)
(require 'url-util)
(require 'prot-common)

(defgroup prot-elfeed ()
  "Personal extensions for Elfeed."
  :group 'elfeed)

(defcustom prot-elfeed-feeds-file
  (thread-last user-emacs-directory (expand-file-name "feeds.el.gpg"))
  "Path to file with `elfeed-feeds'."
  :type 'string
  :group 'prot-elfeed)

(defcustom prot-elfeed-archives-directory "~/Documents/feeds/"
  "Path to directory for storing Elfeed entries."
  :type 'string
  :group 'prot-elfeed)

(defcustom prot-elfeed-tag-faces nil
  "Add faces for certain tags.
The tags are: critical, important, personal."
  :type 'boolean
  :group 'prot-elfeed)

(defcustom prot-elfeed-search-tags '(critical important personal)
  "List of user-defined tags.
Used by `prot-elfeed-toggle-tag'."
  :type 'list
  :group 'prot-elfeed)

(defface prot-elfeed-entry-critical '((t :inherit font-lock-warning-face))
  "Face for Elfeed entries tagged with `critical'.")

(defface prot-elfeed-entry-important '((t :inherit font-lock-constant-face))
  "Face for Elfeed entries tagged with `important'.")

(defface prot-elfeed-entry-personal '((t :inherit font-lock-variable-name-face))
  "Face for Elfeed entries tagged with `personal'.")

;;;; Utilities

;;;###autoload
(defun prot-elfeed-load-feeds ()
  "Load file containing the `elfeed-feeds' list.
Add this to `elfeed-search-mode-hook'."
  (let ((feeds prot-elfeed-feeds-file))
    (if (file-exists-p feeds)
        (load-file feeds)
      (user-error "Missing feeds' file"))))

(defvar elfeed-search-face-alist)

;;;###autoload
(defun prot-elfeed-fontify-tags ()
  "Expand Elfeed faces if `prot-elfeed-tag-faces' is non-nil."
  (if prot-elfeed-tag-faces
      (setq elfeed-search-face-alist
            '((critical prot-elfeed-entry-critical)
              (important prot-elfeed-entry-important)
              (personal prot-elfeed-entry-personal)
              (unread elfeed-search-unread-title-face)))
    (setq elfeed-search-face-alist
          '((unread elfeed-search-unread-title-face)))))

(defvar prot-elfeed--tag-hist '()
  "History of inputs for `prot-elfeed-toggle-tag'.")

(defun prot-elfeed--character-prompt (tags)
  "Helper of `prot-elfeed-toggle-tag' to read TAGS."
  (let ((def (car prot-elfeed--tag-hist)))
    (completing-read
     (format "Toggle tag [%s]: " def)
     tags nil t nil 'prot-elfeed--tag-hist def)))

(defvar elfeed-show-entry)
(declare-function elfeed-tagged-p "elfeed")
(declare-function elfeed-search-toggle-all "elfeed")
(declare-function elfeed-show-tag "elfeed")
(declare-function elfeed-show-untag "elfeed")

;;;###autoload
(defun prot-elfeed-toggle-tag (tag)
  "Toggle TAG for the current item.

When the region is active in the `elfeed-search-mode' buffer, all
entries encompassed by it are affected.  Otherwise the item at
point is the target.  For `elfeed-show-mode', the current entry
is always the target.

The list of tags is provided by `prot-elfeed-search-tags'."
  (interactive
   (list
    (intern
     (prot-elfeed--character-prompt prot-elfeed-search-tags))))
  (if (derived-mode-p 'elfeed-show-mode)
      (if (elfeed-tagged-p tag elfeed-show-entry)
          (elfeed-show-untag tag)
        (elfeed-show-tag tag))
    (elfeed-search-toggle-all tag)))

(defvar elfeed-show-truncate-long-urls)
(declare-function elfeed-entry-title "elfeed")
(declare-function elfeed-show-refresh "elfeed")

;;;; General commands

(defvar elfeed-search-filter-active)
(defvar elfeed-search-filter)
(declare-function elfeed-db-get-all-tags "elfeed")
(declare-function elfeed-search-update "elfeed")
(declare-function elfeed-search-clear-filter "elfeed")

(defun prot-elfeed--format-tags (tags sign)
  "Prefix SIGN to each tag in TAGS."
  (mapcar (lambda (tag)
            (format "%s%s" sign tag))
          tags))

;;;###autoload
(defun prot-elfeed-search-tag-filter ()
  "Filter Elfeed search buffer by tags using completion.

Completion accepts multiple inputs, delimited by `crm-separator'.
Arbitrary input is also possible, but you may have to exit the
minibuffer with something like `exit-minibuffer'."
  (interactive)
  (unwind-protect
      (elfeed-search-clear-filter)
    (let* ((elfeed-search-filter-active :live)
           (db-tags (elfeed-db-get-all-tags))
           (plus-tags (prot-elfeed--format-tags db-tags "+"))
           (minus-tags (prot-elfeed--format-tags db-tags "-"))
           (all-tags (delete-dups (append plus-tags minus-tags)))
           (tags (completing-read-multiple
                  "Apply one or more tags: "
                  all-tags #'prot-common-crm-exclude-selected-p t))
           (input (string-join `(,elfeed-search-filter ,@tags) " ")))
      (setq elfeed-search-filter input))
    (elfeed-search-update :force)))

(provide 'prot-elfeed)
;;; prot-elfeed.el ends here
