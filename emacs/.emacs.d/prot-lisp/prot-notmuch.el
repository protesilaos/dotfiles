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

(defface prot-notmuch-notmuch-spam-tag
  '((default :inherit (bold italic))
    (((class color) (min-colors 88) (background light))
     :foreground "#70480f")
    (((class color) (min-colors 88) (background dark))
     :foreground "#c4d030")
    (t :foreground "yellow"))
  "Face for the 'spam' tag or related in Notmuch.
Refer to the variable `notmuch-tag-formats' for how to assign
those.")

(defface prot-notmuch-notmuch-todo-tag
  '((default :inherit (bold italic))
    (((class color) (min-colors 88) (background light))
     :foreground "#145c33")
    (((class color) (min-colors 88) (background dark))
     :foreground "#00c06f")
    (t :foreground "green"))
  "Face for the 'todo' tag or related in Notmuch.
Refer to the variable `notmuch-tag-formats' for how to assign
those.")

(declare-function message-fetch-field "message")
(declare-function message-remove-header "message")
(declare-function message-add-header "message")
(declare-function message-sort-headers "message")
(declare-function notmuch-fcc-header-setup "notmuch")

;;;###autoload
(defun prot-notmuch-message-headers ()
  "While `notmuch' is running, configure From header.
Add this function to `message-header-setup-hook'."
  (when (and (eq mail-user-agent 'notmuch-user-agent)
             (eq last-command 'compose-mail))
    (when (message-fetch-field "From")
      (message-remove-header "From"))
    (message-add-header (format "From: %s <%s>" user-full-name user-mail-address))
    (notmuch-fcc-header-setup)
    (message-sort-headers)))

(provide 'prot-notmuch)
;;; prot-notmuch.el ends here
