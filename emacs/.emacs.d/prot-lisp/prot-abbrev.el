;;; prot-abbrev.el --- Functions for use with abbrev-mode -*- lexical-binding: t -*-

;; Copyright (C) 2025-2025  Protesilaos Stavrou

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos.com/emacs/dotemacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))

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
;; Functions for use with `abbrev-mode'.

;;; Code:

(defgroup prot-abbrev ()
  "Functions for use with `abbrev-mode'."
  :group 'editing)

(defcustom prot-abbrev-time-specifier "%R"
  "Time specifier for `format-time-string'."
  :type 'string
  :group 'prot-abbrev)

(defcustom prot-abbrev-date-specifier "%F"
  "Date specifier for `format-time-string'."
  :type 'string
  :group 'prot-abbrev)

(defun prot-abbrev-current-time ()
  "Insert the current time per `prot-abbrev-time-specifier'."
  (insert (format-time-string prot-abbrev-time-specifier)))

(defun prot-abbrev-current-date ()
  "Insert the current date per `prot-abbrev-date-specifier'."
  (insert (format-time-string prot-abbrev-date-specifier)))

(defun prot-abbrev-jitsi-link ()
  "Insert a Jitsi link."
  (insert (concat "https://meet.jit.si/" (format-time-string "%Y%m%dT%H%M%S"))))

(defvar prot-abbrev-update-html-history nil
  "Minibuffer history for `prot-abbrev-update-html-prompt'.")

(defun prot-abbrev-update-html-prompt ()
  "Minibuffer prompt for `prot-abbrev-update-html'.
Use completion among previous entries, retrieving their data from
`prot-abbrev-update-html-history'."
  (completing-read
   "Insert update for manual: "
   prot-abbrev-update-html-history
   nil nil nil 'prot-abbrev-update-html-history))

(defun prot-abbrev-update-html ()
  "Insert message to update NAME.html page, by prompting for NAME."
  (insert (format "Update %s.html" (prot-abbrev-update-html-prompt))))

(defvar prot-abbrev-org-macro-key-history nil
  "Minibuffer history for `prot-abbrev-org-macro-key-prompt'.")

(defun prot-abbrev-org-macro-key-prompt ()
  "Minibuffer prompt for `prot-abbrev-org-macro-key'.
Use completion among previous entries, retrieving their data from
`prot-abbrev-org-macro-key-history'."
  (completing-read
   "Key binding: "
   prot-abbrev-org-macro-key-history
   nil nil nil 'prot-abbrev-org-macro-key-history))

(defvar prot-abbrev-org-macro-key-symbol-history nil
  "Minibuffer history for `prot-abbrev-org-macro-key-symbol-prompt'.")

(defun prot-abbrev-org-macro-key-symbol-prompt ()
  "Minibuffer prompt for `prot-abbrev-org-macro-key'.
Use completion among previous entries, retrieving their data from
`prot-abbrev-org-macro-key-symbol-history'."
  (completing-read
   "Command name: "
   prot-abbrev-org-macro-key-symbol-history
   nil nil nil 'prot-abbrev-org-macro-key-symbol-history))

(defun prot-abbrev-org-macro-key-command ()
  "Insert {{{kbd(KEY)}}} (~SYMBOL~) by prompting for KEY and SYMBOL."
  (insert (format "{{{kbd(%s)}}} (~%s~)"
                  (prot-abbrev-org-macro-key-prompt)
                  (prot-abbrev-org-macro-key-symbol-prompt))))

(defun prot-abbrev-org-macro-key ()
  "Insert {{{kbd(KEY)}}} by prompting for KEY."
  (insert (format "{{{kbd(%s)}}}" (prot-abbrev-org-macro-key-prompt))))

(provide 'prot-abbrev)
;;; prot-abbrev.el ends here
