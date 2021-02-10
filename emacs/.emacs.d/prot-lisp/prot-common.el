;;; prot-common.el --- Common functions for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2020-2021  Protesilaos Stavrou

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
;; Common functions for my Emacs: <https://protesilaos.com/dotemacs/>.

;;; Code:

(defgroup prot-common ()
  "Auxiliary functions for my dotemacs."
  :group 'editing)

;;;###autoload
(defun prot-common-number-even-p (n)
  "Test if N is an even number."
  (if (numberp n)
      (= (% n 2) 0)
    (error "%s is not a number" n)))

;;;###autoload
(defun prot-common-number-integer-p (n)
  "Test if N is an integer."
  (if (integerp n)
      n
    (error "%s is not an integer" n)))

;;;###autoload
(defun prot-common-number-negative (n)
  "Make N negative."
  (if (numberp n)
      (string-to-number (format "-%d" n)) ; TODO: better way?
    (error "%s is not a number" n)))

;;;###autoload
(defun prot-common-minor-modes-active ()
  "Return list of active minor modes for the current buffer."
  (let ((active-modes))
    (mapc (lambda (m)
            (when (and (boundp m) (symbol-value m))
              (push m active-modes)))
          minor-mode-list)
    active-modes))

;; Thanks to Omar Antolín Camarena for providing this snippet!
;;;###autoload
(defun prot-common-completion-table (category candidates)
  "Pass appropriate metadata CATEGORY to completion CANDIDATES.

This is intended for bespoke functions that need to pass
completion metadata that can then be parsed by other
tools (e.g. `embark')."
  (lambda (string pred action)
    (if (eq action 'metadata)
        `(metadata (category . ,category))
      (complete-with-action action candidates string pred))))

;; Thanks to Igor Lima for the `prot-common-crm-exclude-selected-p':
;; <https://github.com/0x462e41>.
;; This is used as a filter predicate in the relevant prompts.
(defvar crm-separator)

;;;###autoload
(defun prot-common-crm-exclude-selected-p (input)
  "Filter out INPUT from `completing-read-multiple'.
Hide non-destructively the selected entries from the completion
table, thus avoiding the risk of inputting the same match twice.

To be used as the PREDICATE of `completing-read-multiple'."
  (if-let* ((pos (string-match-p crm-separator input))
            (rev-input (reverse input))
            (element (reverse
                      (substring rev-input 0
                                 (string-match-p crm-separator rev-input))))
            (flag t))
      (progn
        (while pos
          (if (string= (substring input 0 pos) element)
              (setq pos nil)
            (setq input (substring input (1+ pos))
                  pos (string-match-p crm-separator input)
                  flag (when pos t))))
        (not flag))
    t))

(declare-function auth-source-search "auth-source")

;;;###autoload
(defun prot-common-auth-get-field (host prop)
  "Find PROP in `auth-sources' for HOST entry."
  (let* ((source (auth-source-search :host host))
         (field (plist-get
                 (flatten-list source)
                 prop)))
    (if source
        field
      (user-error "No entry in auth sources"))))

;; Based on `org--line-empty-p'.
(defmacro prot-common--line-p (name regexp)
  "Make NAME function to match REGEXP on line n from point."
  `(defun ,name (n)
     (save-excursion
       (goto-char (point-at-bol))
       (and (not (bobp))
	        (or (beginning-of-line n) t)
	        (save-match-data
	          (looking-at ,regexp))))))

(prot-common--line-p
 prot-common-empty-line-p
 "[\s\t]*$")

(prot-common--line-p
 prot-common-indent-line-p
 "^[\s\t]+")

(prot-common--line-p
 prot-common-non-empty-line-p
 "^.+$")

(prot-common--line-p
 prot-common-text-list-line-p
 "^\\([\s\t#*+]+\\|[0-9]+[).]+\\)")

(prot-common--line-p
 prot-common-text-heading-line-p
 "^[=-]+")

(provide 'prot-common)
;;; prot-common.el ends here
