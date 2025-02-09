;;; prot-comment.el --- Extensions newcomment.el for my dotemacs -*- lexical-binding: t -*-

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
;; This covers my newcomment.el extras, for use in my Emacs setup:
;; https://protesilaos.com/emacs/dotemacs.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

(require 'newcomment)
(require 'prot-common)

(defgroup prot-comment ()
  "Extensions for newcomment.el."
  :group 'comment)

(defcustom prot-comment-keywords
  '("TODO" "NOTE" "XXX" "REVIEW" "FIXME")
  "List of strings with keywords used by `prot-comment-timestamp-keyword'."
  :type '(repeat string)
  :group 'prot-comment)

(defcustom prot-comment-timestamp-format-concise "%F"
  "Specifier for date in `prot-comment-timestamp-keyword'.
Refer to the doc string of `format-time-string' for the available
options."
  :type 'string
  :group 'prot-comment)

(defcustom prot-comment-timestamp-format-verbose "%F %T %z"
  "Like `prot-comment-timestamp-format-concise', but longer."
  :type 'string
  :group 'prot-comment)

;;;###autoload
(defun prot-comment (n)
  "Comment N lines, defaulting to the current one.
When the region is active, comment its lines instead."
  (interactive "p")
  (if (use-region-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-line n)))

(make-obsolete 'prot-comment-comment-dwim 'prot-comment "2023-09-28")

(defvar prot-comment--keyword-hist '()
  "Minibuffer history of `prot-comment--keyword-prompt'.")

(defun prot-comment--keyword-prompt (keywords)
  "Prompt for candidate among KEYWORDS (per `prot-comment-timestamp-keyword')."
  (let ((def (car prot-comment--keyword-hist)))
    (completing-read
     (format "Select keyword [%s]: " def)
     keywords nil nil nil 'prot-comment--keyword-hist def)))

(defun prot-comment--format-date (verbose)
  "Format date using `format-time-string'.
VERBOSE has the same meaning as `prot-comment-timestamp-keyword'."
  (format-time-string
   (if verbose
       prot-comment-timestamp-format-verbose
     prot-comment-timestamp-format-concise)))

(defun prot-comment--timestamp (keyword &optional verbose)
  "Format string using current time and KEYWORD.
VERBOSE has the same meaning as `prot-comment-timestamp-keyword'."
  (format "%s %s: " keyword (prot-comment--format-date verbose)))

(defun prot-comment--format-comment (string)
  "Format comment STRING per `prot-comment-timestamp-keyword'.
STRING is a combination of a keyword and a time stamp."
  (concat comment-start
          (make-string comment-add (string-to-char comment-start))
          comment-padding
          string
          comment-end))

(defun prot-comment--maybe-newline ()
  "Call `newline' if current line is not empty.
Check `prot-comment-timestamp-keyword' for the rationale."
  (unless (prot-common-line-regexp-p 'empty 1)
    (save-excursion (newline))))

;;;###autoload
(defun prot-comment-timestamp-keyword (keyword &optional verbose)
  "Add timestamped comment with KEYWORD.

When called interactively, the list of possible keywords is that
of `prot-comment-keywords', though it is possible to input
arbitrary text.

If point is at the beginning of the line or if line is empty (no
characters at all or just indentation), the comment is started
there in accordance with `comment-style'.  Any existing text
after the point will be pushed to a new line and will not be
turned into a comment.

If point is anywhere else on the line and the line is not empty,
the comment is appended to the line with `comment-indent'.

The comment is always formatted as DELIMITER KEYWORD DATE:, with
the date format being controlled by the variable
`prot-comment-timestamp-format-concise'.  DELIMITER is the value
of `comment-start', as defined by the current major mode.

With optional VERBOSE argument (such as a prefix argument), use
an alternative date format, as specified by
`prot-comment-timestamp-format-verbose'."
  (interactive
   (list
    (prot-comment--keyword-prompt prot-comment-keywords)
    current-prefix-arg))
  (let ((string (prot-comment--timestamp keyword verbose))
        (beg (point)))
    (cond
     ((prot-common-line-regexp-p 'empty)
      (insert (prot-comment--format-comment string)))
     ((eq beg (line-beginning-position))
      (insert (prot-comment--format-comment string))
      (indent-region beg (point))
      (prot-comment--maybe-newline))
     (t
      (comment-indent t)
      (insert (concat " " string))))))

(provide 'prot-comment)
;;; prot-comment.el ends here
