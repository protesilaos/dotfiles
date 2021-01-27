;;; prot-comment.el --- Extensions newcomment.el for my dotemacs -*- lexical-binding: t -*-

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
;; This covers my newcomment.el extras, for use in my Emacs setup:
;; https://protesilaos.com/dotemacs.

;;; Code:

(defgroup prot-comment ()
  "Extensions for newcomment.el."
  :group 'comment)

(defcustom prot-comment-comment-keywords
  '("TODO" "NOTE" "XXX" "REVIEW" "FIXME")
  "List of strings with comment keywords."
  :type 'list
  :group 'prot-comment)

;;;###autoload
(defun prot-comment-comment-dwim (arg)
  "Wrapper around `comment-line' and `comment-dwim'.

If the region is active, then toggle the comment status of the
region or, if the major mode defines as much, of all the lines
implied by the region boundaries.

Else toggle the comment status of the line at point.

ARG is a numerix prefix that controls how many lines to comment
on.  It defaults to 1 when no prefix is supplied."
  (interactive "p")
  (if (use-region-p)
      (comment-dwim (or arg 1))
    (comment-line (or arg 1))))

(defvar prot-comment--keyword-hist '()
  "Input history of selected comment keywords.")

(defun prot-comment--keyword-prompt (keywords)
  "Prompt for candidate among KEYWORDS."
  (let ((def (car prot-comment--keyword-hist)))
    (completing-read
     (format-prompt "Select keyword" def)
     keywords nil nil nil 'prot-comment--keyword-hist def)))

;;;###autoload
(defun prot-comment-timestamp-keyword (keyword)
  "Add timestamped comment with KEYWORD.
When called interactively, the list of possible keywords is that
of `prot-comment-comment-keywords', though it is possible to
input arbitrary text.

If point is at the beginning of the line, the comment is started
there.  Any existing text after the point will not be turned into
a comment but will be pushed to a new line instead.

If point is anywhere else on the line, the comment is indented
with `comment-indent'.

The comment is formatted as 'DELIMITER KEYWORD DATE:', with the
date being represented as Year-Month-Day."
  (interactive
   (list
    (prot-comment--keyword-prompt prot-comment-comment-keywords)))
  (let ((beg (point))
        (end (gensym))
        (string (format "%s %s: " keyword (format-time-string "%F"))))
    (cond
     ((eq beg (point-at-bol))
      (insert string)
      (setq end (point))
      (comment-region beg end))
     (t
      (comment-indent t)
      (insert (concat " " string))))))

(provide 'prot-comment)
;;; prot-comment.el ends here
