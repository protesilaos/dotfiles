;;; prot-text.el --- Extensions to text-mode.el for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2020-2021  Protesilaos Stavrou

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
;; This covers my text-mode.el extensions, for use in my Emacs setup:
;; https://protesilaos.com/dotemacs.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

(require 'prot-common)
(require 'prot-simple)

;;;###autoload
(defun prot-text-insert-heading (&optional arg)
  "Insert equal length heading delimiter below current line.

A heading delimiter is drawn as a series of dashes (-).  With
optional ARG, i.e. by prefixing \\[universal-argument], draw the
heading delimiter with equals signs (=).  The latter is
considered a heading level 1, while the former is level 2.

A heading delimiter is inserted only when that would not mess up
with existing headings or lists.  In such cases, point will move
to the next line.  For the purposes of this command, text that
starts with a number and no further delimiter is not consider a
list element.

This command is meant to be used in Text mode buffers and
compatible derivatives, such as Markdown mode, though not Org
mode which has its own conventions."
  (interactive "P")
  (cond
   ((derived-mode-p 'outline-mode)
    (user-error "Do not use `prot-text-insert-heading' in `outline-mode' or derivatives!"))
   ((derived-mode-p 'text-mode)
    (let* ((num (- (point-at-eol) (point-at-bol)))
           (char (string-to-char (if arg "=" "-"))))
      (cond
       ((and (eobp)
             (or (prot-common-line-regexp-p 'list 1)
                 (prot-common-line-regexp-p 'heading 1)
                 (prot-common-line-regexp-p 'empty 1)
                 (prot-common-line-regexp-p 'indent 1)))
        (newline 1))
       ((or (prot-common-line-regexp-p 'empty 1)
            (prot-common-line-regexp-p 'indent 1))
        (prot-simple-new-line-below))
       ((or (prot-common-line-regexp-p 'list 1)
            (prot-common-line-regexp-p 'heading 2))
        (if (prot-common-line-regexp-p 'empty 3)
            (beginning-of-line 3)
          (prot-simple-new-line-below)))
       ((or (prot-common-line-regexp-p 'empty 2)
            (prot-common-line-regexp-p 'indent 2))
        (prot-simple-new-line-below)
        (insert (make-string num char))
        (newline 1)
        (beginning-of-line 2))
       (t
        (prot-simple-new-line-below)
        (insert (make-string num char))
        (newline 2)))))))

(provide 'prot-text)
;;; prot-text.el ends here
