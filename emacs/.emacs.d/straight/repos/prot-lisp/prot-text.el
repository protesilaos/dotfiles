;;; prot-text.el --- Extensions to text-mode.el for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2020  Protesilaos Stavrou

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

;;; Code:

(require 'text-mode)
(require 'prot-common)
(require 'prot-simple)

;; TODO: make this more robust
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

This command is meant to be used in `text-mode' buffers and
derivatives, such as `markdown-mode', though not in `org-mode'."
  (interactive "P")
  (cond
   ((eq major-mode 'org-mode)
    (user-error "Do not use `prot-common-text-mode-heading' in `org-mode'!"))
   ((derived-mode-p 'text-mode)
    (let* ((num (- (point-at-eol) (point-at-bol)))
           (char (string-to-char (if arg "=" "-"))))
      (cond
       ((and (eobp)
             (or (prot-common-text-list-line-p 1)
                 (prot-common-text-heading-line-p 1)
                 (prot-common-empty-line-p 1)
                 (prot-common-indent-line-p 1)))
        (newline 1))
       ((or (prot-common-empty-line-p 1)
            (prot-common-indent-line-p 1))
        (prot-simple-new-line-below))
       ((or (prot-common-text-list-line-p 1)
            (prot-common-text-heading-line-p 2))
        (if (prot-common-empty-line-p 3)
            (beginning-of-line 3)
          (prot-simple-new-line-below)))
       (t
        (prot-simple-new-line-below)
        (insert (make-string num char))
        (newline 2)))))))

;;;###autoload
(defun prot-text-cite-region (beg end &optional arg)
  "Cite text in region between BEG and END.
With optional prefix ARG (\\[universal-argument]) prompt for a
description that will be placed on a new line at the top of the
newly formatted text."
  (interactive "*r\nP")
  (let* ((text (buffer-substring-no-properties beg end))
         (text-new (replace-regexp-in-string "^.*?" "| " text))
         (description (if arg
                          (format "+----[ %s ]\n"
                                  (read-string "Add description: "))
                        "+----\n")))
    (delete-region beg end)
    (insert (concat description text-new "\n+----"))))

(provide 'prot-text)
;;; prot-text.el ends here
