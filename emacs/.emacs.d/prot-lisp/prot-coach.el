;;; prot-coach.el --- Code for my personal coaching sessions -*- lexical-binding: t -*-

;; Copyright (C) 2023  Protesilaos Stavrou

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos.com/emacs/dotemacs
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
;; Code for my personal coaching sessions: <https://protesilaos.com/coach>.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

;;;; Jitsi link

(defvar prot-coach--name-prompt-history nil
  "Minibuffer history of `prot-coach--name-prompt'.")

(defun prot-coach--name-prompt ()
  "Prompt for student name."
  (read-string "Name of student: " nil 'prot-coach--name-prompt-history))

(defun prot-coach--return-identifier ()
  "Return identifier as YYYYmmddTHHMMSS.
This is the Denote identifier I use practically everywhere:
https://protesilaos.com/emacs/denote."
  (format-time-string "%Y%m%dT%H%M%S"))

;; I copied the "slug" functions from my denote.el.
(defconst prot-coach-excluded-punctuation-regexp "[][{}!@#$%^&*()=+'\"?,.\|;:~`‘’“”/]*"
  "Punctionation that is removed from file names.
We consider those characters illegal for our purposes.")

(defvar prot-coach-excluded-punctuation-extra-regexp nil
  "Additional punctuation that is removed from file names.
This variable is for advanced users who need to extend the
`prot-coach-excluded-punctuation-regexp'.  Once we have a better
understanding of what we should be omitting, we will update
things accordingly.")

(defun prot-coach--slug-no-punct (str)
  "Convert STR to a file name slug."
  (replace-regexp-in-string
   (concat prot-coach-excluded-punctuation-regexp
           prot-coach-excluded-punctuation-extra-regexp)
   "" str))

(defun prot-coach--slug-hyphenate (str)
  "Replace spaces and underscores with hyphens in STR.
Also replace multiple hyphens with a single one and remove any
leading and trailing hyphen."
  (replace-regexp-in-string
   "^-\\|-$" ""
   (replace-regexp-in-string
    "-\\{2,\\}" "-"
    (replace-regexp-in-string "_\\|\s+" "-" str))))

(defun prot-coach-sluggify (str)
  "Make STR an appropriate slug for file names and related."
  (downcase (prot-coach--slug-hyphenate (prot-coach--slug-no-punct str))))

(defun prot-coach--format-jitsi (name)
  "Format a Jitsi link with a unique identifier that includes NAME."
  (format "https://meet.jit.si/%s--%s"
          (prot-coach--return-identifier)
          (prot-coach-sluggify name)))

(defun prot-coach-jitsi-link (name)
  "Produce Jitsi link for NAME person."
  (interactive (list (prot-coach--name-prompt)))
  (insert (prot-coach--format-jitsi name)))

;;;; Time tables

(require 'org)

;; FIXME 2023-03-29: Can this work with logbooks for repeatable
;; entries?
(defun prot-coach--get-deadline-and-close (&optional name)
  "Get time stamps of deadline and closed for optional NAME.
Omit entries with a CANCEL state."
  (org-with-point-at (point)
    (when-let* ((case-fold-search t)
                (heading (org-no-properties
                          (org-get-heading :no-tags :no-todo :no-priority :no-comment)))
                ((string-match-p (or name ".*") heading))
                (deadline (org-entry-get nil "DEADLINE"))
                (closed (org-entry-get nil "CLOSED"))
                ((not (string= (org-entry-get nil "TODO") "CANCEL"))))
      (list heading (format "%s" deadline) (format "%s" closed)))))

(defun prot-coach--print-table-with-sessions (name sessions)
  "Return time table for NAME given SESSIONS."
  (let ((buf (get-buffer-create (format "*prot-coach with %s" name))))
    (with-current-buffer buf
      (org-mode)
      (erase-buffer)
      (goto-char (point-min))
      (insert "# Type C-c C-c in the TBLFM line to produce the table\n\n")
      (insert "* Cost counting without counting minutes\n")
      (insert (concat
               "\n"
               "| Description | Started | Closed | Days | Hours | Cost (EUR) |" "\n"
               "|-------------+---------+--------+------+-------+------------|" "\n"))
      (mapc
       (lambda (session)
         (insert (format "| %s | %s | %s |\n"
                         (nth 0 session)
                         (nth 1 session)
                         (nth 2 session))))
       sessions)
      (insert (concat "|-+" "\n"
                      "| " "\n"
                      "#+TBLFM: $4=date($3) - date($2) :: $5=86400 * $4;U"
                      ":: @>$5=vsum(@<<..@>>);U :: $6=$5*20;N :: @>$>=vsum(@<<..@>>)"
                      ":: @>$1='(length (org-lookup-all \".*\" '(@<<$1..@>>$1) nil 'string-match-p))"
                      "\n"))
      (org-table-recalculate-buffer-tables))
    (pop-to-buffer buf)))

;;;###autoload
(defun prot-coach-done-sessions-with-person ()
  "Produce buffer with time table for a given student."
  (declare (interactive-only t))
  (interactive)
  (when-let* ((file (buffer-file-name))
              (dir (file-name-directory file))
              ((string-match-p "coach" file))
              ((or (member file org-agenda-files)
                   ;; NOTE 2023-03-29: This assumes the file paths are
                   ;; absolute and end with a directory delimiter.
                   (member dir org-agenda-files)))
              (name (prot-coach--name-prompt)))
    (let (sessions)
      (org-map-entries
       (lambda ()
         (when-let ((entry (prot-coach--get-deadline-and-close name)))
           (push entry sessions))))
      (prot-coach--print-table-with-sessions name sessions))))

(provide 'prot-coach)
;;; prot-coach.el ends here
