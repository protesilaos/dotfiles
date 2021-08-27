;;; prot-org.el --- Tweaks for my org-mode configurations -*- lexical-binding: t -*-

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
;; This covers my tweaks for Org that are meant for use in my
;; Emacs setup: https://protesilaos.com/dotemacs.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

(require 'prot-common)

(defgroup prot-org ()
  "Extensions for org.el."
  :group 'org)

;;;; Source blocks

(defvar modus-themes-org-blocks)
(defvar org-fontify-whole-block-delimiter-line)

(defun prot-org-modus-themes-fontify-block-delimiters ()
  "Match `org-fontify-whole-block-delimiter-line' to theme style.
Run this function at the post theme load phase, such as with the
hook `modus-themes-after-load-theme-hook'."
  (if (eq modus-themes-org-blocks 'gray-background)
      (setq org-fontify-whole-block-delimiter-line t)
    (setq org-fontify-whole-block-delimiter-line nil))
  (when (derived-mode-p 'org-mode)
    (font-lock-flush)))

(when (require 'modus-themes nil t)
  (add-hook 'modus-themes-after-load-theme-hook
            #'prot-org-modus-themes-fontify-block-delimiters))

;;;; org-capture

(defvar prot-org-agenda-after-edit-hook nil
  "Hook that runs after select Org commands.
To be used with `advice-add'.")

(defun prot-org-agenda-after-edit (&rest _)
  "Run `prot-org-agenda-after-edit-hook'."
  (run-hooks 'prot-org-agenda-after-edit-hook))

(dolist (fn '(org-agenda-archive org-archive-subtree))
  (advice-add fn :after #'prot-org-agenda-after-edit))

(dolist (hook '(org-capture-after-finalize-hook
                  org-agenda-after-show-hook
                  prot-org-agenda-after-edit-hook))
  (add-hook hook #'org-agenda-to-appt))

;;;; org-agenda

(defun prot/org-agenda-format-date-aligned (date)
  "Format a DATE string for display in the daily/weekly agenda.
This function makes sure that dates are aligned for easy reading.

Slightly tweaked version of `org-agenda-format-date-aligned' that
produces dates with a fixed length."
  (require 'cal-iso)
  (let* ((dayname (calendar-day-name date t))
         (day (cadr date))
         (day-of-week (calendar-day-of-week date))
         (month (car date))
         (monthname (calendar-month-name month t))
         (year (nth 2 date))
         (iso-week (org-days-to-iso-week
                    (calendar-absolute-from-gregorian date)))
         (weekyear (cond ((and (= month 1) (>= iso-week 52))
                          (1- year))
                         ((and (= month 12) (<= iso-week 1))
                          (1+ year))
                         (t year)))
         (weekstring (if (= day-of-week 1)
                         (format " (W%02d)" iso-week)
                       "")))
    (format "%s %2d %s %4d%s"
            dayname day monthname year weekstring)))

(setq org-agenda-format-date #'prot/org-agenda-format-date-aligned)

;;;; org-export

(defun prot/ox-html ()
  (interactive)
  (org-html-export-as-html nil nil nil t nil))

(defun prot/ox-texinfo ()
  (interactive)
  (org-texinfo-export-to-info))

;;;; org-id

(declare-function org-id-add-location "org")

;; Copied from this article (with minor tweaks from my side):
;; <https://writequit.org/articles/emacs-org-mode-generate-ids.html>.
(defun contrib/org-id-get (&optional pom create prefix)
  "Get the CUSTOM_ID property of the entry at point-or-marker
POM. If POM is nil, refer to the entry at point. If the entry
does not have an CUSTOM_ID, the function returns nil. However,
when CREATE is non nil, create a CUSTOM_ID if none is present
already. PREFIX will be passed through to `org-id-new'. In any
case, the CUSTOM_ID of the entry is returned."
  (org-with-point-at pom
    (let ((id (org-entry-get nil "CUSTOM_ID")))
      (cond
       ((and id (stringp id) (string-match "\\S-" id))
        id)
       (create
        (setq id (org-id-new (concat prefix "h")))
        (org-entry-put pom "CUSTOM_ID" id)
        (org-id-add-location id (format "%s" (buffer-file-name (buffer-base-buffer))))
        id)))))

(defun contrib/org-id-headlines ()
  "Add CUSTOM_ID properties to all headlines in the current
file which do not already have one."
  (interactive)
  (org-map-entries
   (lambda () (contrib/org-id-get (point) t))))

(provide 'prot-org)
;;; prot-org.el ends here
