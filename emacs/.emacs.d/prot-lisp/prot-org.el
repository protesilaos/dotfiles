;;; prot-org.el --- Tweaks for my org-mode configurations -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024  Protesilaos Stavrou

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
;; This covers my tweaks for Org that are meant for use in my
;; Emacs setup: https://protesilaos.com/emacs/dotemacs.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

(require 'prot-common)
(require 'org)

(defgroup prot-org ()
  "Extensions for org.el."
  :group 'org)

;;;; org-capture

(defvar prot-org--capture-coach-person-history nil)

(declare-function message-fetch-field "message" (header &optional first))
(declare-function notmuch-show-get-header "notmuch-show")

(defun prot-org--capture-coach-person-message-from ()
  "Return default value for `prot-org--capture-coach-person-prompt'."
  (when-let* ((from (cond
                     ((derived-mode-p 'message-mode)
                      (message-fetch-field "To"))
                     ((derived-mode-p 'notmuch-show-mode)
                      (notmuch-show-get-header :From)))))
    (string-clean-whitespace (car (split-string from "<")))))

(defun prot-org--capture-coach-person-message-from-and-subject ()
  "Return default value for `prot-org--capture-coach-person-prompt'."
  (cond
   ((derived-mode-p 'message-mode)
    (message-fetch-field "Subject"))
   ((derived-mode-p 'notmuch-show-mode)
    (notmuch-show-get-header :Subject))))

(defun prot-org--capture-coach-person-prompt ()
  "Prompt for person for use in `prot-org-capture-coach'."
  (completing-read "Person to coach: "
                   prot-org--capture-coach-person-history
                   nil nil nil
                   'prot-org--capture-coach-person-history
                   (prot-org--capture-coach-person-message-from)))

(defvar prot-org--capture-coach-description-history nil)

(defun prot-org--capture-coach-description-prompt ()
  "Prompt for description in `prot-org-capture-coach'."
  (read-string "Description: "
               nil
               'prot-org--capture-coach-description-history
               (prot-org--capture-coach-person-message-from-and-subject)))

(defun prot-org--capture-coach-date-prompt-range ()
  "Prompt for Org date and return it as a +1h range.
For use in `prot-org-capture-coach'."
  (let ((date (org-read-date :with-time)))
    ;; We cannot use this here, unfortunately, as the Org agenda
    ;; interprets it both as a deadline and an event with the date
    ;; range.
    ;;
    ;; (format "DEADLINE: <%s>--<%s>\n" date
    (format "<%s>--<%s>\n" date
            (org-read-date
             :with-time nil "++1h" nil
             (org-encode-time (org-parse-time-string date))))))

(defun prot-org-capture-coach ()
  "Contents of an Org capture template for my coaching lessons."
  (let ((identifier (format-time-string "%Y%m%dT%H%M%S")))
    (format "* COACH %s %s :lesson:
DEADLINE: %%^T
:PROPERTIES:
:CAPTURED: %%U
:CUSTOM_ID: h:%s
:APPT_WARNTIME: 20
:END:

%%a%%?"
            (prot-org--capture-coach-person-prompt)
            (prot-org--capture-coach-description-prompt)
            identifier
            identifier)))

(defun prot-org-capture-coach-clock ()
  "Contents of an Org capture for my clocked coaching services."
  (format "* COACH %s %s :service:
:PROPERTIES:
:CAPTURED: %%U
:CUSTOM_ID: h:%s
:END:

%%a%%?"
          (prot-org--capture-coach-person-prompt)
          (prot-org--capture-coach-description-prompt)
          (format-time-string "%Y%m%dT%H%M%S")))

(declare-function cl-letf "cl-lib")

;; Adapted from source: <https://stackoverflow.com/a/54251825>.
;;
;; Thanks to Juanjo Presa (@uningan on GitHub) for discovering that the
;; original version was causing an error in `org-roam'.  I then figure
;; we were missing the `&rest':
;; <https://github.com/org-roam/org-roam/issues/2142#issuecomment-1100718373>.
(defun prot-org--capture-no-delete-windows (&rest args)
  "Apply ARGS while ignoring `delete-other-windows'."
  (cl-letf (((symbol-function 'delete-other-windows) 'ignore))
    (apply args)))

;; Same source as above
(advice-add 'org-capture-place-template :around 'prot-org--capture-no-delete-windows)
(advice-add 'org-add-log-note :around 'prot-org--capture-no-delete-windows)

;;;;; Custom function to select a project to add to

(defun prot-org--get-outline (&optional file)
  "Return `outline-regexp' headings and line numbers of current file or FILE."
  (with-current-buffer (find-file-noselect file)
    (let ((outline-regexp (format "^\\(?:%s\\)" (or (bound-and-true-p outline-regexp) "[*\^L]+")))
          candidates)
      (save-excursion
        (goto-char (point-min))
        (while (if (bound-and-true-p outline-search-function)
                   (funcall outline-search-function)
                 (re-search-forward outline-regexp nil t))
          (push
           ;; NOTE 2024-11-24: The -5 (minimum width) is a sufficiently high number to keep the
           ;; alignment consistent in most cases.  Larger files will simply shift the heading text
           ;; in minibuffer, but this is not an issue anymore.
           (format "%-5s\t%s"
                   (line-number-at-pos (point))
                   (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
           candidates)
          (goto-char (1+ (line-end-position)))))
      (if candidates
          (nreverse candidates)
        (user-error "No outline")))))

(defvar prot-org-outline-history nil
  "Minibuffer history for `prot-org-outline-prompt'.")

(defun prot-org-outline-prompt (&optional file)
  "Prompt for outline among headings retrieved by `prot-org--get-outline'.
With optional FILE use the outline of it, otherwise use that of
the current file."
  (let ((current-file (or file buffer-file-name))
        (default (car prot-org-outline-history)))
    (completing-read
     (format-prompt
      (format "Select heading inside `%s': "
              (propertize (file-name-nondirectory current-file) 'face 'error))
      default)
     (prot-common-completion-table-no-sort 'imenu (prot-org--get-outline current-file))
     nil :require-match nil 'prot-org-outline-history default)))

(defvar prot-org-file-history nil
  "Minibuffer history of `prot-org-file-prompt'.")

(defun prot-org--not-useful-p (file)
  "Return non-nil if FILE is not a useful Org file for `org-capture'."
  (or (string-match-p "\\.org_archive\\'" file)
      (backup-file-name-p file)
      (not (string-match-p "\\.org\\'" file))))

(defun prot-org-file-prompt ()
  "Select a file in the `org-directory'."
  (if-let* ((dir org-directory)
            (files (directory-files-recursively org-directory ".*" nil))
            (files (seq-remove #'prot-org--not-useful-p files)))
      (let ((default (car prot-org-file-history)))
        (completing-read
         (format-prompt "Select file" default)
         (prot-common-completion-table 'file files)
         nil :require-match nil 'prot-org-file-history default))
    (user-error "There are no files in the `org-directory'")))

;;;###autoload
(defun prot-org-select-heading-in-file ()
  "Like `prot-org-select-project' but specifically for `org-capture'."
  (declare (interactive-only t))
  (interactive)
  (pcase-let* ((file (prot-org-file-prompt))
               (line-with-heading (prot-org-outline-prompt file))
               (`(,line ,text) (split-string line-with-heading "\t"))
               (line (string-to-number line)))
    ;; NOTE 2024-11-24: `with-current-buffer' does not work with `org-capture'.
    (find-file file)
    (goto-char (point-min))
    (forward-line (1- line))))

(defalias 'prot-org-goto-heading-in-file 'prot-org-select-heading-in-file
 "Alias for `prot-org-select-heading-in-file'.")

;;;; Org clock report

(defvar prot-org-clock--template-with-effort
  "#+BEGIN: clocktable :formula % :properties (\"Effort\") :timestamp t :sort (1 . ?a) :link t :scope nil :hidefiles t :maxlevel 8 :stepskip0 t
#+END:"
  "Clock table with effort estimate column to use for custom clock reports.")

(defvar prot-org-clock--template-no-effort
  "#+BEGIN: clocktable :formula % :timestamp t :sort (1 . ?a) :link nil :scope nil :hidefiles t :maxlevel 8 :stepskip0 t
#+END:"
  "Clock table to use for custom clock reports.")

(defvar prot-org-clock--ranges
  '( today yesterday thisweek lastweek thismonth
     lastmonth thisyear lastyear untilnow)
  "Time ranges of my interest for clock reports.")

(defvar prot-org-clock--report-range-history nil
  "Minibuffer history for `prot-org-clock--report-range-prompt'.")

(defun prot-org-clock--report-range-prompt ()
  "Prompt for a clock table range among `prot-org-clock--ranges'."
  (let ((default (car prot-org-clock--report-range-history)))
    (completing-read
     (format-prompt "Select a time range for the clock" default)
     prot-org-clock--ranges nil :require-match nil 'prot-org-clock--report-range-history
     default)))

(defun prot-org-clock--get-report (scope)
  "Produce clock report with current file SCOPE and return its buffer.
SCOPE is a symbol of either `file' or `subtree'.  If the former, then
use the entire file's contents.  Else use those of the current subtree."
  (let ((buffer (get-buffer-create "*prot-org-custom-clock-report*")))
    (save-restriction
      (unwind-protect
          (progn
            (pcase scope
              ('file nil)
              ('subtree (org-narrow-to-subtree))
              (_ (error "The scope `%s' is unknown" scope)))
            (let ((contents (buffer-substring (point-min) (point-max))))
              (with-current-buffer buffer
                (erase-buffer)
                (org-mode)
                (save-excursion
                  (insert (format "%s\n\n" prot-org-clock--template-with-effort))
                  (insert contents))
                (save-excursion
                  (let ((range (prot-org-clock--report-range-prompt)))
                    (goto-char (line-end-position))
                    (insert (concat " :block " range))))
                (org-dblock-update))))
        (widen)))
    buffer))

;;;###autoload
(defun prot-org-clock-report-current-subtree-or-file (&optional whole-buffer)
  "Produce a clock report in a new buffer for the subtree at point.
With optional WHOLE-BUFFER as a non-nil value, operate on the entire file.
When called interactively WHOLE-BUFFER is a prefix argument."
  (interactive "P")
  (when-let* ((buffer (prot-org-clock--get-report (if whole-buffer 'file 'subtree))))
    (pop-to-buffer buffer)))

;;;###autoload
(defun prot-org-clock-select-heading-and-clock-report ()
  "Select a heading in a file and do a clock report for it in a new buffer."
  (interactive)
  (call-interactively 'prot-org-select-heading-in-file)
  (call-interactively 'prot-org-clock-report-current-subtree-or-file))

;;;;; Coaching-related Org custom clocking

;; TODO 2024-12-15: This sort of thing must exist in Org, but I did
;; not find it.
(defun prot-org--timestamp-to-time (string)
  "Return time object of STRING timestamp."
  (org-timestamp-to-time (org-timestamp-from-string string)))

(defun prot-org-coach--get-entries (todo-keyword string since)
  "Get Org entries matching TODO-KEYWORD followed by STRING in the heading.
Limit entries to those whole deadline/scheduled is equal or greater to
SINCE date.

Each entry is a plist of :heading, :contents, :started, :closed."
  (or (delq nil
            (org-map-entries
             (lambda ()
               (when-let* ((case-fold-search t)
                           (started (prot-org--timestamp-to-time (or (org-entry-get nil "DEADLINE") (org-entry-get nil "SCHEDULED"))))
                           (closed (prot-org--timestamp-to-time (org-entry-get nil "CLOSED")))
                           ((re-search-forward (format "\\<%s\\>.*\\<%s\\>" todo-keyword string) (line-end-position) t 1))
                           ((org-time-less-p since started)))
                 (list
                  :heading (org-get-heading :no-tags :no-todo :no-priority :no-comment)
                  :contents (org-get-entry)
                  :started started
                  :closed closed)))))
      (user-error "No entries with heading matching `\\<%s\\>.*\\<%s\\>'" todo-keyword string)))

(defvar prot-org-coach--name-history nil
  "Minibuffer history of `prot-org-coach--name-prompt'.")

(defun prot-org-coach--name-prompt ()
  "Prompt for name of person."
  (let ((default (car prot-org-coach--name-history)))
    (read-string
     (format-prompt "Name of person" default)
     nil 'prot-org-coach--name-history default)))

;;;###autoload
(defun prot-org-coach-report (name since)
  "Produce clock report for coaching with person of NAME.
SINCE is the date (of time 00:00) to count from until now."
  (interactive
   (list
    (prot-org-coach--name-prompt)
    (format "[%s]" (org-read-date))))
  (if-let* ((since-object (prot-org--timestamp-to-time since))
            (entries (prot-org-coach--get-entries "done" name since-object))
            (buffer (get-buffer-create "*prot-org-coach-entries*")))
      (with-current-buffer (pop-to-buffer buffer)
        (erase-buffer)
        (org-mode)
        (dolist (entry entries)
          (insert (format "* %s\n%s\n\n" (plist-get entry :heading) (plist-get entry :contents)))
          (org-clock-in nil (plist-get entry :started))
          (org-clock-out nil t (plist-get entry :closed)))
        (goto-char (point-min))
        (save-excursion
          (insert (format "%s\n\n" prot-org-clock--template-no-effort)))
        (save-excursion
          (goto-char (line-end-position))
          (insert (format " :tstart %S" since)))
        (org-dblock-update))
    (user-error "No entries for name `%s'" name)))

;;;; org-agenda

(declare-function calendar-day-name "calendar")
(declare-function calendar-day-of-week "calendar")
(declare-function calendar-month-name "calendar")
(declare-function org-days-to-iso-week "org")
(declare-function calendar-absolute-from-gregorian "calendar")

(defvar org-agenda-format-date)

;;;###autoload
(defun prot-org-agenda-format-date-aligned (date)
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
         ;; (weekyear (cond ((and (= month 1) (>= iso-week 52))
         ;;                  (1- year))
         ;;                 ((and (= month 12) (<= iso-week 1))
         ;;                  (1+ year))
         ;;                 (t year)))
         (weekstring (if (= day-of-week 1)
                         (format " (W%02d)" iso-week)
                       "")))
    (format "%s %2d %s %4d%s"
            dayname day monthname year weekstring)))

(defvar org-priority-highest)

(defun prot-org-agenda-include-priority-no-timestamp ()
  "Return nil if heading has a priority but no timestamp.
Otherwise, return the buffer position from where the search should
continue, per `org-agenda-skip-function'."
  (let ((point (point)))
    (if (and (eq (nth 3 (org-heading-components)) ?A)
             (not (org-get-deadline-time point))
             (not (org-get-scheduled-time point)))
        nil
      (line-beginning-position 2))))

(defvar prot-org-custom-daily-agenda
  ;; NOTE 2021-12-08: Specifying a match like the following does not
  ;; work.
  ;;
  ;; tags-todo "+PRIORITY=\"A\""
  ;;
  ;; So we match everything and then skip entries with
  ;; `org-agenda-skip-function'.
  `((tags-todo "*"
               ((org-agenda-overriding-header "Important tasks without a date\n")
                ;; NOTE 2024-10-31: Those used to work, but now the
                ;; query for the timestamp is ignored.  I thus wrote
                ;; `prot-org-agenda-include-priority-no-timestamp'.
                ;;
                ;; (org-agenda-skip-function '(org-agenda-skip-subtree-if nil '(timestamp)))
                ;; (org-agenda-skip-function
                ;;  `(org-agenda-skip-entry-if
                ;;    'notregexp ,(format "\\[#%s\\]" (char-to-string org-priority-highest))))
                (org-agenda-skip-function #'prot-org-agenda-include-priority-no-timestamp)
                (org-agenda-block-separator nil)))
    (agenda "" ((org-agenda-overriding-header "\nPending scheduled tasks")
                (org-agenda-time-grid nil)
                (org-agenda-start-on-weekday nil)
                (org-agenda-span 1)
                (org-agenda-show-all-dates nil)
                (org-scheduled-past-days 365)
                ;; Excludes today's scheduled items
                (org-scheduled-delay-days 1)
                (org-agenda-block-separator nil)
                (org-agenda-entry-types '(:scheduled))
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'regexp "ROUTINE"))
                (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                (org-agenda-format-date "")))
    (agenda "" ((org-agenda-overriding-header "\nToday's agenda\n")
                (org-agenda-span 1)
                (org-deadline-warning-days 0)
                (org-agenda-block-separator nil)
                (org-scheduled-past-days 0)
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'regexp "ROUTINE"))
                ;; We don't need the `org-agenda-date-today'
                ;; highlight because that only has a practical
                ;; utility in multi-day views.
                (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                (org-agenda-format-date "%A %-e %B %Y")))
    ;; (agenda "" ((org-agenda-overriding-header "\nRoutine")
    ;;             (org-agenda-time-grid nil)
    ;;             (org-agenda-start-on-weekday nil)
    ;;             (org-agenda-span 1)
    ;;             (org-agenda-show-all-dates nil)
    ;;             (org-scheduled-past-days 365)
    ;;             ;; Excludes today's scheduled items
    ;;             (org-scheduled-delay-days 1)
    ;;             (org-agenda-block-separator nil)
    ;;             (org-agenda-entry-types '(:scheduled))
    ;;             (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp "ROUTINE"))
    ;;             (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
    ;;             (org-agenda-format-date "")))
    (agenda "" ((org-agenda-overriding-header "\nNext three days\n")
                (org-agenda-start-on-weekday nil)
                (org-agenda-start-day nil)
                (org-agenda-start-day "+1d")
                (org-agenda-span 3)
                (org-deadline-warning-days 0)
                (org-agenda-block-separator nil)
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))))
    (agenda "" ((org-agenda-overriding-header "\nUpcoming deadlines (+14d)\n")
                (org-agenda-time-grid nil)
                (org-agenda-start-on-weekday nil)
                ;; We don't want to replicate the previous section's
                ;; three days, so we start counting from the day after.
                (org-agenda-start-day "+4d")
                (org-agenda-span 14)
                (org-agenda-show-all-dates nil)
                (org-deadline-warning-days 0)
                (org-agenda-block-separator nil)
                (org-agenda-entry-types '(:deadline))
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done)))))
  "Custom agenda for use in `org-agenda-custom-commands'.")

;;;;; agenda appointments

(defvar prot-org-agenda-after-edit-hook nil
  "Hook that runs after select Org commands.
To be used with `advice-add'.")

(defun prot-org--agenda-after-edit (&rest _)
  "Run `prot-org-agenda-after-edit-hook'."
  (run-hooks 'prot-org-agenda-after-edit-hook))

(defvar prot-org-after-deadline-or-schedule-hook nil
  "Hook that runs after `org--deadline-or-schedule'.
To be used with `advice-add'.")

(defvar prot-org--appt-agenda-commands
  '( org-agenda-archive org-agenda-deadline org-agenda-schedule
     org-agenda-todo org-archive-subtree)
  "List of commands that run `prot-org-agenda-after-edit-hook'.")

(dolist (fn prot-org--appt-agenda-commands)
  (advice-add fn :after #'prot-org--agenda-after-edit))

(defun prot-org--after-deadline-or-schedule (&rest _)
  "Run `prot-org-after-deadline-or-schedule-hook'."
  (run-hooks 'prot-org-after-deadline-or-schedule-hook))

(defun prot-org-org-agenda-to-appt ()
  "Make `org-agenda-to-appt' always refresh appointment list."
  (org-agenda-to-appt :refresh))

(dolist (hook '(org-capture-after-finalize-hook
                org-after-todo-state-change-hook
                org-agenda-after-show-hook
                prot-org-agenda-after-edit-hook))
  (add-hook hook #'prot-org-org-agenda-to-appt))

(declare-function org--deadline-or-schedule "org" (arg type time))

(advice-add #'org--deadline-or-schedule :after #'prot-org--after-deadline-or-schedule)

(add-hook 'prot-org-after-deadline-or-schedule-hook #'prot-org-org-agenda-to-appt)

;;;; org-export

(declare-function org-html-export-as-html "org")
(declare-function org-texinfo-export-to-info "org")

;;;###autoload
(defun prot-org-ox-html ()
  "Streamline HTML export."
  (interactive)
  (org-html-export-as-html nil nil nil t nil))

;;;###autoload
(defun prot-org-ox-texinfo ()
  "Streamline Info export."
  (interactive)
  (org-texinfo-export-to-info))

;;;; org-id

(declare-function org-id-add-location "org")
(declare-function org-with-point-at "org")
(declare-function org-entry-get "org")
(declare-function org-id-new "org")
(declare-function org-entry-put "org")

;; Original idea:
;; <https://writequit.org/articles/emacs-org-mode-generate-ids.html>.
(defun prot-org--id-get ()
  "Get the CUSTOM_ID of the current entry.
If the entry already has a CUSTOM_ID, return it as-is, else
create a new one."
  (let* ((pos (point))
         (id (org-entry-get pos "CUSTOM_ID")))
    (if (and id (stringp id) (string-match-p "\\S-" id))
        id
      (setq id (org-id-new "h"))
      (org-entry-put pos "CUSTOM_ID" id)
      id)))

(declare-function org-map-entries "org")

;;;###autoload
(defun prot-org-id-headlines ()
  "Add missing CUSTOM_ID to all headlines in current file."
  (interactive)
  (org-map-entries
   (lambda () (prot-org--id-get))))

;;;###autoload
(defun prot-org-id-headline ()
  "Add missing CUSTOM_ID to headline at point."
  (interactive)
  (prot-org--id-get))

;;;###autoload
(defun prot-org-get-dotemacs-link ()
  "Get URL to current heading in my dotemacs file."
  (declare (interactive-only t))
  (interactive)
  (if (and (derived-mode-p 'org-mode)
           (string-match-p "prot-emacs\\.org\\'" buffer-file-name))
      (if-let* ((id (org-entry-get (point) "CUSTOM_ID"))
                (url (concat "https://protesilaos.com/emacs/dotemacs#" id)))
          (progn
            (kill-new url)
            (message "Copied %s" (propertize url 'face 'success)))
        (error "No CUSTOM_ID for the current entry"))
    (user-error "You are not in the right file")))

(provide 'prot-org)
;;; prot-org.el ends here
