;;; tmr.el --- Set timers using a convenient notation -*- lexical-binding: t -*-

;; Copyright (C) 2020-2022  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>,
;;         Damien Cassou <damien@cassou.me>
;; Maintainer: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://git.sr.ht/~protesilaos/tmr
;; Mailing list: https://lists.sr.ht/~protesilaos/tmr
;; Version: 0.3.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, timer

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
;; TMR is an Emacs package that provides facilities for setting timers
;; using a convenient notation.
;;
;; Please read the manual for all the technicalities.  Either evaluate
;; (info "(tmr) Top") or visit <https://protesilaos.com/emacs/tmr>.

;;; Code:

(eval-when-compile (require 'cl-lib))

(defgroup tmr ()
  "TMR May Ring: set timers using a simple notation."
  :group 'data)

(defcustom tmr-descriptions-list (list "Boil water" "Prepare tea" "Bake bread")
  "Optional description candidates for the current `tmr'.
These are provided as completion candidates when `tmr' is called
with a DESCRIPTION argument or when `tmr-with-description' is
used."
  :type '(repeat string)
  :group 'tmr)

(defcustom tmr-timer-created-functions
  (list #'tmr-print-message-for-created-timer)
  "Functions to execute when a timer is created.
Each function must accept a timer as argument."
  :type 'hook
  :options '(tmr-print-message-for-created-timer))

(declare-function tmr-sound-play "ext:tmr-sound.el" (&optional timer))
(declare-function tmr-notification-notify "ext:tmr-notification.el" (title message))

(defcustom tmr-timer-completed-functions
  (list #'tmr-print-message-for-completed-timer
        #'tmr-sound-play
        #'tmr-notification-notify)
  "Functions to execute when a timer is completed.
Each function must accept a timer as argument."
  :type 'hook
  :options (list #'tmr-print-message-for-completed-timer
                 #'tmr-sound-play
                 #'tmr-notification-notify))

(defcustom tmr-timer-cancelled-functions
  (list #'tmr-print-message-for-cancelled-timer)
  "Functions to execute when a timer is created.
Each function must accept a timer as argument."
  :type 'hook)

(cl-defstruct (tmr-timer
               (:constructor tmr--timer-create)
               (:conc-name tmr--timer-))
  (creation-date
   nil
   :read-only t
   :documentation "Time at which the timer was created.")
  (duration
   nil
   :read-only t
   :documentation "Number of seconds after `start' indicating when the timer finishes.")
  (donep
   nil
   :read-only nil
   :documentation "Non-nil if the timer is finished.")
  (timer-object
   nil
   :read-only nil
   :documentation "The object returned by `run-with-timer'.")
  (input
   nil
   :read-only t
   :documentation "The original input which is internally interpreted as a duration.")
  (description
   nil
   :read-only nil
   :documentation "Optional string describing the purpose of the timer, e.g., \"Stop the oven\"."))

(defun tmr--long-description (timer)
  "Return a human-readable description for TIMER."
  (let ((start (tmr--format-creation-date timer))
        (end (tmr--format-end-date timer))
        (description (tmr--timer-description timer)))
    ;; We prefix it with TMR just so it is easier to find in
    ;; `view-echo-area-messages'.  The concise wording makes it flexible
    ;; enough to be used when starting a timer but also when cancelling
    ;; one: check `tmr-print-message-for-created-timer' and
    ;; `tmr-print-message-for-cancelled-timer'.
    (format "TMR start at %s; end at %s%s"
            (propertize start 'face 'success)
            (propertize end 'face 'error)
            (if description
                (format " [%s]" (propertize description 'face 'bold))
              ""))))

(defun tmr--long-description-for-completed-timer (timer)
  "Return a human-readable description of completed TIMER.
This includes the creation and completion dates as well as the
optional `tmr--timer-description'."
  (let ((start (tmr--format-creation-date timer))
        (end (tmr--format-end-date timer))
        (description (tmr--timer-description timer)))
    ;; For the TMR prefix, see comment in `tmr--long-description'.
    (format "TMR Time is up!\n%s%s %s\n%s %s"
            (if description (format "%s\n" description) "")
            (propertize "Started" 'face 'success)
            start
            (propertize "Ended" 'face 'error)
            end)))

(defun tmr--long-description-for-clonable-timer (timer)
  "Return a human-readable description for clonable TIMER.
This is like `tmr--long-description' with the inclusion of the
original input for TIMER's duration."
  (let ((start (tmr--format-creation-date timer))
        (end (tmr--format-end-date timer))
        (description (tmr--timer-description timer))
        (input (tmr--timer-input timer)))
    (format "TMR start at %s; end at %s%s (input was '%s')"
            (propertize start 'face 'success)
            (propertize end 'face 'error)
            (if description
                (format " [%s]" (propertize description 'face 'bold))
              "")
            (propertize input 'face 'warning))))

(defun tmr--format-creation-date (timer)
  "Return a string representing when TIMER was created."
  (tmr--format-time (tmr--timer-creation-date timer)))

(defun tmr--format-end-date (timer)
  "Return a string representing when TIMER should finish."
  (format-time-string "%T" (time-add (tmr--timer-creation-date timer)
                                     (tmr--timer-duration timer))))

(defun tmr--format-time (time)
  "Return a human-readable string representing TIME."
  (format-time-string "%T" time))

(defun tmr--unit (time)
  "Determine common time unit for TIME."
  (cond
   ((and (stringp time)
         (string-match-p "[0-9]\\'" time))
    (let ((time (string-to-number time)))
      (* time 60)))
   ((natnump time)
    (* time 60))
   (t
    (let* ((unit (substring time -1))
           (str (substring time 0 -1))
           (num (abs (string-to-number str))))
      (pcase unit
        ("s" num)
        ("h" (* num 60 60))
        ;; This is not needed, of course, but we should not miss a good
        ;; chance to make some fun of ourselves.
        ("w" (user-error "TMR Made Ridiculous; append character for [m]inutes, [h]ours, [s]econds"))
        (_ (* num 60)))))))

(defvar tmr--timers nil
  "List of timer objects.
Populated by `tmr' and then operated on by `tmr-cancel'.")

(declare-function cl-find "cl-seq" (cl-item cl-seq &rest cl-keys))
(declare-function cl-delete "cl-seq" (cl-item cl-seq &rest cl-keys))
(declare-function cl-delete-if "cl-seq" (cl-pred cl-list &rest cl-keys))
(declare-function cl-remove-if "cl-seq" (cl-pred cl-list &rest cl-keys))

(defun tmr--active-timers ()
  "Retun list of active timers."
  (cl-remove-if
   (lambda (timer)
     (tmr--timer-donep timer))
   tmr--timers))

(defun tmr--get-timer-by-creation-date (creation-date)
  "Return the timer which was started at CREATION-DATE."
  (cl-find creation-date tmr--timers :key #'tmr--timer-creation-date))

;;;###autoload
(defun tmr-cancel (timer &optional no-hooks)
  "Cancel TIMER object set with `tmr' command.
Interactively, let the user choose which timer to cancel with
completion.

With optional NO-HOOKS refrain from calling
`tmr-timer-cancelled-functions'."
  (interactive (list (tmr--read-timer :active) current-prefix-arg))
  (if (not timer)
      (user-error "No `tmr' to cancel")
    (cancel-timer (tmr--timer-timer-object timer))
    (setq tmr--timers (cl-delete timer tmr--timers))
    (unless no-hooks
      (run-hook-with-args 'tmr-timer-cancelled-functions timer))))

(defun tmr-remove-finished ()
  "Remove all finished timers."
  (interactive)
  (setq tmr--timers (cl-delete-if #'tmr--timer-donep tmr--timers)))

(defun tmr--read-timer (&optional active description)
  "Let the user choose a timer among all timers.
Return the selected timer.  If there is a single timer, use that.
If there are multiple timers, prompt for one with completion.  If
there are no timers, return nil.

If optional ACTIVE is non-nil, limit the list of timers to those
that are still running.

If optional DESCRIPTION is provided use it to format the
completion candidates."
  (let ((timers (if active (tmr--active-timers) tmr--timers)))
    (cond
     ((null timers)
      (user-error "No timers available"))
     ((= (length timers) 1)
      (car timers))
     ((> (length timers) 1)
      (let* ((formatter (or description #'tmr--long-description))
             (timer-descriptions (mapcar formatter timers))
             (selection (completing-read "Timer: " timer-descriptions nil t)))
        (cl-find selection timers :test #'string= :key formatter))))))

(defun tmr-print-message-for-created-timer (timer)
  "Show a `message' informing the user that TIMER was created."
  (message "%s" (tmr--long-description timer)))

(defun tmr-print-message-for-completed-timer (timer)
  "Show a `message' informing the user that TIMER has completed."
  (message "%s" (tmr--long-description-for-completed-timer timer)))

(defun tmr-print-message-for-cancelled-timer (timer)
  "Show a `message' informing the user that TIMER is cancelled."
  (message "Cancelled: <<%s>>" (tmr--long-description timer)))

(defvar tmr--duration-hist '()
  "Minibuffer history of `tmr' durations.")

(defun tmr--read-duration (&optional default)
  "Ask the user to type a duration.
If DEFAULT is provided, use that as a default."
  (let ((def (or default (nth 0 tmr--duration-hist))))
    (read-string
     (if def
         (format "N minutes for timer (append `h' or `s' for other units) [%s]: " def)
       "N minutes for timer (append `h' or `s' for other units): ")
     nil
     'tmr--duration-hist def)))

(defvar tmr--description-hist '()
  "Minibuffer history of `tmr' descriptions.")

(defun tmr--description-prompt (&optional default)
  "Helper prompt for descriptions in `tmr'.
If optional DEFAULT is provided use it as a default candidate."
  (completing-read
   (if default
       (format "Description for this tmr [%s]: " default)
     "Description for this tmr: ")
   (lambda (string predicate action)
     (if (eq action 'metadata)
         `(metadata (display-sort-function . ,#'identity)
                    (cycle-sort-function . ,#'identity))
       (complete-with-action
        action tmr-descriptions-list string predicate)))
   nil nil nil
   'tmr--description-hist default))

(defun tmr--complete (timer)
  "Mark TIMER as completed and execute `tmr-timer-completed-functions'."
  (setf (tmr--timer-donep timer) t)
  (run-hook-with-args 'tmr-timer-completed-functions timer))

;;;###autoload
(defun tmr (time &optional description)
  "Set timer to TIME duration and notify after it elapses.

When TIME is a number, it is interpreted as a count of minutes.
Otherwise TIME must be a string that consists of a number and a
special final character denoting a unit of time: 'h' for 'hours',
's' for 'seconds'.

With optional DESCRIPTION as a prefix (\\[universal-argument]),
prompt for a description among `tmr-descriptions-list', though
allow for any string to serve as valid input.

This command also plays back `tmr-sound-file' if it is available.

To cancel the timer, use the `tmr-cancel' command.

To always prompt for a DESCRIPTION when setting a timer, use the
command `tmr-with-description' instead of this one."
  (interactive
   (list
    (tmr--read-duration)
    (when current-prefix-arg (tmr--description-prompt))))
  (let* ((creation-date (current-time))
         (duration (tmr--unit time))
         (timer (tmr--timer-create
                 :description description
                 :creation-date creation-date
                 :duration duration
                 :input time))
         (timer-object (run-with-timer
                        duration nil
                        #'tmr--complete timer)))
    (setf (tmr--timer-timer-object timer) timer-object)
    (push timer tmr--timers)
    (run-hook-with-args 'tmr-timer-created-functions timer)))

;;;###autoload
(defun tmr-with-description (time description)
  "Set timer to TIME duration and notify with DESCRIPTION after it elapses.

See `tmr' for a description of the arguments.  The difference
between the two commands is that `tmr-with-description' always
asks for a description whereas `tmr' only asks for it when the
user uses a prefix argument (\\[universal-argument])."
  (interactive
   (list
    (tmr--read-duration)
    (tmr--description-prompt)))
  (tmr time description))

;;;###autoload
(defun tmr-clone (timer &optional prompt)
  "Create a new timer by cloning TIMER.
With optional PROMPT, such as a prefix argument, ask for
confirmation about the duration and the description.  The
description is asked only if TIMER had one.

Without a PROMPT, clone TIMER outright."
  (interactive
   (list
    (tmr--read-timer nil #'tmr--long-description-for-clonable-timer)
    current-prefix-arg))
  (let ((description (tmr--timer-description timer)))
    (cond
     (prompt
      (tmr
       (tmr--read-duration (format "%s" (tmr--timer-input timer)))
       (when description (tmr--description-prompt description))))
     (t
      (tmr
       (format "%s" (tmr--timer-input timer))
       (tmr--timer-description timer))))))

(provide 'tmr)
;;; tmr.el ends here
