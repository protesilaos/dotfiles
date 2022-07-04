;;; tmr.el --- Set timers using a convenient notation -*- lexical-binding: t -*-

;; Copyright (C) 2020-2022  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>,
;;         Damien Cassou <damien@cassou.me>,
;;         Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: TMR Development <~protesilaos/tmr@lists.sr.ht>
;; URL: https://git.sr.ht/~protesilaos/tmr
;; Mailing-List: https://lists.sr.ht/~protesilaos/tmr
;; Version: 0.3.1
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

(require 'seq)
(eval-when-compile (require 'cl-lib))

(defgroup tmr ()
  "TMR May Ring: set timers using a simple notation."
  :group 'data)

(define-obsolete-variable-alias
  'tmr-descriptions-list
  'tmr-description-list
  "0.4.0")

(defcustom tmr-description-list 'tmr-description-history
  "List of timer description presets.
The value can be either a list of strings or the symbol of a
variable that holds a list of strings.

The default value of `tmr-description-history', is the name of a
variable that contains input provided by the user at the relevant
prompt of the `tmr' and `tmr-with-description' commands."
  :type '(choice symbol (repeat string)))

(defcustom tmr-sound-file
  "/usr/share/sounds/freedesktop/stereo/alarm-clock-elapsed.oga"
  "Path to sound file used by `tmr-sound-play'.
If nil, don't play any sound."
  :type '(choice
          file
          (const :tag "Off" nil)))

(defcustom tmr-confirm-single-timer t
  "Whether to act on the sole timer outright or with confirmation.

If non-nil (the default), TMR will use the minibuffer to select a
timer object to operate on, even when there is only one candidate
available.

If set to nil, TMR will not ask for confirmation when there is
one timer available: the operatation will be carried out
outright."
  :type 'boolean)

(defcustom tmr-timer-created-functions
  (list #'tmr-print-message-for-created-timer)
  "Functions to execute when a timer is created.
Each function must accept a timer as argument."
  :type 'hook
  :options '(tmr-print-message-for-created-timer))

(declare-function tmr-notification-notify "ext:tmr-notification.el" (title message))

(define-obsolete-variable-alias
  'tmr-timer-completed-functions
  'tmr-timer-finished-functions
  "0.4.0")

(defcustom tmr-timer-finished-functions
  (list #'tmr-print-message-for-finished-timer
        #'tmr-sound-play
        #'tmr-notification-notify)
  "Functions to execute when a timer is finished.
Each function must accept a timer as argument."
  :type 'hook
  :options (list #'tmr-print-message-for-finished-timer
                 #'tmr-sound-play
                 #'tmr-notification-notify))

(defcustom tmr-timer-cancelled-functions
  (list #'tmr-print-message-for-cancelled-timer)
  "Functions to execute when a timer is created.
Each function must accept a timer as argument."
  :type 'hook)

(cl-defstruct (tmr--timer
               (:constructor tmr--timer-create)
               (:copier tmr--timer-copy))
  (creation-date
   nil
   :read-only t
   :documentation "Time at which the timer was created.")
  (end-date
   nil
   :read-only t
   :documentation "Time at which the timer finishes.")
  (finishedp
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
    (format "TMR start %s; end %s; %s %s%s"
            (propertize start 'face 'success)
            (propertize end 'face 'error)
            (if (string-match-p ":" (tmr--timer-input timer))
                "until"
              "duration")
            (tmr--timer-input timer)
            (if description
                (format " [%s]" (propertize description 'face 'bold))
              ""))))

(defun tmr--long-description-for-finished-timer (timer)
  "Return a human-readable description of finished TIMER.
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

(defun tmr--format-creation-date (timer)
  "Return a string representing when TIMER was created."
  (tmr--format-time (tmr--timer-creation-date timer)))

(defun tmr--format-end-date (timer)
  "Return a string representing when TIMER should finish."
  (tmr--format-time (tmr--timer-end-date timer)))

(defun tmr--format-remaining (timer)
  "Format remaining time of TIMER."
  (if (tmr--timer-finishedp timer)
      "✔"
    (let ((secs (round (- (float-time (tmr--timer-end-date timer))
                          (float-time)))))
      (if (> secs 3600)
          (format "%sh %sm" (/ secs 3600) (/ (% secs 3600) 60))
        (if (> secs 60)
            (format "%sm %ss" (/ secs 60) (% secs 60))
          (format "%ss" secs))))))

(defun tmr--format-time (time)
  "Return a human-readable string representing TIME."
  (format-time-string "%T" time))

(defun tmr--unit (now time)
  "Determine common time unit for TIME given current time NOW."
  (save-match-data
    (cond
     ((natnump time)
      (* time 60))
     ((and (stringp time) (string-match-p "\\`[0-9]+\\(?:\\.[0-9]+\\)?\\'" time))
      (* (string-to-number time) 60))
     ((and (stringp time) (string-match "\\`\\([0-9]+\\):\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?\\'" time))
      (let ((val (decode-time now)))
        (setf (decoded-time-hour val) (string-to-number (match-string 1 time))
              (decoded-time-minute val) (string-to-number (match-string 2 time))
              (decoded-time-second val) (if (match-end 3)
                                            (string-to-number (match-string 3 time))
                                          0)
              val (encode-time val))
        (when (time-less-p val now)
          (user-error "Time %s is already over" time))
        (ceiling (float-time (time-subtract val now)))))
     ((and (stringp time) (string-match "\\`\\([0-9]+\\(?:\\.[0-9]+\\)?\\)[mhs]\\'" time))
      (let ((num (string-to-number (match-string 1 time))))
        (pcase (aref time (1- (length time)))
          (?s num)
          (?h (* num 60 60))
          (?m (* num 60)))))
     (t (user-error "TMR Made Ridiculous; append character for [m]inutes, [h]ours, [s]econds")))))

(defvar tmr--timers nil
  "List of timer objects.
Populated by `tmr' and then operated on by `tmr-cancel'.")

(defvar tmr--update-hook nil
  "Hooks to execute when timers are changed.")

;;;###autoload
(defun tmr-remove (timer)
  "Cancel and remove TIMER object set with `tmr' command.
Interactively, let the user choose which timer to cancel with
completion."
  (interactive (list (tmr--read-timer "Remove timer: ")))
  (cancel-timer (tmr--timer-timer-object timer))
  (setq tmr--timers (delete timer tmr--timers))
  (run-hooks 'tmr--update-hook)
  (run-hook-with-args 'tmr-timer-cancelled-functions timer))

;;;###autoload
(defun tmr-cancel (timer)
  "Cancel TIMER object set with `tmr' command.
Interactively, let the user choose which timer to cancel with
completion.  This command is the same as `tmr-remove' but
chooses only among active timers."
  (interactive (list (tmr--read-timer "Cancel timer: " :active)))
  (tmr-remove timer))

;;;###autoload
(defun tmr-reschedule (timer)
  "Reschedule TIMER.
This is the same as cloning it, prompting for duration and
cancelling the original one."
  (interactive (list (tmr--read-timer "Reschedule timer: ")))
  (tmr-clone timer :prompt)
  (let (tmr-timer-cancelled-functions)
    (tmr-cancel timer)))

;;;###autoload
(defun tmr-edit-description (timer description)
  "Change TIMER description with that of DESCRIPTION."
  (interactive
   (list
    (tmr--read-timer "Edit description of timer: ")
    (tmr--description-prompt)))
  (setf (tmr--timer-description timer) description)
  (run-hooks 'tmr--update-hook))

;;;###autoload
(defun tmr-remove-finished ()
  "Remove all finished timers."
  (interactive)
  (setq tmr--timers (seq-remove #'tmr--timer-finishedp tmr--timers))
  (run-hooks 'tmr--update-hook))

(defvar tmr--read-timer-hook nil
  "Hooks to execute to find current timer.")

(defun tmr--timer-annotation (timer)
  "Annotate TIMER completion candidate with remaining time."
  (setq timer (get-text-property 0 'tmr-timer timer))
  (if (tmr--timer-finishedp timer)
      " (finished)"
    (format " (%s remaining)" (tmr--format-remaining timer))))

(defun tmr--read-timer (prompt &optional active)
  "Let the user choose a timer among all (or ACTIVE) timers.

Return the selected timer.  If there is a single timer and
`tmr-confirm-single-timer' is nil, use that.  If there are
multiple timers, prompt for one with completion with PROMPT text.
If there are no timers, throw an error."
  (or
   (run-hook-with-args-until-success 'tmr--read-timer-hook)
   (pcase
       (if active
           (seq-remove #'tmr--timer-finishedp tmr--timers)
         tmr--timers)
     ('nil (user-error "No timers available"))
     ((and `(,timer) (guard (not tmr-confirm-single-timer))) timer)
     (timers
      (let* ((timer-list (mapcar
                          (lambda (x)
                            (propertize
                             (tmr--long-description x)
                             'tmr-timer x))
                          timers))
             (selected
              (car (member (completing-read
                            prompt
                            (tmr--completion-table
                             timer-list 'tmr-timer #'tmr--timer-annotation)
                            nil t)
                           timer-list))))
        (or (and selected (get-text-property 0 'tmr-timer selected))
            (user-error "No timer selected")))))))

;; NOTE 2022-04-21: Emacs has a `play-sound' function but it only
;; supports .wav and .au formats.  Also, it does not work on all
;; platforms and Emacs needs to be compiled --with-sound capabilities.
(defun tmr-sound-play (&optional _timer)
  "Play `tmr-sound-file' using the ffplay executable (ffmpeg).
TIMER is unused."
  (when-let ((sound tmr-sound-file)
             ((file-exists-p sound)))
    (unless (executable-find "ffplay")
      (user-error "Cannot play %s without `ffplay'" sound))
    (call-process-shell-command
     (format "ffplay -nodisp -autoexit %s >/dev/null 2>&1" sound) nil 0)))

(defun tmr-print-message-for-created-timer (timer)
  "Show a `message' informing the user that TIMER was created."
  (message "%s" (tmr--long-description timer)))

(defun tmr-print-message-for-finished-timer (timer)
  "Show a `message' informing the user that TIMER has finished."
  (message "%s" (tmr--long-description-for-finished-timer timer)))

(define-obsolete-function-alias
  'tmr-print-message-for-completed-timer
  'tmr-print-message-for-finished-timer
  "0.4.0")

(defun tmr-print-message-for-cancelled-timer (timer)
  "Show a `message' informing the user that TIMER is cancelled."
  (message "Cancelled: <<%s>>" (tmr--long-description timer)))

(defvar tmr-duration-history '()
  "Minibuffer history of `tmr' durations.")

(defun tmr--read-duration (&optional default)
  "Ask the user to type a duration.
If DEFAULT is provided, use that as a default."
  (let ((def (or default (nth 0 tmr-duration-history))))
    (read-string
     (if def
         (format "N minutes for timer (append `h' or `s' for other units) [%s]: " def)
       "N minutes for timer (append `h' or `s' for other units): ")
     nil
     'tmr-duration-history def)))

(defvar tmr-description-history '()
  "Minibuffer history of `tmr' descriptions.")

(defun tmr--description-prompt (&optional default)
  "Helper prompt for descriptions in `tmr'.
If optional DEFAULT is provided use it as a default candidate."
  (completing-read
   (if default
       (format "Description for this tmr [%s]: " default)
     "Description for this tmr: ")
   (tmr--completion-table
    (if (listp tmr-description-list)
        tmr-description-list
      (symbol-value tmr-description-list)))
   nil nil nil 'tmr-description-history default))

(defun tmr--complete (timer)
  "Mark TIMER as finished and execute `tmr-timer-finished-functions'."
  (setf (tmr--timer-finishedp timer) t)
  (run-hooks 'tmr--update-hook)
  (run-hook-with-args 'tmr-timer-finished-functions timer))

;;;###autoload
(defun tmr (time &optional description)
  "Set timer to TIME duration and notify after it elapses.

When TIME is a number, it is interpreted as a count of minutes.
Otherwise TIME must be a string that consists of a number and a
special final character denoting a unit of time: h for hours, s
for seconds.

With optional DESCRIPTION as a prefix (\\[universal-argument]),
prompt for a description among `tmr-description-list', though
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
         (duration (tmr--unit creation-date time))
         (timer (tmr--timer-create
                 :description description
                 :creation-date creation-date
                 :end-date (time-add creation-date duration)
                 :input time))
         (timer-object (run-with-timer
                        duration nil
                        #'tmr--complete timer)))
    (setf (tmr--timer-timer-object timer) timer-object)
    (push timer tmr--timers)
    (run-hooks 'tmr--update-hook)
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
confirmation about the duration.  When PROMPT is a double prefix
argument, ask for a description as well.

Without a PROMPT, clone TIMER outright."
  (interactive
   (list
    (tmr--read-timer "Clone timer: ")
    current-prefix-arg))
  (tmr
   (if prompt
       (tmr--read-duration (format "%s" (tmr--timer-input timer)))
     (format "%s" (tmr--timer-input timer)))
   (if (equal prompt '(16))
       (tmr--description-prompt (tmr--timer-description timer))
     (tmr--timer-description timer))))

(defun tmr--completion-table (candidates &optional category annotation)
  "Make completion table for CANDIDATES with sorting disabled.
CATEGORY is the completion category.
ANNOTATION is an annotation function."
   (lambda (str pred action)
     (if (eq action 'metadata)
         `(metadata (display-sort-function . identity)
                    (cycle-sort-function . identity)
                   (annotation-function . ,annotation)
                    (category . ,category))
       (complete-with-action action candidates str pred))))

(provide 'tmr)
;;; tmr.el ends here
