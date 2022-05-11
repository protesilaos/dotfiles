;;; tmr.el --- Set timers using a convenient notation -*- lexical-binding: t -*-

;; Copyright (C) 2020-2022  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>,
;;         Damien Cassou <damien@cassou.me>
;; Maintainer: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://git.sr.ht/~protesilaos/tmr
;; Mailing list: https://lists.sr.ht/~protesilaos/tmr
;; Version: 0.2.3
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
;; using a convenient notation.  The first point of entry is the `tmr'
;; command.  It prompts for a unit of time, which is represented as a
;; string that consists of a number and, optionally, a single character
;; suffix which specifies the unit of time.  Valid input formats:
;;
;; | Input | Meaning   |
;; |-------+-----------|
;; | 5     | 5 minutes |
;; | 5m    | 5 minutes |
;; | 5s    | 5 seconds |
;; | 5h    | 5 hours   |
;;
;; If `tmr' is called with an optional prefix argument (`C-u'), it also
;; asks for a description which accompanies the given timer.  Preconfigured
;; candidates are specified in the user option `tmr-descriptions-list',
;; though any arbitrary input is acceptable at the minibuffer prompt.
;;
;; An alternative to the `tmr' command is `tmr-with-description'.  The
;; difference between the two is that the latter always prompts for a
;; description.
;;
;; When the timer is set, a message is sent to the echo area recording the
;; current time and the point in the future when the timer elapses.  Echo
;; area messages can be reviewed with the `view-echo-area-messages' which is
;; bound to `C-h e' by default.  Though TMR provides its own buffer for
;; reviewing its log: it is named `*tmr-messages*' and can be accessed with
;; the command `tmr-view-echo-area-messages'.
;;
;; Once the timer runs its course, it produces a desktop notification and
;; plays an alarm sound.  The notification's message is practically the
;; same as that which is sent to the echo area.  The sound file for the
;; alarm is defined in `tmr-sound-file', while the urgency of the
;; notification can be set through the `tmr-notification-urgency' option.
;; Note that it is up to the desktop environment or notification daemon to
;; decide how to handle the urgency value.
;;
;; If the `tmr-sound-file' is nil, or the file is not found, no sound will
;; be played.
;;
;; The `tmr-cancel' command is used to cancel running timers (as set by the
;; `tmr' or `tmr-with-description' commands).  If there is only one timer,
;; it cancels it outright.  If there are multiple timers, it produces a
;; minibuffer completion prompt which asks for one among them.  Timers at
;; the completion prompt are described by the exact time they were set and
;; the input that was used to create them, including the optional
;; description that `tmr' and `tmr-with-description' accept.
;;
;; Active timers can be viewed in a grid with `tmr-tabulated-view' (part of
;; the `tmr-tabulated.el' file).  The grid is placed in the
;; `*tmr-tabulated-view*' buffer and looks like this:
;;
;; Start      End        Finished?  Description
;; 12:26:50   12:51:50   ✔         Update tmr manual
;; 12:26:35   12:56:35              Bake bread
;; 12:26:26   12:36:26              Prepare tea
;;
;; If a timer has elapsed, it has a check mark, otherwise the `Finished?'
;; column is empty.  A `Description' is shown only if it is provided while
;; setting the timer, otherwise the field is left blank.
;;
;; The `tmr-tabulated-view' command relies on Emacs' `tabulated-list-mode'.
;; From the `*tmr-tabulated-view*' buffer, invoke the command
;; `describe-mode' to learn about the applicable key bindings, such as how
;; to expand/contract columns and toggle their sort.
;;
;; While in this grid view, the `k' key runs the `tmr-tabulated-cancel'
;; command.  It immediately cancels the timer at point.

;;; Code:

(require 'notifications)

(defgroup tmr ()
  "TMR May Ring: set timers using a simple notation."
  :group 'data)

(defcustom tmr-sound-file
  "/usr/share/sounds/freedesktop/stereo/alarm-clock-elapsed.oga"
  "Path to sound file used by `tmr--play-sound'.
If nil, don't play any sound."
  :type '(choice file
                 (const :tag "Off" nil))
  :group 'tmr)

(defcustom tmr-notification-urgency 'normal
  "The urgency level of the desktop notification.
Values can be `low', `normal' (default), or `critical'.

The desktop environment or notification daemon is responsible for
such notifications."
  :type '(choice
          (const :tag "Low" low)
          (const :tag "Normal" normal)
          (const :tag "Critical" critical))
  :group 'tmr)

(defcustom tmr-descriptions-list (list "Boil water" "Prepare tea" "Bake bread")
  "Optional description candidates for the current `tmr'.
These are provided as completion candidates when `tmr' is called
with a DESCRIPTION argument or when `tmr-with-description' is
used."
  :type '(repeat string)
  :group 'tmr)

(defcustom tmr-notify-function #'tmr-notifications-notify
  "Function called to send notification.
It should take two string arguments: the title and the message."
  :type 'function
  :group 'tmr)

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
  (description
   nil
   :read-only t
   :documentation "Optional string describing the purpose of the timer, e.g., \"Stop the oven\"."))

(defun tmr--long-description (timer)
  "Return a human-readable description for TIMER."
  (let ((start (tmr--format-creation-date timer))
        (duration (tmr--timer-duration timer))
        (description (tmr--timer-description timer)))
    (if description
        (format "Started at %s with input '%s' and description '%s'" start duration description)
      (format "Started at %s with input '%s'" start duration))))

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

;; NOTE 2022-04-21: Emacs has a `play-sound' function but it only
;; supports .wav and .au formats.  Also, it does not work on all
;; platforms and Emacs needs to be compiled --with-sound capabilities.
(defun tmr--play-sound ()
  "Play `tmr-sound-file' using the 'ffplay' executable (ffmpeg)."
  (when-let* ((sound tmr-sound-file))
    (when (file-exists-p sound)
      (unless (executable-find "ffplay")
        (user-error "Cannot play %s without `ffplay'" sound))
      (call-process-shell-command
       (format "ffplay -nodisp -autoexit %s >/dev/null 2>&1" sound) nil 0))))

(defun tmr--log-in-buffer (log)
  "Insert LOG message in tmr buffer."
  (when-let ((buf (get-buffer-create "*tmr-messages*")))
    (with-current-buffer buf
      (messages-buffer-mode)
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (insert (concat log "\n"))))))

(defun tmr-view-echo-area-messages ()
  "View the '*tmr-messages*' buffer if present."
  (interactive)
  (if-let ((buf (get-buffer "*tmr-messages*")))
      (with-current-buffer buf
        (goto-char (point-max))
        (let ((win (display-buffer (current-buffer))))
          ;; If the buffer is already displayed, we need to forcibly set
          ;; the window point to scroll to the end of the buffer.
          (set-window-point win (point))
          win))
    (user-error "No *tmr-messages* buffer; have you used `tmr'?")))

(defun tmr-notifications-notify (title message)
  "Dispatch notification titled TITLE with MESSAGE via D-Bus.

Read: (info \"(elisp) Desktop Notifications\") for details."
  (notifications-notify
   :title title
   :body message
   :app-name "GNU Emacs"
   :urgency tmr-notification-urgency
   :sound-file tmr-sound-file))

(defun tmr--notify-send-notification (title message)
  "Send notification with TITLE and MESSAGE using `tmr-notify-function'."
  (funcall tmr-notify-function title message))

(defun tmr--notify (timer)
  "Send notification for TIMER."
  (let* ((description (tmr--timer-description timer))
         (desc-plain (if description
                         (concat "\n" description)
                       ""))
         (desc-propertized (if description
                               (concat " [" (propertize description 'face 'bold) "]")
                             ""))
         (start (tmr--format-creation-date timer))
         (end (tmr--format-end-date timer)))
    (setf (tmr--timer-donep timer) t)
    (tmr--notify-send-notification
     "TMR May Ring (Emacs tmr package)"
     (format "Time is up!\nStarted: %s\nEnded: %s%s" start end desc-plain))
    (message
     "TMR %s %s ; %s %s%s"
     (propertize "Start:" 'face 'success) start
     (propertize "End:" 'face 'error) end
     desc-propertized)
    (tmr--log-in-buffer (format "Completed at %s what started at %s" end start))
    (unless (plist-get (notifications-get-capabilities) :sound)
      (tmr--play-sound))))

(defvar tmr--timers nil
  "List of timer objects.
Populated by `tmr' and then operated on by `tmr-cancel'.")

(defun tmr--get-timer-by-creation-date (creation-date)
  "Return the timer which was started at CREATION-DATE."
  (cl-find creation-date tmr--timers :key #'tmr--timer-creation-date))

;;;###autoload
(defun tmr-cancel (timer)
  "Cancel TIMER object set with `tmr' command.
Interactively, let the user choose which timer to cancel with
completion."
  (interactive (list (tmr--read-timer)))
  (if (not timer)
      (user-error "No `tmr' to cancel")
    (cancel-timer (tmr--timer-timer-object timer))
    (tmr--log-in-buffer (format "CANCELLED <<%s>>" (tmr--long-description timer)))
    (setq tmr--timers (cl-delete timer tmr--timers))))

(defun tmr--read-timer ()
  "Let the user choose a timer among all timers.
Return the selected timer.  If there is a single timer, use that.
If there are multiple timers, prompt for one with completion.  If
there are no timers, return nil."
  (let ((timers tmr--timers))
    (cond
     ((= (length timers) 1)
      (car timers))
     ((> (length timers) 1)
      (let* ((timer-descriptions (mapcar #'tmr--long-description timers))
             (selection (completing-read "Timer: " timer-descriptions nil t)))
        (cl-find selection timers :test #'string= :key #'tmr--long-description))))))

(defun tmr--echo-area (time &optional description)
  "Produce `message' for current `tmr' TIME.
Optionally include DESCRIPTION."
  (let* ((specifier (substring time -1))
         (amount (substring time 0 -1))
         (start (tmr--format-time (current-time)))
         (unit (pcase specifier
                 ("s" (format "%ss (s == second)" amount))
                 ("h" (format "%sh (h == hour)" amount))
                 (_   (concat time "m (m == minute)")))))
    (message "`tmr' started at %s for %s%s"
             ;; Remember: these are just faces.  Don't get caught in the
             ;; semantics.
             (propertize start 'face 'success)
             (propertize unit 'face 'error)
             (if description
                 (format " [%s]" (propertize description 'face 'bold))
               ""))))

(defvar tmr--duration-hist '()
  "Minibuffer history of `tmr' durations.")

(defun tmr--read-duration ()
  "Ask the user to type a duration."
  (let ((def (nth 0 tmr--duration-hist)))
    (read-string
     (if def
         (format "N minutes for timer (append `h' or `s' for other units) [%s]: " def)
       "N minutes for timer (append `h' or `s' for other units): ")
     nil
     'tmr--duration-hist def)))

(defvar tmr--description-hist '()
  "Minibuffer history of `tmr' descriptions.")

(defun tmr--description-prompt ()
  "Helper prompt for descriptions in `tmr'."
  (let ((def (nth 0 tmr--description-hist)))
    (completing-read
     (if def
         (format "Description for this tmr [%s]: " def)
       "Description for this tmr: ")
     tmr-descriptions-list nil nil nil
     'tmr--description-hist def)))

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
                 :duration duration))
         (timer-object (run-with-timer
                        duration nil
                        #'tmr--notify timer)))
    (setf (tmr--timer-timer-object timer) timer-object)
    (tmr--echo-area time description)
    (push timer tmr--timers)
    (tmr--log-in-buffer (tmr--long-description timer))))

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

(provide 'tmr)
;;; tmr.el ends here
