;;; tmr.el --- TMR Must Recur -*- lexical-binding: t -*-

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
;; TMR Must Recur.  Else a super simple timer for my Emacs setup:
;; https://protesilaos.com/dotemacs.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

(require 'notifications)

(defgroup tmr ()
  "TMR Must Recur (super simple timer for my private use)."
  :group 'data)

(defcustom tmr-sound-file
  "/usr/share/sounds/freedesktop/stereo/alarm-clock-elapsed.oga"
  "Path to sound file used by `tmr--play-sound'."
  :type 'string
  :group 'tmr)

(defun tmr--unit (time)
  "Determine common time unit for TIME."
  (if (numberp time) ; TODO: how to recognise a positive/negative number?
      ;; NOTE: If time is negative, beeps immediately (bug?). so
      ;; convert time to absolute value.
      (* (abs time) 60)
    (let* ((unit (substring time -1))
           (str (substring time 0 -1))
           (num (abs (string-to-number str))))
      (pcase unit
        ("s" num)
        ("h" (* num 60 60))
        ;; This is not needed, of course, but we should not miss a good
        ;; chance to make some fun of ourselves.
        ("w" (user-error "TMR Made Ridiculous; use minutes, hours, seconds"))
        (_ (* num 60))))))

(defun tmr--play-sound ()
  "Play `tmr-sound-file' using the 'ffplay' executable (ffmpeg)."
  (let ((sound tmr-sound-file))
    (when (and (file-exists-p sound)
               (executable-find "ffplay"))
      (call-process-shell-command
       (format "ffplay -nodisp -autoexit %s >/dev/null 2>&1" sound) nil 0))))

(defun tmr--notify-send (start)
  "Send system notification for timer with START time."
  (let ((end (format-time-string "%R")))
    (notifications-notify
     :title "TMR Must Recur"
     :body (format "Time is up!\nStarted: %s\nEnded: %s" start end)
     :app-name "GNU Emacs")
    (message
     "TMR %s %s ; %s %s"
     (propertize "Start:" 'face 'success) start
     (propertize "End:" 'face 'warning) end)
    (tmr--play-sound)))

;;;###autoload
(defun tmr (time)
  "Set timer to TIME duration and notify after it elapses.

When TIME is a number, it is interpreted as a count of minutes.
Otherwise TIME must be a string that consists of a number and a
special final character denoting a unit of time: 'h' for 'hours',
's' for 'seconds'.

This command also plays back `tmr-sound-file'."
  (let ((start (format-time-string "%R"))
        (unit (tmr--unit time)))
    (run-with-timer
     unit nil
     'tmr--notify-send start)))

(provide 'tmr)
;;; tmr.el ends here
