;;; tmr-sound.el --- Play a sound -*- lexical-binding: t -*-

;; Copyright (C) 2020-2022  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>,
;;         Damien Cassou <damien@cassou.me>
;; Maintainer: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://git.sr.ht/~protesilaos/tmr
;; Mailing list: https://lists.sr.ht/~protesilaos/tmr
;; Version: 0.3.1
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
;; Provides a function to play a configurable sound file.  This is
;; useful to get an audio notification when a timer completes.  This
;; feature requires "ffplay" (part of ffmpeg) to be in the path.
;;
;; Choose the sound file through the `tmr-sound-file' option: if its
;; value is nil or if the file is not found, no sound will be played.
;;
;; Please read the manual for all the technicalities.  Either evaluate
;; (info "(tmr) Top") or visit <https://protesilaos.com/emacs/tmr>.

;;; Code:
(require 'tmr)

(defcustom tmr-sound-file
  "/usr/share/sounds/freedesktop/stereo/alarm-clock-elapsed.oga"
  "Path to sound file used by `tmr--play-sound'.
If nil, don't play any sound."
  :type '(choice
          file
          (const :tag "Off" nil))
  :group 'tmr)

;; NOTE 2022-04-21: Emacs has a `play-sound' function but it only
;; supports .wav and .au formats.  Also, it does not work on all
;; platforms and Emacs needs to be compiled --with-sound capabilities.
;;;###autoload
(defun tmr-sound-play (&optional _timer)
  "Play `tmr-sound-file' using the 'ffplay' executable (ffmpeg).
TIMER is unused."
  (when-let ((sound tmr-sound-file)
             ((file-exists-p sound)))
    (unless (executable-find "ffplay")
      (user-error "Cannot play %s without `ffplay'" sound))
    (call-process-shell-command
     (format "ffplay -nodisp -autoexit %s >/dev/null 2>&1" sound) nil 0)))

(provide 'tmr-sound)
;;; tmr-sound.el ends here
