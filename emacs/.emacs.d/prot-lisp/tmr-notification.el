;;; tmr-notification.el --- Display timers in a notification list -*- lexical-binding: t -*-

;; Copyright (C) 2020-2022  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>,
;;         Damien Cassou <damien@cassou.me>,
;;         Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: TMR Development <~protesilaos/tmr@lists.sr.ht>
;; URL: https://git.sr.ht/~protesilaos/tmr
;; Mailing-List: https://lists.sr.ht/~protesilaos/tmr
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
;; Provides a function to display a desktop notification.  This is
;; useful to get a passive popup when a timer completes.
;;
;; Please read the manual for all the technicalities.  Either evaluate
;; (info "(tmr) Top") or visit <https://protesilaos.com/emacs/tmr>.
;;
;; Developers can also read (info "(elisp) Desktop Notifications") for
;; details.

;;; Code:
(require 'tmr)
(require 'notifications)

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

;;;###autoload
(defun tmr-notification-notify (timer)
  "Dispatch a notification for TIMER.

Read: (info \"(elisp) Desktop Notifications\") for details."
  (if (featurep 'dbusbind)
      (let ((title "TMR May Ring (Emacs tmr package)")
            (body (tmr--long-description-for-finished-timer timer)))
        (notifications-notify
         :title title
         :body body
         :app-name "GNU Emacs"
         :urgency tmr-notification-urgency
         :sound-file tmr-sound-file))
    (warn "Emacs has no DBUS support, TMR notifications unavailable")))

(provide 'tmr-notification)
;;; tmr-notification.el ends here
