;;; cursory.el --- Manage cursor styles using presets -*- lexical-binding: t -*-

;; Copyright (C) 2022  Protesilaos Stavrou

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://git.sr.ht/~protesilaos/cursory
;; Mailing list: https://lists.sr.ht/~protesilaos/cursory
;; Version: 0.1.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, cursor

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
;; Cursory provides a thin wrapper around built-in variables that affect
;; the style of the Emacs cursor.  The intent is to allow the user to
;; define preset configurations such as "block with slow blinking" or
;; "bar with fast blinking" and set them on demand.
;;
;; Users can define their preferences in the option `cursory-presets'.
;; The command `cursory-set-preset' can then be used to select one such
;; style.  Selection uses minibuffer completion.
;;
;; The function `cursory-store-latest-preset' can be used to save the
;; last selected style in the `cursory-latest-state-file'.  The value
;; can then be restored with the `cursory-restore-latest-preset'
;; function.

;;; Code:

(defgroup cursory ()
  "Manage cursor styles using presets."
  :group 'cursor)

(defcustom cursory-presets
  '((bar
     :cursor-type (bar . 2)
     :cursor-in-non-selected-windows hollow
     :blink-cursor-blinks 10
     :blink-cursor-interval 0.5
     :blink-cursor-delay 0.2)
    (box
     :cursor-type box
     :cursor-in-non-selected-windows hollow
     :blink-cursor-blinks 10
     :blink-cursor-interval 0.5
     :blink-cursor-delay 0.2)
    (underscore
     :cursor-type (hbar . 3)
     :cursor-in-non-selected-windows hollow
     :blink-cursor-blinks 50
     :blink-cursor-interval 0.2
     :blink-cursor-delay 0.2))
  "Alist of preset configurations for `blink-cursor-mode'.

The car of each cons cell is an arbitrary, user-specified key
that broadly describes the set.

The cdr is a plist which specifies the cursor type and blink
properties.  In particular, it accepts the following properties:

    :cursor-type
    :cursor-in-non-selected-windows
    :blink-cursor-blinks
    :blink-cursor-interval
    :blink-cursor-delay

They correspond to built-in variables of the same name:
`cursor-type', `cursor-in-non-selected-windows',
`blink-cursor-blinks', `blink-cursor-interval',
`blink-cursor-delay'.  The value each of them accepts is the same
as the corresponding variable."
  :group 'cursory
  :type 'alist) ; FIXME 2022-04-15: Make this usable in the Custom UI

(defcustom cursory-latest-state-file
  (locate-user-emacs-file "cursory-latest-state.eld")
  "File to save the value of `cursory-set-preset'.
Saving is done by the `cursory-store-latest-preset' function."
  :type 'file
  :group 'cursory)

(defvar cursory--style-hist '()
  "Minibuffer history of `cursory--set-cursor-prompt'.")

(defun cursory--set-cursor-prompt ()
  "Promp for `cursory-presets' (used by `cursory-set-preset')."
  (let ((def (nth 1 cursory--style-hist)))
    (completing-read
     (format "Select cursor STYLE [%s]: " def)
     (mapcar #'car cursory-presets)
     nil t nil 'cursory--style-hist def)))

;;;###autoload
(defun cursory-set-preset (style &optional local)
  "Set cursor preset associated with STYLE.

STYLE is a symbol that represents the car of a list in
`cursory-presets'.

With optional LOCAL as a prefix argument, set the
`cursory-presets' only for the current buffer."
  (interactive (list (cursory--set-cursor-prompt) current-prefix-arg))
  (when-let* ((styles (if (stringp style) (intern style) style))
              (properties (alist-get styles cursory-presets))
              (type (plist-get properties :cursor-type))
              (type-no-select (plist-get properties :cursor-in-non-selected-windows))
              (blinks (plist-get properties :blink-cursor-blinks))
              (interval (plist-get properties :blink-cursor-interval))
              (delay (plist-get properties :blink-cursor-delay)))
    (if local
        (setq-local cursor-type type
                    cursor-in-non-selected-windows type-no-select
                    blink-cursor-blinks blinks
                    blink-cursor-interval interval
                    blink-cursor-delay delay)
      (dolist (var '( cursor-type cursor-in-non-selected-windows
                      blink-cursor-blinks blink-cursor-interval
                      blink-cursor-delay))
        (kill-local-variable var))
      (setq-default cursor-type type
                    cursor-in-non-selected-windows type-no-select
                    blink-cursor-blinks blinks
                    blink-cursor-interval interval
                    blink-cursor-delay delay))
    ;; We only want to save global values in
    ;; `cursory-store-latest-preset'.
    (unless local
      (add-to-history 'cursory--style-hist (format "%s" style)))))

;;;###autoload
(defun cursory-store-latest-preset ()
  "Write latest cursor state to `cursory-latest-state-file'.
Can be assigned to `kill-emacs-hook'."
  (when cursory--style-hist
    (with-temp-file cursory-latest-state-file
      (insert ";; Auto-generated file; don't edit -*- mode: "
	      (if (<= 28 emacs-major-version)
		  "lisp-data"
		"emacs-lisp")
	      " -*-\n"))
      (pp (intern (car cursory--style-hist)) (current-buffer))))

(defvar cursory-recovered-preset nil
  "Recovered value of latest store cursor preset.")

;;;###autoload
(defun cursory-restore-latest-preset ()
  "Restore latest cursor style."
  (when-let ((file cursory-latest-state-file))
    (when (file-exists-p file)
      (setq cursory-recovered-preset
            (unless (zerop
                     (or (file-attribute-size (file-attributes file))
                         0))
              (with-temp-buffer
                (insert-file-contents file)
                (read (current-buffer))))))))

(provide 'cursory)
;;; cursory.el ends here
