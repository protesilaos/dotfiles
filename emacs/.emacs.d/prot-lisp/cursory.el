;;; cursory.el --- Manage cursor styles using presets -*- lexical-binding: t -*-

;; Copyright (C) 2022  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Cursory Development <~protesilaos/cursory@lists.sr.ht>
;; URL: https://git.sr.ht/~protesilaos/cursory
;; Version: 0.1.4
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
;; the style of the Emacs cursor on graphical terminals.  The intent is to
;; allow the user to define preset configurations such as "block with slow
;; blinking" or "bar with fast blinking" and set them on demand.
;;
;; The user option `cursory-presets' holds the presets.  The command
;; `cursory-set-preset' is used to select one among them.  Selection
;; supports minibuffer completion when there are multiple presets, else
;; sets the single preset outright.
;;
;; Presets consist of a list of properties that govern the cursor type in
;; the active and inactive windows, as well as cursor blinking variables.
;; They look like this:
;;
;;     (bar
;;      :cursor-type (bar . 2)
;;      :cursor-in-non-selected-windows hollow
;;      :blink-cursor-blinks 10
;;      :blink-cursor-interval 0.5
;;      :blink-cursor-delay 0.2)
;;
;; The car of the list is an arbitrary, user-defined symbol that identifies
;; (and can describe) the set.  Each of the properties corresponds to
;; built-in variables: `cursor-type', `cursor-in-non-selected-windows',
;; `blink-cursor-blinks', `blink-cursor-interval', `blink-cursor-delay'.
;; The value each property accepts is the same as the variable it
;; references.
;;
;; When called from Lisp, the `cursory-set-preset' command requires a
;; PRESET argument, such as:
;;
;;     (cursory-set-preset 'bar)
;;
;; The default behaviour of `cursory-set-preset' is to change cursors
;; globally.  The user can, however, limit the effect to the current
;; buffer.  With interactive use, this is done by invoking the command with
;; a universal prefix argument (`C-u' by default).  When called from Lisp,
;; the LOCAL argument must be non-nil.
;;
;; The function `cursory-store-latest-preset' is used to save the last
;; selected style in the `cursory-latest-state-file'.  The value can then
;; be restored with the `cursory-restore-latest-preset' function.

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

They correspond to built-in variables: `cursor-type',
`cursor-in-non-selected-windows', `blink-cursor-blinks',
`blink-cursor-interval', `blink-cursor-delay'.  The value each of
them accepts is the same as the variable it references."
  :group 'cursory
  :type `(alist
          :value-type
          (plist :options
                 (((const :tag "Cursor type"
                          :cursor-type)
                   ,(get 'cursor-type 'custom-type))
                  ((const :tag "Cursor in non-selected windows"
                          :cursor-in-non-selected-windows)
                   ,(get 'cursor-in-non-selected-windows 'custom-type))
                  ((const :tag "Number of blinks"
                          :blink-cursor-blinks)
                   ,(get 'blink-cursor-blinks 'custom-type))
                  ((const :tag "Blink interval"
                          :blink-cursor-interval)
                   ,(get 'blink-cursor-interval 'custom-type))
                  ((const :tag "Blink delay"
                          :blink-cursor-delay)
                   ,(get 'blink-cursor-delay 'custom-type))))
          :key-type symbol))

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
  (interactive
   (list
    (if (= (length cursory-presets) 1)
        (caar cursory-presets)
      (cursory--set-cursor-prompt))
    current-prefix-arg))
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
  (when-let ((hist cursory--style-hist))
    (with-temp-file cursory-latest-state-file
      (insert ";; Auto-generated file; don't edit -*- mode: "
              (if (<= 28 emacs-major-version)
                  "lisp-data"
                "emacs-lisp")
              " -*-\n")
      (pp (intern (car hist)) (current-buffer)))))

(defvar cursory-recovered-preset nil
  "Recovered value of latest store cursor preset.")

;;;###autoload
(defun cursory-restore-latest-preset ()
  "Restore latest cursor style."
  (when-let* ((file cursory-latest-state-file)
              ((file-exists-p file)))
    (setq cursory-recovered-preset
          (unless (zerop
                   (or (file-attribute-size (file-attributes file))
                       0))
            (with-temp-buffer
              (insert-file-contents file)
              (read (current-buffer)))))))

(provide 'cursory)
;;; cursory.el ends here
