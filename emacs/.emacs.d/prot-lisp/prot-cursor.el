;;; prot-cursor.el --- Extensions for the cursor -*- lexical-binding: t -*-

;; Copyright (C) 2021  Protesilaos Stavrou

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos.com/dotemacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))

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
;; Extensions for the cursor, intended for my Emacs setup:
;; <https://protesilaos.com/dotemacs/>.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

(defgroup prot-cursor ()
  "Tweaks for cursor appearance."
  :group 'cursor)

(defcustom prot-cursor-presets
  '((bar . ( :cursor-type (bar . 2)
             :cursor-no-selected hollow
             :blinks 10
             :blink-interval 0.5
             :blink-delay 0.2))

    (box  . ( :cursor-type box
              :cursor-no-selected hollow
              :blinks 10
              :blink-interval 0.5
              :blink-delay 0.2))

    (underscore . ( :cursor-type (hbar . 3)
                    :cursor-no-selected hollow
                    :blinks 50
                    :blink-interval 0.2
                    :blink-delay 0.2)))
  "Alist of desired typeface properties for Blink-cursor-mode.

The car of each cons cell is an arbitrary key that broadly
describes the display type.

The cdr is a plist that specifies the cursor type and blink
properties."
  :group 'prot-cursor
  :type 'alist)

(defcustom prot-cursor-last-state-file
  (locate-user-emacs-file "prot-cursor-last-state")
  "File to save the value of `prot-cursor-set-cursor'."
  :type 'file
  :group 'prot-cursor)

(defvar prot-cursor--style-hist '()
  "History of inputs for display-related font associations.")

(defun prot-cursor--set-cursor-prompt ()
  "Promp for font set (used by `prot-cursor-set-cursor')."
  (let ((def (nth 1 prot-cursor--style-hist)))
    (completing-read
     (format "Select cursor STYLE [%s]: " def)
     (mapcar #'car prot-cursor-presets)
     nil t nil 'prot-cursor--style-hist def)))

;;;###autoload
(defun prot-cursor-set-cursor (style)
  "Set cursor preset associated with STYLE.

STYLE is a symbol that represents the car of a cons cell in
`prot-cursor-presets'."
  (interactive (list (prot-cursor--set-cursor-prompt)))
  (when (or (eq style t) (null style))
    (setq style 'box))
  (let* ((styles (if (stringp style) (intern style) style))
         (properties (alist-get styles prot-cursor-presets))
         (type (plist-get properties :cursor-type))
         (type-no-select (plist-get properties :cursor-no-selected))
         (blinks (plist-get properties :blinks))
         (blink-interval (plist-get properties :blink-interval))
         (blink-delay (plist-get properties :blink-delay)))
    (setq-default cursor-type type
                  cursor-in-non-selected-windows type-no-select
                  blink-cursor-blinks blinks
                  blink-cursor-interval blink-interval
                  blink-cursor-delay blink-delay)
    (add-to-history 'prot-cursor--style-hist (format "%s" style))))

(defun prot-cursor-store-last-preset ()
  "Write latest cursor state to `prot-cursor-last-state-file'.
Can be assigned to `kill-emacs-hook'."
  (when prot-cursor--style-hist
    (with-temp-file prot-cursor-last-state-file
      (insert (concat ";; Auto-generated file;"
                      " don't edit -*- mode: lisp-data -*-\n"))
      (pp (intern (car prot-cursor--style-hist)) (current-buffer)))))

(defvar prot-cursor--recovered-preset nil
  "Recovered value of last store cursor preset.")

(defun prot-cursor-restore-last-preset ()
  "Restore last cursor style."
  (when-let ((file prot-cursor-last-state-file))
    (when (file-exists-p file)
      (setq prot-cursor--recovered-preset
            (unless (zerop
                     (or (file-attribute-size (file-attributes file))
                         0))
              (with-temp-buffer
                (insert-file-contents file)
                (read (current-buffer))))))))

(provide 'prot-cursor)
;;; prot-cursor.el ends here
