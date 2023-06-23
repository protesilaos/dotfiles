;;; prot-window.el --- Display-buffer and window-related extensions for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2023  Protesilaos Stavrou

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
;; This covers my window and display-buffer extensions, for use in my
;; Emacs setup: https://protesilaos.com/emacs/dotemacs.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

(defvar prot-window-window-sizes
  '( :max-height (lambda () (floor (frame-height) 3))
     :min-height 10
     :max-width (lambda () (floor (frame-width) 4))
     :min-width 20)
  "Property list of maximum and minimum window sizes.
The property keys are `:max-height', `:min-height', `:max-width',
and `:min-width'.  They all accept a value of either a
number (integer or floating point) or a function.")

(defun prot-window--get-window-size (key)
  "Extract the value of KEY from `prot-window-window-sizes'."
  (when-let ((value (plist-get prot-window-window-sizes key)))
    (cond
     ((functionp value)
      (funcall value))
     ((numberp value)
      value)
     (t
      (error "The value of `%s' is neither a number nor a function" key)))))

(defun prot-window-select-fit-size (window &rest _)
  "Select WINDOW and resize it.
The resize pertains to the maximum and minimum values for height
and width, per `prot-window-window-sizes'."
  (select-window window)
  (fit-window-to-buffer
   window
   (prot-window--get-window-size :max-height)
   (prot-window--get-window-size :min-height)
   (prot-window--get-window-size :max-width)
   (prot-window--get-window-size :max-width)))

(defun prot-window-shell-or-term-p (buffer &rest _)
  "Check if BUFFER is a shell or terminal.
This is a predicate function for `buffer-match-p', intended for
use in `display-buffer-alist'."
  (when (string-match-p "\\*.*\\(e?shell\\|v?term\\).*" (buffer-name (get-buffer buffer)))
    (with-current-buffer buffer
      ;; REVIEW 2022-07-14: Is this robust?
      (and (not (derived-mode-p 'message-mode 'text-mode))
           (derived-mode-p 'eshell-mode 'shell-mode 'comint-mode 'fundamental-mode)))))

(defmacro prot-window-with-full-frame (name &rest args)
  "Define function to evaluate ARGS with `display-buffer-full-frame' bound.

Name the function prot-window- followed by NAME.

If ARGS is nil, call NAME as a function."
  (declare (indent 1))
  `(defun ,(intern (format "prot-window-%s" name)) ()
     ,(format "Call `prot-window-%s' in accordance with `prot-window-with-full-frame'." name)
     (interactive)
     (let ((display-buffer-alist '((".*" (display-buffer-full-frame)))))
       ,(if args
            `(progn ,@args)
          `(funcall ',name)))))

(prot-window-with-full-frame shell)

(prot-window-with-full-frame coach
  (let ((buffer (get-buffer-create "*scratch for coach*")))
    (with-current-buffer buffer
      (funcall initial-major-mode))
    (display-buffer buffer)))

(provide 'prot-window)
;;; prot-window.el ends here
