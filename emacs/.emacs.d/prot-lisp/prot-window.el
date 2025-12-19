;;; prot-window.el --- Display-buffer and window-related extensions for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2023-2025  Protesilaos Stavrou

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

;;; Code:

(require 'prot-common)

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
  (when-let* ((value (plist-get prot-window-window-sizes key)))
    (cond
     ((functionp value)
      (funcall value))
     ((numberp value)
      value)
     (t
      (error "The value of `%s' is neither a number nor a function" key)))))

(defun prot-window-select-fit-size (window)
  "Select WINDOW and resize it.
The resize pertains to the maximum and minimum values for height
and width, per `prot-window-window-sizes'.

Use this as the `body-function' in a `display-buffer-alist' entry."
  (select-window window)
  (fit-window-to-buffer
   window
   (prot-window--get-window-size :max-height)
   (prot-window--get-window-size :min-height)
   (prot-window--get-window-size :max-width)
   (prot-window--get-window-size :min-width))
  ;; If we did not use `display-buffer-below-selected', then we must
  ;; be in a lateral window, which has more space.  Then we do not
  ;; want to dedicate the window to this buffer, because we will be
  ;; running out of space.
  (when (or (window-in-direction 'above) (window-in-direction 'below))
    (set-window-dedicated-p window t)))

(defun prot-window--get-display-buffer-below-or-pop ()
  "Return list of functions for `prot-window-display-buffer-below-or-pop'."
  (list
   #'display-buffer-reuse-mode-window
   (if (or (prot-common-window-small-p)
           (prot-common-three-or-more-windows-p))
       #'display-buffer-below-selected
     #'display-buffer-pop-up-window)))

(defun prot-window-display-buffer-below-or-pop (&rest args)
  "Display buffer below current window or pop a new window.
The criterion for choosing to display the buffer below the
current one is a non-nil return value for
`prot-common-window-small-p'.

Apply ARGS expected by the underlying `display-buffer' functions.

This as the action function in a `display-buffer-alist' entry."
  (let ((functions (prot-window--get-display-buffer-below-or-pop)))
    (catch 'success
      (dolist (fn functions)
        (when (apply fn args)
          (throw 'success fn))))))

(defun prot-window-shell-or-term-p (buffer &rest _)
  "Check if BUFFER is a shell or terminal.
This is a predicate function for `buffer-match-p', intended for
use in `display-buffer-alist'."
  (when (string-match-p "\\*.*\\(e?shell\\|v?term\\).*" (buffer-name (get-buffer buffer)))
    (with-current-buffer buffer
      ;; REVIEW 2022-07-14: Is this robust?
      (and (not (derived-mode-p 'message-mode 'text-mode))
           (derived-mode-p 'eshell-mode 'shell-mode 'comint-mode 'fundamental-mode)))))

(defun prot-window-remove-dedicated (&rest _)
  "Remove dedicated window parameter.
Use this as :after advice to `delete-other-windows' and
`delete-window'."
  (when (one-window-p :no-mini)
    (set-window-dedicated-p nil nil)))

(mapc
 (lambda (fn)
   (advice-add fn :after #'prot-window-remove-dedicated))
 '(delete-other-windows delete-window))

(defmacro prot-window-define-full-frame (name &rest args)
  "Define command to call ARGS in new frame with `display-buffer-full-frame' bound.
Name the function prot-window- followed by NAME.  If ARGS is nil,
call NAME as a function."
  (declare (indent 1))
  `(defun ,(intern (format "prot-window-%s" name)) ()
     ,(format "Call `prot-window-%s' in accordance with `prot-window-define-full-frame'." name)
     (interactive)
     (let ((display-buffer-alist '((".*" (display-buffer-full-frame)))))
       (with-selected-frame (make-frame)
         ,(if args
              `(progn ,@args)
            `(funcall ',name))
         (modify-frame-parameters nil '((buffer-list . nil)))))))

(defun prot-window--get-shell-buffers ()
  "Return list of `shell' buffers."
  (seq-filter
   (lambda (buffer)
     (with-current-buffer buffer
       (derived-mode-p 'shell-mode)))
   (buffer-list)))

(defun prot-window--get-new-shell-buffer ()
  "Return buffer name for `shell' buffers."
  (if-let* ((buffers (prot-window--get-shell-buffers))
            (buffers-length (length buffers))
            ((>= buffers-length 1)))
      (format "*shell*<%s>" (1+ buffers-length))
    "*shell*"))

;;;###autoload (autoload 'prot-window-shell "prot-window")
(prot-window-define-full-frame shell
  (let ((name (prot-window--get-new-shell-buffer)))
    (shell name)
    (set-frame-name name)
    (when-let* ((buffer (get-buffer name)))
      (with-current-buffer buffer
        (add-hook
         'delete-frame-functions
         (lambda (_)
           ;; FIXME 2023-09-09: Works for multiple frames (per
           ;; `make-frame-command'), but not if the buffer is in two
           ;; windows in the same frame.
           (unless (> (safe-length (get-buffer-window-list buffer nil t)) 1)
             (let ((kill-buffer-query-functions nil))
               (kill-buffer buffer))))
         nil
         :local)))))

;;;###autoload (autoload 'prot-window-meeting "prot-window")
(prot-window-define-full-frame meeting
  (let ((buffer (get-buffer-create "*scratch for meeting*")))
    (with-current-buffer buffer
      (funcall initial-major-mode)
      (when (and (zerop (buffer-size))
                 (stringp initial-scratch-message))
        (insert initial-scratch-message)))
    (display-buffer buffer)
    (set-frame-name "Meeting")))

;; REVIEW 2023-06-25: Does this merit a user option?  I don't think I
;; will ever set it to the left.  It feels awkward there.
(defun prot-window-scroll-bar-placement ()
  "Control the placement of scroll bars."
  (when scroll-bar-mode
    (setq default-frame-scroll-bars 'right)
    (set-scroll-bar-mode 'right)))

(add-hook 'scroll-bar-mode-hook #'prot-window-scroll-bar-placement)

(defun prot-window-no-minibuffer-scroll-bar (frame)
  "Remove the minibuffer scroll bars from FRAME."
  (set-window-scroll-bars (minibuffer-window frame) nil nil nil nil :persistent))

(add-hook 'after-make-frame-functions 'prot-window-no-minibuffer-scroll-bar)

;;;; Run commands in a popup frame (via emacsclient)

(defun prot-window-delete-popup-frame (&rest _)
  "Kill selected selected frame if it has parameter `prot-window-popup-frame'.
Use this function via a hook."
  (dolist (frame (frame-list))
    (when (frame-parameter frame 'prot-window-popup-frame)
      (delete-frame frame))))

;; NOTE 2025-02-11: Also see `prot-vertico-with-buffer-mode', which
;; extends this to use a full-frame buffer for Vertico.
(defmacro prot-window-define-with-popup-frame (command)
  "Define function which calls COMMAND in a new frame.
Make the new frame have the `prot-window-popup-frame' parameter."
  `(defun ,(intern (format "prot-window-popup-%s" command)) ()
     ,(format "Run `%s' in a popup frame with `prot-window-popup-frame' parameter.
Also see `prot-window-delete-popup-frame'." command)
     (interactive)
     (let ((frame (make-frame '((prot-window-popup-frame . t)
                                (explicit-name . t)
                                (name . "prot-window-popup")))))
       (select-frame frame)
       (switch-to-buffer " prot-window-hidden-buffer-for-popup-frame")
       (condition-case nil
           (call-interactively ',command)
         ((quit error user-error)
          (delete-frame frame))))))

(declare-function org-capture "org-capture" (&optional goto keys))
(declare-function tmr "tmr" (time &optional description acknowledgep))
(declare-function prot-project-switch "prot-project" (directory))

;;;###autoload (autoload 'prot-window-popup-org-capture "prot-window")
(prot-window-define-with-popup-frame org-capture) ; defines command `prot-window-popup-org-capture'

;;;###autoload (autoload 'prot-window-popup-tmr "prot-window")
(prot-window-define-with-popup-frame tmr)  ; defines command `prot-window-popup-tmr'

;;;###autoload (autoload 'prot-window-popup-tmr "prot-window")
(prot-window-define-with-popup-frame prot-project-switch)  ; defines command `prot-window-popup-prot-project-switch'

(defun prot-window-set-delete-popup-hook (feature hook)
  "Set up `prot-window-delete-popup-frame' for FEATURE with HOOK."
  (with-eval-after-load feature
    (add-hook hook #'prot-window-delete-popup-frame)))

(defvar org-capture-after-finalize-hook)
(defvar tmr-timer-created-functions)
(defvar prot-project-switch-hook)

(prot-window-set-delete-popup-hook 'org-capture 'org-capture-after-finalize-hook)
(prot-window-set-delete-popup-hook 'tmr 'tmr-timer-created-functions)
(prot-window-set-delete-popup-hook 'prot-project 'prot-project-switch-hook)

(provide 'prot-window)
;;; prot-window.el ends here
