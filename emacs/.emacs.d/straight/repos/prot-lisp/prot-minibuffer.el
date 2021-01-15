;;; prot-minibuffer.el --- Extensions for the minibuffer -*- lexical-binding: t -*-

;; Copyright (C) 2020-2021  Protesilaos Stavrou

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
;; Extensions for the minibuffer, intended for my Emacs setup:
;; <https://protesilaos.com/dotemacs/>.

;;; Code:

;;;; General utilities

(defgroup prot-minibuffer ()
  "Extensions for the minibuffer."
  :group 'minibuffer)

(defcustom prot-minibuffer-completion-windows-regexp
  "\\*\\(Completions\\|Embark Collect \\(Live\\|Completions\\)\\)"
  "Regexp to match window names with completion candidates.
Used by `prot-minibuffer--get-completion-window'."
  :group 'prot-minibuffer
  :type 'string)

;;;; Minibuffer behaviour

;; Thanks to Omar Antolín Camarena for providing the messageless and
;; stealthily.  Source: <https://github.com/oantolin/emacs-config>.
(defun prot-minibuffer--messageless (fn &rest args)
  "Set `minibuffer-message-timeout' to 0.
Meant as advice for minibuffer completion FN with ARGS."
  (let ((minibuffer-message-timeout 0))
    (apply fn args)))

(dolist (fn '(minibuffer-force-complete-and-exit
              minibuffer-complete-and-exit
              exit-minibuffer))
  (advice-add fn :around #'prot-minibuffer--messageless))

;; Note that this solves bug#45686 and is only considered a temporary
;; measure: <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=45686>
(defun prot-minibuffer--stealthily (fn &rest args)
  "Prevent minibuffer default from counting as a modification.
Meant as advice for FN `minibuf-eldef-setup-minibuffer' with rest
ARGS."
  (let ((inhibit-modification-hooks t))
    (apply fn args)))

(advice-add 'minibuf-eldef-setup-minibuffer :around #'prot-minibuffer--stealthily)

;;;; Cursor appearance

(defun prot-minibuffer--cursor-type ()
  "Determine whether `cursor-type' is a list and return value.
If it is a list, this actually returns its car."
  (if (listp cursor-type)
      (car cursor-type)
    cursor-type))

;;;###autoload
(defun prot-minibuffer-mini-cursor ()
  "Local value of `cursor-type' for `minibuffer-setup-hook'."
  (pcase (prot-minibuffer--cursor-type)
    ('hbar (setq-local cursor-type '(hbar . 8)))
    ('bar (setq-local cursor-type '(hbar . 3)))
    (_  (setq-local cursor-type '(bar . 2)))))

;;;###autoload
(defun prot-minibuffer-completions-cursor ()
  "Local value of `cursor-type' for `completion-setup-hook'."
  (pcase (prot-minibuffer--cursor-type)
    ('hbar (setq-local cursor-type 'box))
    ('bar (setq-local cursor-type '(hbar . 8)))
    (_  (setq-local cursor-type '(bar . 3)))))

;;;; Minibuffer interactions

;;;###autoload
(defun prot-minibuffer-focus-mini ()
  "Focus the active minibuffer."
  (interactive)
  (let ((mini (active-minibuffer-window)))
    (when mini
      (select-window mini))))

(defun prot-minibuffer--get-completion-window ()
  "Find a live window showing completion candidates."
  (get-window-with-predicate
   (lambda (window)
     (string-match-p
      prot-minibuffer-completion-windows-regexp
      (format "%s" window)))))

(defun prot-minibuffer-focus-mini-or-completions ()
  "Focus the active minibuffer or the completions' window.

If both the minibuffer and the Completions are present, this
command will first move per invocation to the former, then the
latter, and then continue to switch between the two.

The continuous switch is essentially the same as running
`prot-minibuffer-focus-minibuffer' and `switch-to-completions' in
succession.

What constitutes a completions' window is ultimately determined
by `prot-minibuffer-completion-windows-regexp'."
  (interactive)
  (let* ((mini (active-minibuffer-window))
         (completions (prot-minibuffer--get-completion-window)))
    (cond ((and mini (not (minibufferp)))
           (select-window mini nil))
          ((and completions (not (eq (selected-window) completions)))
           (select-window completions nil)))))

;;;; M-X utility (M-x limited to buffer's major and minor modes)

;; UPDATE 2020-12-23: A better version of this is now part of Consult.
;; I am using that one instead, but keeping the code here:
;; <https://github.com/minad/consult>.

;; Adapted from the smex.el library of Cornelius Mika:
;; <https://github.com/nonsequitur/smex>.

(defun prot-minibuffer--extract-commands (mode)
  "Extract commands from MODE."
  (let ((commands)
        (library-path (symbol-file mode))
        (mode-name (substring (symbol-name major-mode) 0 -5)))
    (dolist (feature load-history)
      (let ((feature-path (car feature)))
        (when (and feature-path
                   (or (equal feature-path library-path)
                       (string-match mode-name (file-name-nondirectory
                                                feature-path))))
          (dolist (item (cdr feature))
            (when (and (listp item) (eq 'defun (car item)))
              (let ((function (cdr item)))
                (when (commandp function)
                  (setq commands (append commands (list function))))))))))
    commands))

(declare-function prot-common-minor-modes-active "prot-common")

(defun prot-minibuffer--extract-commands-minor ()
  "Extract commands from active minor modes."
  (let ((modes))
    (dolist (mode (prot-common-minor-modes-active))
      (push (prot-minibuffer--extract-commands mode) modes))
    modes))

(defun prot-minibuffer--commands ()
  "Merge and clean list of commands."
  (delete-dups
   (append (prot-minibuffer--extract-commands major-mode)
           (prot-minibuffer--extract-commands-minor))))

;;;###autoload
(defun prot-minibuffer-mode-commands ()
  "Run commands from current major mode and active minor modes."
  (interactive)
  (let ((commands (prot-minibuffer--commands)))
    (command-execute (intern (completing-read "M-X: " commands)))))

;;;; Simple actions for the "*Completions*" buffer

;; DEPRECATED: I just use Embark for such tasks, but am keeping this
;; around in case I ever need it.

(defun prot-minibuffer-completions-kill-save-symbol ()
  "Add `symbol-at-point' to the kill ring.

Intended for use in the \\*Completions\\* buffer.  Bind this to a
key in `completion-list-mode-map'."
  (interactive)
  (kill-new (thing-at-point 'symbol)))

(defmacro prot-minibuffer-completions-buffer-act (name doc &rest body)
  "Produce NAME function with DOC and rest BODY.
This is meant to define some basic commands for use in the
Completions' buffer."
  `(defun ,name ()
     ,doc
     (interactive)
     (let ((completions-window (get-buffer-window "*Completions*"))
           (completions-buffer (get-buffer "*Completions*"))
           (symbol (thing-at-point 'symbol)))
       (if (window-live-p completions-window)
           (with-current-buffer completions-buffer
             ,@body)
         (user-error "No live window with Completions")))))

(prot-minibuffer-completions-buffer-act
 prot-minibuffer-completions-kill-symbol-at-point
 "Append `symbol-at-point' to the `kill-ring'.
Intended to be used from inside the Completions' buffer."
 (kill-new `,symbol)
 (message "Copied %s to kill-ring"
          (propertize `,symbol 'face 'success)))

(prot-minibuffer-completions-buffer-act
 prot-minibuffer-completions-insert-symbol-at-point
 "Add `symbol-at-point' to last active window.
Intended to be used from inside the Completions' buffer."
 (let ((window (window-buffer (get-mru-window))))
   (with-current-buffer window
     (insert `,symbol)
     (message "Inserted %s"
              (propertize `,symbol 'face 'success)))))

(prot-minibuffer-completions-buffer-act
 prot-minibuffer-completions-insert-symbol-at-point-exit
 "Add `symbol-at-point' to last window and exit all minibuffers.
Intended to be used from inside the Completions' buffer."
 (let ((window (window-buffer (get-mru-window))))
   (with-current-buffer window
     (insert `,symbol)
     (message "Inserted %s"
              (propertize `,symbol 'face 'success))))
 (top-level))

(provide 'prot-minibuffer)
;;; prot-minibuffer.el ends here
