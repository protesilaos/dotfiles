;;; prot-minibuffer.el --- Extensions for the minibuffer -*- lexical-binding: t -*-

;; Copyright (C) 2020  Protesilaos Stavrou

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

(defgroup prot-minibuffer ()
  "Minibuffer-related configurations for my dotemacs."
  :group 'minibuffer)

(defun prot-minibuffer-focus-mini ()
  "Focus the active minibuffer.

Bind this to `completion-list-mode-map' to easily jump between
the list of candidates present in the \\*Completions\\* buffer
and the minibuffer."
  (interactive)
  (let ((mini (active-minibuffer-window)))
    (when mini
      (select-window mini))))

(defun prot-minibuffer-focus-mini-or-completions ()
  "Focus the active minibuffer or the \\*Completions\\*.

If both the minibuffer and the Completions are present, this
command will first move per invocation to the former, then the
latter, and then continue to switch between the two.

The continuous switch is essentially the same as running
`prot-minibuffer-focus-minibuffer' and `switch-to-completions' in
succession."
  (interactive)
  (let* ((mini (active-minibuffer-window))
         (completions (or (get-buffer-window "*Completions*")
                          (get-buffer-window "*Embark Live Occur*"))))
    (cond ((and mini
                (not (minibufferp)))
           (select-window mini nil))
          ((and completions
                (not (eq (selected-window)
                         completions)))
           (select-window completions nil)))))

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
