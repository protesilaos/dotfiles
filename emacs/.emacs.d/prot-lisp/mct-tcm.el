;;; mct-tcm.el --- MCT which Treats the Completions as the Minibuffer -*- lexical-binding: t -*-

;; Copyright (C) 2022  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://gitlab.com/protesilaos/mct
;; Version: 0.5.0
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
;; MCT extension which Treats the Completions as the Minibuffer.  It
;; intercepts any single character input (without Control or Alt
;; modifiers) in the Completions' buffer and passes it to the minibuffer
;; as input.  This practically means that the user can (i) narrow the
;; list of candidates from the minibuffer, (ii) switch to the
;; Completions in anticipation of selecting a candidate, (iii) change
;; their mind and opt to narrow further, (iv) type something to bring
;; focus back to the minibuffer while narrowing to the given input.
;;
;; When the `mct-tcm-mode' is enabled and the above sequence of events
;; takes place, the current session is treated as if it belongs to the
;; `mct-completion-passlist' (read its doc string).

;;; Code:

;;;; General utilities

(require 'mct)
(require 'seq)
(eval-when-compile (require 'subr-x))

;; FIXME 2022-02-22: Silence message when key binding is undefined.
;;;###autoload
(define-minor-mode mct-tcm-mode
  "MCT extension which Treats the Completions as the Minibuffer.
It intercepts any single character input (without Control or Alt
modifiers) in the Completions' buffer and passes it to the
minibuffer as input.  This practically means that the user
can (i) narrow the list of candidates from the minibuffer, (ii)
switch to the Completions in anticipation of selecting a
candidate, (iii) change their mind and opt to narrow
further, (iv) type something to bring focus back to the
minibuffer while narrowing to the given input.

When this mode is enabled and the above sequence of events takes
place, the current session is treated as if it belongs to the
`mct-completion-passlist' (read its doc string)."
  :global t
  :group 'mct
  (if mct-tcm-mode
      (add-hook 'completion-list-mode-hook #'mct-tcm--setup-redirect-self-insert)
    (remove-hook 'completion-list-mode-hook #'mct-tcm--setup-redirect-self-insert)))

;; NOTE 2022-02-25: This assumes that `kill-word' is bound to those
;; keys.  Is there a more general method?
(defun mct-tcm--kill-word-p (keys)
  "Return non-nil if KEYS are C-DEL or M-DEL."
  (or (seq-contains-p keys 'C-backspace)
      (seq-contains-p keys '134217855)))

(defun mct-tcm--insertable-char-p (char)
  "Retun non-nil if CHAR can be used with `insert'."
  (and (char-or-string-p char)
       (or (memq 'shift (event-modifiers char))
           (not (event-modifiers char)))))

(defun mct-tcm--redirect-self-insert (&rest _)
  "Redirect single character keys as input to the minibuffer."
  (when-let* ((mct-tcm-mode)
              (keys (this-single-command-keys))
              (char (aref keys 0))
              (mini (active-minibuffer-window)))
    (let* ((delete-char (seq-contains-p keys '127)) ; Same assumption as `mct-tcm--kill-word-p'
           (kill-word (mct-tcm--kill-word-p keys))
           (del (or delete-char kill-word)))
      (when (or del (mct-tcm--insertable-char-p char))
        (select-window mini)
        (setq-local completion-at-point-functions nil
                    mct-live-completion t
                    mct-live-update-delay 0
                    mct-minimum-input 0)
        (goto-char (point-max))
        (let ((empty (string-empty-p (minibuffer-contents))))
          (cond
           (kill-word
            (when empty (push-mark))
            (kill-word -1))
           (delete-char
            (unless empty
              (delete-char -1)))
           ;; FIXME 2022-02-25: Emacs 27 inserts twice.  It inserts once
           ;; even without the `insert' here.
           (t
            (insert char))))))))

(defun mct-tcm--setup-redirect-self-insert ()
  "Set up `mct-tcm--redirect-self-insert'."
  (when (mct--minibuffer-p)
    (add-hook 'pre-command-hook #'mct-tcm--redirect-self-insert nil t)))

(provide 'mct-tcm)
;;; mct-tcm.el ends here
