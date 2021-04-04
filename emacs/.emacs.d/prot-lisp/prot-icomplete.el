;;; prot-icomplete.el --- Icomplete extensions for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2020-2021  Protesilaos Stavrou

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos.com/dotemacs
;; Version: 0.1.0
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
;; Icomplete extensions for use in my Emacs setup:
;; <https://protesilaos.com/dotemacs/>.  De facto DEPRECATED.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

(defgroup prot-icomplete ()
  "Icomplete-related configurations for my dotemacs."
  :group 'minibuffer)

(defun prot-icomplete-truncate ()
  "Truncate minibuffer lines when Icomplete is enabled.
This should only affect the horizontal layout and is meant to
enforce `icomplete-prospects-height' being set to 1.

Hook it to `icomplete-minibuffer-setup-hook'."
  (when (and (minibufferp) (bound-and-true-p icomplete-mode))
    (setq truncate-lines t)))           ; always becomes buffer-local

;;; Minibuffer actions

(defmacro prot-icomplete-act (name doc &rest body)
  "Define fun NAME with DOC and rest BODY."
  `(defun ,name ()
     ,doc
     (interactive)
     (let ((candidate (car completion-all-sorted-completions)))
       (when (and (minibufferp)
                  (bound-and-true-p icomplete-mode))
         ,@body))))

(prot-icomplete-act
 prot-icomplete-kill-completion
 "Place minibuffer candidate to the top of the `kill-ring'."
 (kill-new `,candidate)
 (message "Copied %s to kill-ring" (propertize `,candidate 'face 'success)))

(prot-icomplete-act
 prot-icomplete-insert-completion
 "Insert minibuffer candidate in last active window."
 (with-minibuffer-selected-window (insert `,candidate)))

(prot-icomplete-act
 prot-icomplete-insert-completion-exit
 "Like `prot-icomplete-insert-completion' but exit minibuffer."
 (with-minibuffer-selected-window (insert `,candidate))
 (top-level))

(defvar prot-icomplete-act-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-\\ w") 'prot-icomplete-kill-completion)
    (define-key map (kbd "M-\\ i") 'prot-icomplete-insert-completion)
    (define-key map (kbd "M-\\ j") 'prot-icomplete-insert-completion-exit)
    map))

;;;###autoload
(define-minor-mode prot-icomplete-actions
  "Common global actions for use with Icomplete.

\\{prot-icomplete-act-map}"
  :init-value nil
  :global t
  :keymap prot-icomplete-act-map)

(provide 'prot-icomplete)
;;; prot-icomplete.el ends here
