;;; prot-register.el --- Extensions for project.el -*- lexical-binding: t -*-

;; Copyright (C) 2025  Protesilaos Stavrou

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos.com/emacs/dotemacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))

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
;; Extensions for register.el.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

(require 'register)

(cl-defmethod register--type ((_regval vector)) 'vector)

(cl-defmethod register-val-describe ((val vector) _verbose)
  (if-let* ((pos (aref val 2))
            (file (aref val 1)))
      (princ (format "%s at position %s" file pos))
    (princ "Garbage data")))

;;;###autoload
(defun prot-simple-file-to-register (register)
  "Store current location of file's point in REGISTER."
  (interactive (list (register-read-with-preview "File with point to register: ")))
  (set-register register (vector 'file-with-point (buffer-file-name) (point))))

(defvar prot-simple-file-to-register-jump-hook nil
  "Normal hook called after jumping to a file register.
See `prot-simple-file-to-register'.")

;;;###autoload
(cl-defmethod register-val-jump-to ((val vector) delete)
  "Handle how to jump to a location register.
This is like the default, but does not ask to visit a file: it does it
outright."
  (cond
   ((eq (aref val 0) 'file-with-point)
    (find-file (aref val 1))
    (goto-char (aref val 2))
    (run-hooks 'prot-simple-file-to-register-jump-hook))
   (t (cl-call-next-method val delete))))

;;;###autoload
(defun prot-register-add-dwim (register &optional number)
  "Do-What-I-Mean with REGISTER.
If the region is active, call `copy-to-register'.

If the optional prefix argument NUMBER is non-nil, then call
`number-to-register'.

If there are more than two windows or there are more than one
`tab-bar-mode' tabs, then do `frameset-to-register'.

Otherwise, use `prot-simple-file-to-register'.

Also see `prot-register-use-dwim'."
  (interactive
   (list
    (register-read-with-preview "Add register: ")
    (when current-prefix-arg
      (prefix-numeric-value current-prefix-arg))))
  (cond
   ((when-let* ((_ (region-active-p))
                (beg (region-beginning))
                (end (region-end))
                (text (buffer-substring beg end))
                (_ (not (string-blank-p text))))
      (copy-to-register register beg end)
      (message "Copied `%s' to register `%c'" text register)))
   (number
    (number-to-register number register)
    (message "Copied number `%d' to register `%c'" number register))
   ((or (length> (window-list) 2)
        (and (bound-and-true-p tab-bar-mode)
             (length> (tab-bar-tabs) 1)))
    (frameset-to-register register)
    (message "Copied current frameset to register `%c'" register))
   (t
    (prot-simple-file-to-register register)
    (message "Copied current point in file to register `%c'" register))))

;;;###autoload
(defun prot-register-use-dwim (register)
  "Use the REGISTER, jumping or inserting it, depending on its ocntents."
  (interactive (list (register-read-with-preview "Use register: ")))
  (let ((contents (get-register register)))
    (cond
     ((register--jumpable-p contents)
      (jump-to-register register))
     ((or (register--insertable-p contents)
          (numberp register))
      (insert-register register))
     (t
      (error "The register is unknown: %S" contents)))))

(provide 'prot-register)
;;; prot-register.el ends here
