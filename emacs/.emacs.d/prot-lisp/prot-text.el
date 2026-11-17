;;; prot-text.el --- Custom text-mode extras -*- lexical-binding: t -*-

;; Copyright (C) 2023-2026  Protesilaos

;; Author: Protesilaos <info@protesilaos.com>
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

;; Extras for `text-mode'.

;;; Code:

(defun prot-text-insert-prefix (prefix string)
  "Prepend PREFIX to every line of STRING."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (insert prefix)
    (while (and (forward-line 1)
                (not (eobp)))
      (goto-char (line-beginning-position))
      (insert prefix))
    (buffer-string)))

(defmacro prot-text-define-prefix-command (scope prefix)
  "Define command with SCOPE to insert PREFIX to string.
SCOPE is a string that describes what the command will do.  PREFIX is
the prefix as a string.

The command will target the text between `region-beginning' and
`region-end'.  It will produce and error if the region is not active."
  `(defun ,(intern (format "prot-text-prefix-%s" scope)) (beginning end)
     ,(format
       "Add prefix `%s' to text between BEGINNING and END positions.
Report an error if the region is not active."
       prefix)
     (interactive
      (if (use-region-p)
          (list (region-beginning) (region-end))
        (error "The region is not active")))
     (when (= beginning end)
       (error "The BEGINNING and END cannot be equal"))
     (let* ((text (buffer-substring beginning end))
            (replacement (prot-text-insert-prefix ,prefix text)))
       (replace-region-contents beginning end replacement))))

;;;###autoload (autoload 'prot-text-prefix-blockquote "prot-text" nil t)
;;;###autoload (autoload 'prot-text-prefix-indent "prot-text" nil t)
(prot-text-define-prefix-command blockquote "> ")
(prot-text-define-prefix-command indent "    ")

(defvar-keymap prot-text-prefix-leader-map
  :doc "Commands to insert prefix to the active region text."
  :prefix 'prot-text-prefix-leader
  "b" #'prot-text-prefix-blockquote
  "i" #'prot-text-prefix-indent)

(provide 'prot-text)
;;; prot-text.el ends here
