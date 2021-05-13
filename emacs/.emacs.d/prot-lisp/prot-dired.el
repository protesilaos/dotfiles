;;; prot-dired.el --- Extensions to dired.el for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2020-2021  Protesilaos Stavrou

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos.com/dotemacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

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
;; This covers my dired.el extensions, for use in my Emacs setup:
;; https://protesilaos.com/dotemacs.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

(require 'prot-common)

(defvar prot-dired--directory-header-regexp "^ +\\(.+\\):\n"
  "Pattern to match Dired directory headings.")

;;;; Subdir extras

;;;###autoload
(defun prot-dired-subdirectory-next (&optional arg)
  "Move to next or optional ARGth Dired subdirectory heading.
For more on such headings, read `dired-maybe-insert-subdir'."
  (interactive "p")
  (let ((subdir prot-dired--directory-header-regexp))
    (goto-char (point-at-eol))
    (re-search-forward subdir nil t (or arg nil))
    (goto-char (match-beginning 1))
    (goto-char (point-at-bol))))

;;;###autoload
(defun prot-dired-subdirectory-previous (&optional arg)
  "Move to previous or optional ARGth Dired subdirectory heading.
For more on such headings, read `dired-maybe-insert-subdir'."
  (interactive "p")
  (let ((subdir prot-dired--directory-header-regexp))
    (goto-char (point-at-bol))
    (re-search-backward subdir nil t (or arg nil))
    (goto-char (point-at-bol))))

(autoload 'dired-get-filename "dired")
(autoload 'dired-maybe-insert-subdir "dired-aux")
(defvar dired-subdir-switches)
(defvar dired-actual-switches)

;;;###autoload
(defun prot-dired-insert-subdir (subdir &optional switches)
  "Insert SUBDIR in current Dired buffer.
With optional SWITCHES, prompt for the ls switches to use."
  (interactive
   (list
    (or (dired-get-filename 'verbatim t)
        (read-directory-name "Insert directory: "))
    (when current-prefix-arg
	  (read-string "Switches for listing: "
			       (or dired-subdir-switches dired-actual-switches)))))
  (dired-maybe-insert-subdir (expand-file-name subdir) (or switches nil) t))

;;;; Imenu setup

(defun prot-dired-imenu-prev-index-position ()
  "Find the previous file in the buffer."
  (let ((subdir prot-dired--directory-header-regexp))
    (re-search-backward subdir nil t)))

(defun prot-dired-imenu-extract-index-name ()
  "Return the name of the file at point."
  (buffer-substring-no-properties (+ (point-at-bol) 3) (1- (point-at-eol))))

(defun prot-dired-setup-imenu ()
  "Configure imenu for the current dired buffer.
Add this to `dired-mode-hook'."
  (set (make-local-variable 'imenu-prev-index-position-function)
       'prot-dired-imenu-prev-index-position)
  (set (make-local-variable 'imenu-extract-index-name-function)
       'prot-dired-imenu-extract-index-name))

(provide 'prot-dired)
;;; prot-dired.el ends here
