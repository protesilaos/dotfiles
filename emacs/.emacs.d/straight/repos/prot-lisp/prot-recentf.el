;;; prot-recentf.el --- Extensions to recentf.el for my dotemacs -*- lexical-binding: t -*-

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
;; Extensions to `recentf.el' for my Emacs configuration:
;; <https://protesilaos.com/dotemacs/>.

;;; Code:

(require 'recentf)

;;;###autoload
(defun prot-recentf-keep-predicate (file)
  "Additional conditions for saving FILE in `recentf-list'.
Add this function to `recentf-keep'."
  (cond
   ((file-directory-p file) (file-readable-p file))))

(defvar prot-recentf--history-files '()
  "Minibuffer history for prot-recentf files.")

(defvar prot-recentf--history-dirs '()
  "Minibuffer history for prot-recentf directories.")

;;;###autoload
(defun prot-recentf-recent-files (&optional input)
  "Select item from `recentf-list' using completion.
Use INPUT as an initial, yet editable, filter.

The user's $HOME directory is abbreviated as a tilde."
  (interactive)
  (let* ((files (mapcar 'abbreviate-file-name recentf-list))
         (f (completing-read "Open recentf entry: " files nil t
                             (or input nil) 'prot-recentf--history-files)))
    (find-file f)
    (add-to-history 'prot-recentf--history-files f)))

(defun prot-recentf--dirs ()
  "Return list of directories in `recentf-list'."
  (let ((list (mapcar 'abbreviate-file-name recentf-list)))
    (delete-dups
     (mapcar (lambda (file)
               (if (file-directory-p file)
                   (directory-file-name file)
                 (substring (file-name-directory file) 0 -1)))
             list))))

;;;###autoload
(defun prot-recentf-recent-dirs (&optional arg)
  "Select directory from `recentf-list' using completion.
With optional prefix ARG (\\[universal-argument]) present the
list in a `dired' buffer.  This buffer is meant to be reused by
subsequent invocations of this command (otherwise you need to
remove the `when' expression.

Without \\[universal-argument], the user's $HOME directory is
abbreviated as a tilde.  In the Dired buffer paths are absolute."
  (interactive "P")
  (let* ((dirs (prot-recentf--dirs))
         (buf "*Recentf Dired*")
         (default-directory "~")
         (f (unless arg (completing-read
                         "Recent dirs: " dirs nil t nil
                         'prot-recentf--history-dirs))))
    (when (get-buffer buf)
      (kill-buffer buf))
    (if arg
        (dired (cons (generate-new-buffer-name buf) dirs))
      (find-file f)
      (add-to-history 'prot-recentf--history-dirs f))))

(provide 'prot-recentf)
;;; prot-recentf.el ends here
