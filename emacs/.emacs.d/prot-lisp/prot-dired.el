;;; prot-dired.el --- Extensions to dired.el for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2020-2025  Protesilaos Stavrou

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
;; This covers my dired.el extensions, for use in my Emacs setup:
;; https://protesilaos.com/emacs/dotemacs.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

(require 'prot-common)
(require 'dired)
(require 'dired-aux)

(defgroup prot-dired ()
  "Extensions for Dired."
  :group 'dired)

;;;; Flat Dired listing

(defvar prot-dired-regexp-history nil
  "Minibuffer history of `prot-dired-regexp-prompt'.")

(defun prot-dired-regexp-prompt ()
  (let ((default (car prot-dired-regexp-history)))
    (read-regexp
     (format-prompt "Files matching REGEXP" default)
     default 'prot-dired-regexp-history)))

(defun prot-dired--get-files (regexp)
  "Return files matching REGEXP, recursively from `default-directory'."
  (directory-files-recursively default-directory regexp nil))

;;;###autoload
(defun prot-dired-search-flat-list (regexp)
  "Return a Dired buffer for files matching REGEXP.
Perform the search recursively from the current directory."
  (interactive (list (prot-dired-regexp-prompt)))
  (if-let* ((files (prot-dired--get-files regexp))
            (relative-paths (mapcar #'file-relative-name files)))
      (dired (cons (format "prot-flat-dired for `%s'" regexp) relative-paths))
    (error "No files matching `%s'" regexp)))

;;;; General commands

;; NOTE 2023-06-27: This user option is quick-and-dirty.  I prefer not
;; to have an option at all and simply do the right thing based on
;; `dired-guess-shell-alist-user'.
(defcustom prot-dired-always-external-regexp
  "\\(mkv\\|mp4\\|mp4\\|ogg\\|m4a\\|webm\\)"
  "Regular expression of file extensions to open externally.
The test is performed by `prot-dired-open-dwim', which then
defers to the `dired-guess-shell-alist-user'."
  :group 'prot-dired
  :type 'string)

;; NOTE 2023-06-27: This is a proof-of-concept.  See the previous
;; note.
(defun prot-dired-open-dwim (files)
  "Open FILES using the appropriate program."
  (interactive (list (dired-get-marked-files)))
  (if-let* ((extension (file-name-extension (car files)))
            ((string-match-p extension prot-dired-always-external-regexp))
            (guess (dired-guess-default files))
            (program (if (listp guess) (car guess) guess)))
      (dired-do-async-shell-command program nil files)
    (find-file (car files))))

(defvar prot-dired--limit-hist '()
  "Minibuffer history for `prot-dired-limit-regexp'.")

;;;###autoload
(defun prot-dired-limit-regexp (regexp omit)
  "Limit Dired to keep files matching REGEXP.

With optional OMIT argument as a prefix (\\[universal-argument]),
exclude files matching REGEXP.

Restore the buffer with \\<dired-mode-map>`\\[revert-buffer]'."
  (interactive
   (list
    (read-regexp
     (concat "Files "
             (when current-prefix-arg
               (propertize "NOT " 'face 'warning))
             "matching PATTERN: ")
     nil 'prot-dired--limit-hist)
    current-prefix-arg))
  (dired-mark-files-regexp regexp)
  (unless omit (dired-toggle-marks))
  (dired-do-kill-lines)
  (add-to-history 'prot-dired--limit-hist regexp))

(defvar prot-dired-grep-marked-files-history nil
  "Minibuffer history for `prot-dired-grep-marked-files-prompt'.")

(defun prot-dired-grep-marked-files-prompt ()
  "Prompt for string to search for with `prot-dired-grep-marked-files'."
  (read-string
   "grep for PATTERN in marked files: "
   nil 'prot-dired-grep-marked-files-history))

;; Also see `prot-search-grep' from prot-search.el.
;;;###autoload
(defun prot-dired-grep-marked-files (files regexp)
  "Run `find' with `grep' for REGEXP on marked FILES."
  (interactive
   (if-let* ((marks (dired-get-marked-files 'no-dir))
             (_ (> (length marks) 1)))
       (list
        marks
        (prot-dired-grep-marked-files-prompt))
     (user-error "Mark multiple files"))
   dired-mode)
  (let ((buffer-name (format "*prot-dired-grep-marked for `%s'*" regexp)))
    (compilation-start
     (concat
      "find . -not " (shell-quote-argument "(")
      " -wholename " (shell-quote-argument "*/.git*")
      " -prune " (shell-quote-argument ")")
      " -type f"
      " -exec grep -nHER --color=auto " regexp " "
      (shell-quote-argument "{}")
      " " (shell-quote-argument ";") " ")
     'grep-mode
     (lambda (_mode) buffer-name)
     t)))

;;;; Subdir extras and Imenu setup

(defvar prot-dired--directory-header-regexp "^ +\\(.+\\):\n"
  "Pattern to match Dired directory headings.")

;;;###autoload
(defun prot-dired-subdirectory-next (&optional arg)
  "Move to next or optional ARGth Dired subdirectory heading.
For more on such headings, read `dired-maybe-insert-subdir'."
  (interactive "p")
  (let ((pos (point))
        (subdir prot-dired--directory-header-regexp))
    (goto-char (line-end-position))
    (if (re-search-forward subdir nil t (or arg nil))
        (progn
          (goto-char (match-beginning 1))
          (goto-char (line-beginning-position)))
      (goto-char pos))))

;;;###autoload
(defun prot-dired-subdirectory-previous (&optional arg)
  "Move to previous or optional ARGth Dired subdirectory heading.
For more on such headings, read `dired-maybe-insert-subdir'."
  (interactive "p")
  (let ((pos (point))
        (subdir prot-dired--directory-header-regexp))
    (goto-char (line-beginning-position))
    (if (re-search-backward subdir nil t (or arg nil))
        (goto-char (line-beginning-position))
      (goto-char pos))))

(autoload 'dired-current-directory "dired")
(autoload 'dired-kill-subdir "dired-aux")

;;;###autoload
(defun prot-dired-remove-inserted-subdirs ()
  "Remove all inserted Dired subdirectories."
  (interactive)
  (goto-char (point-max))
  (while (and (prot-dired-subdirectory-previous)
              (not (equal (dired-current-directory)
                          (expand-file-name default-directory))))
    (dired-kill-subdir)))

(autoload 'cl-remove-if-not "cl-seq")

(defun prot-dired--dir-list (list)
  "Filter out non-directory file paths in LIST."
  (cl-remove-if-not
   (lambda (dir)
     (file-directory-p dir))
   list))

(defun prot-dired--insert-dir (dir &optional flags)
  "Insert DIR using optional FLAGS."
  (dired-maybe-insert-subdir (expand-file-name dir) (or flags nil)))

(autoload 'dired-get-filename "dired")
(autoload 'dired-get-marked-files "dired")
(autoload 'dired-maybe-insert-subdir "dired-aux")
(defvar dired-subdir-switches)
(defvar dired-actual-switches)

;;;###autoload
(defun prot-dired-insert-subdir (&optional arg)
  "Generic command to insert subdirectories in Dired buffers.

When items are marked, insert those which are subsirectories of
the current directory.  Ignore regular files.

If no files are active and point is on a subdirectory line,
insert it directly.

If no files are active and point is not on a subdirectory line,
prompt for a subdirectory using completion.

With optional ARG as a single prefix (`\\[universal-argument]')
argument, prompt for command line flags to pass to the underlying
ls program.

With optional ARG as a double prefix argument, remove all
inserted subdirectories."
  (interactive "p")
  (let* ((name (dired-get-marked-files))
         (flags (when (eq arg 4)
                  (read-string "Flags for `ls' listing: "
                               (or dired-subdir-switches dired-actual-switches)))))
    (cond  ; NOTE 2021-07-20: `length>', `length=' are from Emacs28
     ((eq arg 16)
      (prot-dired-remove-inserted-subdirs))
     ((and (length> name 1) (prot-dired--dir-list name))
      (mapc (lambda (file)
              (when (file-directory-p file)
                (prot-dired--insert-dir file flags)))
            name))
     ((and (length= name 1) (file-directory-p (car name)))
      (prot-dired--insert-dir (car name) flags))
     (t
      (let ((selection (read-directory-name "Insert directory: ")))
        (prot-dired--insert-dir selection flags))))))

(defun prot-dired--imenu-prev-index-position ()
  "Find the previous file in the buffer."
  (let ((subdir prot-dired--directory-header-regexp))
    (re-search-backward subdir nil t)))

(defun prot-dired--imenu-extract-index-name ()
  "Return the name of the file at point."
  (file-relative-name
   (buffer-substring-no-properties (+ (line-beginning-position) 2)
                                   (1- (line-end-position)))))

;;;###autoload
(defun prot-dired-setup-imenu ()
  "Configure imenu for the current Dired buffer.
Add this to `dired-mode-hook'."
  (set (make-local-variable 'imenu-prev-index-position-function)
       'prot-dired--imenu-prev-index-position)
  (set (make-local-variable 'imenu-extract-index-name-function)
       'prot-dired--imenu-extract-index-name))

(provide 'prot-dired)
;;; prot-dired.el ends here
