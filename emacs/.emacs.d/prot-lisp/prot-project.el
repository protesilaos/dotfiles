;;; prot-project.el --- Extensions to project.el for my dotemacs -*- lexical-binding: t -*-

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
;; This covers my project.el extensions, for use in my Emacs setup:
;; https://protesilaos.com/dotemacs.
;;
;; Make sure to also inspect prot-vc.el and prot-diff.el for a more
;; complete view of what I have on the topic of version control.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

(require 'cl-lib)
(require 'project)
(require 'prot-common)
(require 'vc)

(defgroup prot-project ()
  "Extensions for project.el and related libraries."
  :group 'project)

(defcustom prot-project-project-roots (list "~/Git/Projects/")
  "List of directories with version-controlled projects.
To be used by `prot-project-switch-project'."
  :type 'list
  :group 'prot-project)

(defcustom prot-project-commit-log-limit 25
  "Limit commit logs for project to N entries by default.
A value of 0 means 'unlimited'."
  :type 'integer
  :group 'prot-project)

(defcustom prot-project-large-file-lines 1000
  "How many lines constitute a 'large file' (integer).
This determines whether some automatic checks should be executed
or not, such as `prot-project-flymake-mode-activate'."
  :type 'integer
  :group 'prot-project)

;; Copied from Manuel Uberti:
;; <https://www.manueluberti.eu/emacs/2020/11/14/extending-project/>.
;;
;; Note that I prefer adding some dummy doc string over seeing spurious
;; compiler warnings.
(cl-defmethod project-root ((project (head local)))
  "Project root for PROJECT with HEAD and LOCAL."
  (cdr project))

;; Copied from Manuel Uberti and tweaked accordingly:
;; <https://www.manueluberti.eu/emacs/2020/11/14/extending-project/>.
(defun prot-project--project-files-in-directory (dir)
  "Use `fd' to list files in DIR."
  (unless (executable-find "fd")
    (error "Cannot find 'fd' command is shell environment $PATH"))
  (let* ((default-directory dir)
         (localdir (file-local-name (expand-file-name dir)))
         (command (format "fd -t f -0 . %s" localdir)))
    (project--remote-file-names
     (split-string (shell-command-to-string command) "\0" t))))

;; Copied from Manuel Uberti:
;; <https://www.manueluberti.eu/emacs/2020/11/14/extending-project/>.
;;
;; Same principle for the dummy doc string.
(cl-defmethod project-files ((project (head local)) &optional dirs)
  "Override `project-files' to use `fd' in local projects.

Project root for PROJECT with HEAD and LOCAL, plus optional
DIRS."
  (mapcan #'prot-project--project-files-in-directory
          (or dirs (list (project-root project)))))

(defun prot-project--list-projects ()
  "Produce list of projects in `prot-project-project-roots'."
  (let* ((dirs prot-project-project-roots)
         (dotless directory-files-no-dot-files-regexp)
         (cands (mapcan (lambda (d)
                          (directory-files d t dotless))
                        dirs)))
    (mapcar (lambda (d)
              (list (abbreviate-file-name d)))
            cands)))

;; FIXME: this is fragile since we do not store the original value of
;; `project--list' and may risk losing data.
;;;###autoload
(defun prot-project-add-projects ()
  "Append `prot-project--list-projects' to `project--list'."
  (interactive)
  (project--ensure-read-project-list)
  (let ((projects (prot-project--list-projects)))
    (setq project--list (append projects project--list))
    (project--write-project-list)))

;; TODO: use `completing-read-multiple' and learn how to delete a list
;; from an alist.
;;;###autoload
(defun prot-project-remove-project ()
  "Remove project from `project--list' using completion."
  (interactive)
  (project--ensure-read-project-list)
  (let* ((projects project--list)
         (dir (completing-read "REMOVE project from list: " projects nil t)))
    (setq project--list (delete (assoc dir projects) projects))
    (project--write-project-list)))

(defun prot-project--directory-subdirs (dir)
  "Return list of subdirectories in DIR."
  (cl-remove-if-not
   (lambda (x)
     (file-directory-p x))
   (directory-files-recursively dir ".*" t t)))

;; TODO: generalise this for all VC backends?  Which ones?
(defun prot-project--directory-subdirs-no-git (dir)
  "Remove .git dirs from DIR."
  (cl-remove-if
   (lambda (x)
     (string-match-p "\\.git" x))
   (prot-project--directory-subdirs dir)))

;; NOTE: in practice this is for `embark.el' (or equivalent
;; functionality), as it allows it to export the candidates in a Dired
;; buffer.
(defun prot-project--subdirs-completion-table (dir)
  "Return list of subdirectories in DIR with completion table."
  (prot-common-completion-table
   'file
   (prot-project--directory-subdirs-no-git dir)))

(defvar prot-project--subdir-hist '()
  "Minibuffer history for `prot-project-find-subdir'.")

;;;###autoload
(defun prot-project-find-subdir ()
  "Find subdirectories in the current project, using completion."
  (interactive)
  (let* ((pr (project-current t))
         (dir (cdr pr))
         (subdirs (prot-project--subdirs-completion-table dir))
         (directory (completing-read "Select Project subdir: " subdirs
                                     nil t nil 'prot-project--subdir-hist)))
    (dired directory)
    (add-to-history 'prot-project--subdir-hist dir)))

;; FIXME: the buttons at the bottom of the log for displaying more
;; commits do not seem to work with this.
;;;###autoload
(defun prot-project-commit-log (&optional arg)
  "Print commit log for the current project.
With optional prefix ARG (\\[universal-argument]) shows expanded
commit messages and corresponding diffs.

The log is limited to the integer specified by
`prot-project-commit-log-limit'.  A value of 0 means
'unlimited'."
  (interactive "P")
  (let* ((pr (project-current t))
         (dir (cdr pr))
         (default-directory dir) ; otherwise fails at spontaneous M-x calls
         (backend (vc-responsible-backend dir))
         (num prot-project-commit-log-limit)
         (int (prot-common-number-integer-p num))
         (limit (if (= int 0) t int))
         (diffs (if arg 'with-diff nil))
         (vc-log-short-style (unless diffs '(directory))))
    (vc-print-log-internal backend (list dir) nil nil limit diffs)))

;;;###autoload
(defun prot-project-retrieve-tag ()
  "Run `vc-retrieve-tag' on project and switch to the root dir.
Basically switches to a new branch or tag."
  (interactive)
  (let* ((pr (project-current t))
         (dir (cdr pr))
         (default-directory dir) ; otherwise fails at spontaneous M-x calls
         (name
          (vc-read-revision "Tag name: "
                            (list dir)
                            (vc-responsible-backend dir))))
    (vc-retrieve-tag dir name)
    (project-dired)))

(autoload 'magit-status "magit")

;;;###autoload
(defun prot-project-magit-status ()
  "Run `magit-status' on project."
  (interactive)
  (let* ((pr (project-current t))
         (dir (cdr pr)))
    (magit-status dir)))

(defun prot-project--max-line ()
  "Return the last line's number."
  (save-excursion
    (goto-char (point-max))
    (line-number-at-pos)))

(defun prot-project--large-file-p (&optional n)
  "Check if lines exceed `prot-project-large-file-lines'.
Optional N integer overrides that variable's value."
  (let* ((num (or n prot-project-large-file-lines))
         (int (prot-common-number-integer-p num)))
    (> (prot-project--max-line) int)))

;; Copied from Manuel Uberti, whom I had inspired with an earlier
;; version of this, and adapted accordingly:
;; <https://www.manueluberti.eu/emacs/2020/11/21/flymake-projects/>.
;;;###autoload
(defun prot-project-flymake-mode-activate ()
  "Activate Flymake only for `project-known-project-roots'."
  (project--ensure-read-project-list)
  (let ((known-projects (project-known-project-roots))
        (pr (or (vc-root-dir)
                (locate-dominating-file "." ".git")
                default-directory))
        (modes (prot-common-minor-modes-active)))
    (if (and (eq buffer-read-only nil)
             (member pr known-projects)
             (not (prot-project--large-file-p))
             (not (member 'org-src-mode modes))
             (not (eq buffer-file-truename nil)))
        (flymake-mode 1)
      (flymake-mode -1))))

(defvar org-src-mode-hook)

(add-hook 'org-src-mode-hook #'prot-project-flymake-mode-activate)
(add-hook 'prog-mode-hook #'prot-project-flymake-mode-activate)

(provide 'prot-project)
;;; prot-project.el ends here
