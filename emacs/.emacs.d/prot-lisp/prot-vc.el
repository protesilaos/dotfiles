;;; prot-vc.el --- Extensions to vc.el for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2021  Protesilaos Stavrou

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
;; This covers my vc.el extensions, which mostly concern Git.  For use
;; in my Emacs setup: https://protesilaos.com/dotemacs.
;;
;; Make sure to also inspect prot-project.el and prot-diff.el for a more
;; complete view of what I have on the topic of version control.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

(require 'vc)
(require 'log-edit)
(require 'prot-common)

;;;; Customisation options

(defgroup prot-vc ()
  "Extensions for vc.el and related libraries."
  :group 'project)

(defcustom prot-vc-log-limit 100
  "Limit commits in `prot-vc-custom-log' and others."
  :type 'integer
  :group 'prot-vc)

(defcustom prot-vc-log-bulk-action-limit 50
  "Limit for `prot-vc-log-view-toggle-entry-all'.
This is to ensure that performance does not take a hit.  The
default value is conservative."
  :type 'integer
  :group 'prot-vc)

(defcustom prot-vc-git-log-edit-show-commits nil
  "Show recent commits in Git Log Edit comments."
  :type 'boolean
  :group 'prot-vc)

(defcustom prot-vc-git-log-edit-show-commit-count 10
  "Commit number for `prot-vc-git-log-edit-show-commits'."
  :type 'integer
  :group 'prot-vc)

(defcustom prot-vc-shell-output "*prot-vc-output*"
  "Name of buffer for VC-related shell output."
  :type 'string
  :group 'prot-vc)

(defcustom prot-vc-patch-output-dirs (list "~/" "~/Desktop/")
  "List of directories to save `prot-vc-patch-dwim' output."
  :type 'list
  :group 'prot-vc)

;;;; Commands and helper functions

(defun prot-vc--current-project ()
  "Return root directory of current project."
  (or (vc-root-dir)
      (locate-dominating-file "." ".git")))

;;;###autoload
(defun prot-vc-project-or-dir (&optional arg)
  "Run `vc-dir' for the current project root.
With optional prefix ARG (\\[universal-argument]), use the
`default-directory' instead."
  (interactive "P")
  (let* ((root (prot-vc--current-project))
         (dir (if arg default-directory root)))
    (vc-dir dir)))

(defun prot-vc--log-edit-files-prompt ()
  "Helper completion for `prot-vc-extract-file-name'."
  (let ((files (log-edit-files)))
    (completing-read
     "Derive shortname from: " files nil nil)))

;;;###autoload
(defun prot-vc-git-log-edit-extract-file-name ()
  "Insert at point shortname from file in log edit buffers.
If multiple files are part of the log, a minibuffer completion
prompt will be produced: it can be used to narrow down to an
existing item or input an arbitrary string of characters."
  (interactive)
  (unless (derived-mode-p 'log-edit-mode)
    (user-error "Only try this in Log Edit mode"))
  (let* ((files (log-edit-files))
         (file (if (> (length files) 1)
                   (prot-vc--log-edit-files-prompt)
                 (car files)))
         (name (file-name-sans-extension
                (file-name-nondirectory
                 file))))
    (insert (concat name ": "))))

(autoload 'project-current "project")

(defvar prot-vc--log-insert-num-hist '()
  "History for `prot-vc-git-log-insert-commits'.")

(declare-function project-prompt-project-dir "project")

;;;###autoload
(defun prot-vc-git-log-insert-commits (&optional arg)
  "Insert at point number of commits starting from git HEAD.
If in a version-controlled directory, the commit log is based on
the root of the project, else a prompt for project selection is
produced with `project-current'.

With optional prefix ARG (\\[universal-argument]) always prompt
for a known project."
  (interactive "P")
  (let* ((dir (when arg (project-prompt-project-dir)))
         (default-directory (or dir
                                (prot-vc--current-project)
                                (cdr (project-current t))))
         (number (number-to-string
                 (read-number "Insert N commits from HEAD: " 5
                              'prot-vc--log-insert-num-hist))))
    (insert
     (with-temp-buffer
       (apply 'vc-git-command t nil nil
              (list "log" "--pretty=format:%h  %cs  %s" "-n" number "--"))
       (buffer-string)))
    (add-to-history 'prot-vc--log-insert-num-hist number)))

(autoload 'log-view-current-entry "log-view")
(autoload 'dired-get-marked-files "dired")

(defun prot-vc--commit-num ()
  "Determime whether `prot-vc-log-limit' is a positive integer."
  (let ((num prot-vc-log-limit))
    (if (and (integerp num)
             (> num 0))
        num
      (error "'%s' is not a valid number" num))))

;;;###autoload
(defun prot-vc-custom-log (&optional arg)
  "Like `vc-print-log' but for a custom fileset.

With optional prefix ARG (\\[universal-argument]), prompt for a
number to limit the log to.  Then prompt the user for matching
files in the `default-directory' with `completing-read-multiple'.
The default limit is controlled by the `prot-vc-log-limit'
variable.

In a `dired-mode' buffer, print log for the file at point, or any
marked files, except for when a double prefix argument is passed.
A single prefix arg still provides for a limit to the log.

With a double prefix ARG, prompt for a limit and produce a log
that covers all files in the present directory."
  (interactive "P")
  (let* ((lim (if arg
                  (read-number "Limit log to N entries: " 5)
                (prot-vc--commit-num)))
         (dir default-directory)
         (dotless directory-files-no-dot-files-regexp)
         (files (directory-files dir nil dotless t))
         (set (cond                     ; REVIEW: this is confusing
               ((equal arg '(16))
                files)
               ((eq major-mode 'dired-mode) ; REVIEW: any downside over `derived-mode-p'?
                (dired-get-marked-files t nil))
               (t
                (completing-read-multiple
                 "Select files in current dir: " files
                 #'prot-common-crm-exclude-selected-p t))))
         (backend (vc-backend set))
         (vc-log-short-style (if (> (length set) 1) '(file) '(directory))))
    (vc-print-log-internal backend set nil nil lim nil)))

(autoload 'log-view-msg-prev "log-view")
(autoload 'log-view-msg-next "log-view")
(autoload 'log-view-toggle-entry-display "log-view")

(defvar vc-git-root-log-format)

;;;###autoload
(defun prot-vc-log-view-toggle-entry-all ()
  "Run `log-view-toggle-entry-display' on all commits."
  (interactive)
  (let ((oldlines (count-lines (point-min) (point-max)))
        (point (point))
        (newlines)
        (commits (count-matches (nth 1 vc-git-root-log-format)
                                (point-min) (point-max)))
        (limit prot-vc-log-bulk-action-limit))
    (cond
     ((<= commits limit)
      (save-excursion
        (goto-char (point-max))
        (while (not (eq (line-number-at-pos) 1))
          (log-view-msg-prev)
          (log-view-toggle-entry-display))
        (goto-char point)
        (setq newlines (count-lines (point-min) (point-max))))
      (when (> newlines oldlines)
        (log-view-msg-next))
      (recenter))
     (t
      (user-error "%d commits here; won't expand more than %d" commits limit)))))

;;;###autoload
(defun prot-vc-log-kill-hash ()
  "Save to `kill-ring' contextual commit hash in `vc-print-log'."
  (interactive)
  (let ((commit (cadr (log-view-current-entry (point) t))))
    (kill-new (format "%s" commit))
    (message "Copied: %s" commit)))

(defvar prot-vc--commit-hist '()
  "Minibuffer history for commit logs.")

(defvar prot-vc--patch-output-hist '()
  "Minibuffer history for `prot-vc-patch-dwim' output.")

(defun prot-vc--log-commit-hash (fn)
  "Extract commit hash from FN.
FN is assumed to be something like `prot-vc--log-commit-prompt'."
  (string-match "· \\([a-z0-9]*\\) ·" fn)
  (match-string-no-properties 1 fn))

(defun prot-vc--log-commit-prompt (&optional prompt limit)
  "Select git log commit with completion.

Optional PROMPT pertains to the minibuffer's input field.  While
optional LIMIT will apply `prot-vc-log-limit' as a constraint,
instead of producing a complete log."
  (let ((text (or prompt "Select a commit: "))
        (vc (prot-vc--current-project))
        (num (cond
              ((integerp limit)
               (format "%d" limit))
              (limit
               (format "%d" (prot-vc--commit-num)))
              (t
               (format "%d" -1)))))
    (if vc
        (completing-read
         text
         (prot-common-completion-table
          'line
          (process-lines "git" "log" "--pretty=format:%d · %h · %cs %an: %s" "-n" num))
         nil t nil 'prot-vc--commit-hist)
      (error "'%s' is not under version control" default-directory))))

;;;###autoload
(defun prot-vc-git-patch-dwim (&optional arg)
  "Do-What-I-mean to output Git patches to a directory.

When the region is active inside of a Log View buffer, produce
patches for the commits within that range.  Remember how Git
interprets those ranges where the base commit is the one before
the earliest in the range: if you need to produce patches for the
topmost 4 commits, you must include the last 5 in the region.

With no active region, and while in a Log View buffer, a patch is
produced for the commit at point.

While not in a Log View buffer, prompt for a single commit to
produce a patch for.

Optional prefix ARG (\\[universal-argument]) prompts for a commit
using completion.  The selected item is used as the base of a
range against HEAD (in the format of 'base..HEAD').  When in Log
View buffers, and while no region is active, ARG will skip the
check for the commit at point in order to produce the prompt for
a base commit.  If the region is active in Log View buffers, ARG
is ignored.

Whatever the case, the list of completion candidates for commits
is always confined to `prot-vc-log-limit'."
  (interactive "P")
  (let* ((vc-dir (or (prot-vc--current-project)
                     default-directory))
         (dirs (append (list vc-dir) prot-vc-patch-output-dirs))
         (out-dir
          (completing-read
           "Output directory: "
           (prot-common-completion-table 'file dirs)
           nil t nil 'prot-vc--patch-output-hist))
         (buf (get-buffer-create prot-vc-shell-output)))
    (cond
     ((and (use-region-p) (derived-mode-p 'log-view-mode))
      (let* ((beg (region-beginning))
             (end (region-end))
             (one (cadr (log-view-current-entry beg t)))
             (two (cadr (log-view-current-entry end t)))
             (line-count (count-lines beg end))
             (range (if (> line-count 1)
                        (cond
                         ((> beg end)
                          (format "%s..%s" one two))
                         ((< beg end)
                          (format "%s..%s" two one)))
                     (format "-1 %s" (cadr (log-view-current-entry (point) t))))))
        (shell-command
         (format "git format-patch %s -o %s --" range out-dir) buf)
        (message "Prepared patch for `%s' and sent it to %s"
                 (propertize range 'face 'bold)
                 (propertize out-dir 'face 'success))))
     (arg
      (let ((base (prot-vc--log-commit-hash
                   (prot-vc--log-commit-prompt
                    "Select base commit for base..HEAD: " t))))
        (shell-command
         (format "git format-patch %s..HEAD -o %s --" base out-dir) buf)
        (message "Prepared patch for `%s..HEAD' and sent it to %s"
                 (propertize base 'face 'bold)
                 (propertize out-dir 'face 'success))))
     (t
      (let* ((commit-at-point (when (derived-mode-p 'log-view-mode)
                                (cadr (log-view-current-entry (point) t))))
             (commit (if (not commit-at-point)
                         (prot-vc--log-commit-hash
                          (prot-vc--log-commit-prompt
                           "Prepare patch for commit: " t))
                       commit-at-point)))
        (shell-command
         (format "git format-patch -1 %s -o %s --" commit out-dir) buf)
        (message "Prepared patch for `%s' and sent it to %s"
                 (propertize commit 'face 'bold)
                 (propertize out-dir 'face 'success))
        (add-to-history 'prot-vc--commit-hist commit)))
     (add-to-history 'prot-vc--patch-output-hist out-dir))))

;;;###autoload
(defun prot-vc-git-show (&optional limit)
  "Run git show for commit selected via completion.
With optional LIMIT as a prefix arg (\\[universal-argument]),
prompt for a number to confine the log to.  If LIMIT is a number,
accept it directly.  In the absence of LIMIT, `prot-vc-log-limit'
will be used instead."
  (interactive "P")
  (let* ((num (cond
               ((and limit (listp limit))
                (read-number "Limit to N commits: " 100))
               (limit
                (prefix-numeric-value limit))
               (t
                t)))
         (commit (prot-vc--log-commit-hash
                  (prot-vc--log-commit-prompt "Commit to git-show: " num)))
         (buf-name prot-vc-shell-output)
         (buf (get-buffer-create buf-name)))
    (shell-command (format "git show %s -u --stat -1 --" commit) buf)
    (with-current-buffer buf-name
      (setq-local revert-buffer-function nil)
      (diff-mode))
    (add-to-history 'prot-vc--commit-hist commit)))

(autoload 'vc-git-grep "vc-git")

;;;###autoload
(defun prot-vc-git-grep (regexp)
  "Run 'git grep' for REGEXP in current project.
This is a simple wrapper around `vc-git-grep' to streamline the
basic task of searching for a regexp in the current project.  Use
the original command for its other features."
  (interactive
   (list (read-regexp "git-grep for PATTERN: "
				      nil 'grep-history)))
  (vc-git-grep regexp "*" (prot-vc--current-project)))

(autoload 'vc-git-region-history-mode "vc-git")

;;;###autoload
(defun prot-vc-git-log-grep (pattern &optional diff)
  "Run ’git log --grep’ for PATTERN.
With optional DIFF as a prefix (\\[universal-argument])
argument, also show the corresponding diffs."
  (interactive
   (list (read-regexp "Run 'git log --grep' for PATTERN")
         current-prefix-arg))
   (let* ((buf-name prot-vc-shell-output)
          (buf (get-buffer-create buf-name))
          (diffs (if diff "-p" ""))
          (type (if diff 'with-diff 'log-search))
          (resize-mini-windows nil))
     (shell-command (format "git log %s --grep=%s -E --" diffs pattern) buf)
     (with-current-buffer buf
       (setq-local vc-log-view-type type)
       (setq-local revert-buffer-function nil)
       (vc-git-region-history-mode))))

(defun prot-vc-git--file-rev (file &optional limit)
  "Select revision for FILE using completion.
Optionally apply LIMIT to the log."
  (let ((num (cond
              ((integerp limit)
               (format "%d" limit))
              (limit
               (format "%d" (prot-vc--commit-num)))
              (t
               (format "%d" -1)))))
    (completing-read
     (format "Find revision for %s: " file)
     (prot-common-completion-table
      'line
      (process-lines "git" "log" "--pretty=format:%d · %h · %cs %an: %s" "-n" num "--" file))
     nil t nil 'prot-vc--commit-hist)))

;;;###autoload
(defun prot-vc-git-find-revision (&optional limit)
  "Visit a version of the current file using completion.
With optional LIMIT as a prefix arg (\\[universal-argument]),
prompt for a number to confine the log to.  If LIMIT is a number,
accept it directly.  In the absence of LIMIT, `prot-vc-log-limit'
will be used instead."
  (interactive "P")
  (let* ((num (cond
               ((and limit (listp limit))
                (read-number "Limit to N commits: " 100))
               (limit
                (prefix-numeric-value limit))
               (t
                t)))
         (rev (prot-vc--log-commit-hash
               (prot-vc-git--file-rev buffer-file-name num))))
    (switch-to-buffer-other-window
     (vc-find-revision buffer-file-name rev))
    (add-to-history 'prot-vc--commit-hist rev)))

(autoload 'vc-annotate-mode "vc-annotate")
(autoload 'vc-annotate-display-select "vc-annotate")

;;;###autoload
(defun prot-vc-git-blame-region-or-file (beg end)
  "Git blame lines in region between BEG and END or whole file."
  (interactive "r")
  (let* ((buf-name prot-vc-shell-output)
         (buf (get-buffer-create buf-name))
         (file (buffer-file-name))
         (backend (vc-backend file))
         (rev (vc-working-revision buffer-file-name))
         (e (if (region-active-p) beg (point-min)))
         (b (if (region-active-p) end (- (point-max) 1)))
         (beg-line (line-number-at-pos b t))
         (end-line (line-number-at-pos e t))
         (default-directory (prot-vc--current-project))
         (resize-mini-windows nil))
    (shell-command
     (format "git blame -L %d,%d -- %s" beg-line end-line file) buf)
    (with-current-buffer buf-name
      (unless (equal major-mode 'vc-annotate-mode)
        (setq-local revert-buffer-function nil)
        (vc-annotate-mode))
      (setq-local vc-annotate-backend backend)
      (setq-local vc-annotate-parent-file file)
      (setq-local vc-annotate-parent-rev rev)
      (setq-local vc-annotate-parent-display-mode 'scale)
      (vc-annotate-display-select buf 'fullscale))))

(autoload 'vc-refresh-state "vc-hooks")

;;;###autoload
(defun prot-vc-git-reset (&optional limit)
  "Select commit to 'git reset --soft' back to.
With optional LIMIT as a prefix arg (\\[universal-argument]),
prompt for a number to confine the log to.  If LIMIT is a number,
accept it directly.  In the absence of LIMIT, `prot-vc-log-limit'
will be used instead."
  (interactive "P")
  (let* ((num (cond
               ((and limit (listp limit))
                (read-number "Limit to N commits: " 50))
               (limit
                (prefix-numeric-value limit))
               (t
                t)))
         (commit (prot-vc--log-commit-hash
                  (prot-vc--log-commit-prompt "Run 'git reset --soft' on: " num)))
         (buf-name prot-vc-shell-output)
         (buf (get-buffer-create buf-name)))
    (when (yes-or-no-p (format "Run 'git reset --soft %s'?" commit))
      (shell-command (format "git reset --soft %s --quiet --" commit) buf)
      (vc-refresh-state))))

;;;###autoload
(defun prot-vc-git-log-reset (&optional hard)
  "Select commit in VC Git Log to 'git reset --soft' back to.
With optional prefix argument (\\[universal-argument]) for HARD,
pass the '--hard' flag instead."
  (interactive "P")
  (let* ((commit (cadr (log-view-current-entry (point) t)))
         (buf-name prot-vc-shell-output)
         (buf (get-buffer-create buf-name))
         (flag (if hard "--hard" "--soft")))
    (when (yes-or-no-p (format "Run 'git reset %s %s'?" flag commit))
      (shell-command (format "git reset %s %s --quiet --" flag commit) buf)
      (revert-buffer))))

;;;###autoload
(defun prot-vc-git-checkout-remote (remote)
  "Checkout new local branch tracking REMOTE (git checkout -b)."
  (interactive
   (list (completing-read
          "Select remote tracking branch: "
          (mapcar #'string-trim (process-lines "git" "branch" "-r"))
          nil t)))
  (let* ((name (split-string remote "\\(->\\|[\/]\\)" t "[\s\t]+"))
         (local (if (> (length name) 1)
                    (car (reverse name)) ; Better way than car reverse?
                  (car name))))
    (shell-command (format "git checkout -b %s %s" local remote))))

;;;; User Interface setup

;; This is a tweaked variant of `vc-git-expanded-log-entry'
(defun prot-vc-git-expanded-log-entry (revision)
  "Expand git commit message for REVISION."
  (with-temp-buffer
    (apply 'vc-git-command t nil nil (list "log" revision "--stat" "-1" "--"))
    (goto-char (point-min))
    (unless (eobp)
      (while (re-search-forward "^" nil t)
        (replace-match "  ")
        (forward-line))
      (concat "\n" (buffer-string)))))

(defun prot-vc-git-expand-function ()
  "Set `log-view-expanded-log-entry-function' for `vc-git'."
  (when (eq vc-log-view-type 'short)
    (setq-local log-view-expanded-log-entry-function
                #'prot-vc-git-expanded-log-entry)))

(defvar prot-vc-git-log-view-mode-hook nil
  "Hook that runs after `vc-git-log-view-mode'.")

(defun prot-vc-git-log-view-add-hook (&rest _)
  "Run `prot-vc-git-log-view-mode-hook'."
  (run-hooks 'prot-vc-git-log-view-mode-hook))

(declare-function log-edit-add-field "log-edit")

(defun prot-vc--format-git-comment (branch remote files &optional commits)
  "Add Git Log Edit comment with BRANCH, REMOTE, FILES, COMMITS."
  (let ((log (if commits (concat "\n# Recent commits:\n#\n" commits "\n#") "")))
    (concat
     "\n\n# ---\n# "
     "Files to be committed to branch " "`" branch "' tracking `" remote "':"
     "\n#\n" files "\n#" log
     "\n# All lines starting with `#' are ignored.")))

(defun prot-vc-git-log-edit-comment (&optional no-headers)
  "Append comment block to Git Log Edit buffer.
With optional NO-HEADERS skip the step of inserting the special
headers 'Amend' and 'Summary'."
  (let* ((branch-name (process-lines "git" "branch" "--show-current"))
         (branch (or (car branch-name) "Detached HEAD"))
         (remotes (process-lines "git" "branch" "-r"))
         (remote-name (if remotes
                        (cl-remove-if-not (lambda (s)
                                            (string-match-p "->" s))
                                          remotes)
                        "None"))
         (remote (if (and remote-name (listp remote-name))
                   (cadr (split-string (car remote-name) "->" t "[\s\t]+"))
                   "No Remote Found"))
         (files (mapconcat (lambda (x)
                             (concat "#   " x))
                           (log-edit-files)
                           "\n"))
         (commits (when (and prot-vc-git-log-edit-show-commits
                             (ignore-errors (process-lines "git" "log" "-1")))
                    (mapconcat (lambda (x)
                                 (concat "#   " x))
                               (process-lines
                                "git" "log" "--pretty=format:%h  %cs  %s"
                                (format "-n %d" prot-vc-git-log-edit-show-commit-count))
                               "\n"))))
    (unless no-headers
      (save-excursion
        (rfc822-goto-eoh)
        (unless (re-search-backward "Amend: .*" nil t)
          (log-edit-add-field "Amend" ""))
        (rfc822-goto-eoh)
        (unless (re-search-backward "Summary: .*" nil t)
          (log-edit-add-field "Summary" ""))))
    (goto-char (point-max))
    (insert "\n")
    (insert (prot-vc--format-git-comment branch remote files commits))
    (rfc822-goto-eoh)
    (when (looking-at "\n") (forward-char -1))))

;;;###autoload
(defun prot-vc-git-log-edit-previous-comment (arg)
  "Cycle backwards through comment history.
With a numeric prefix ARG, go back ARG comments."
  (interactive "*p")
  (let ((len (ring-length log-edit-comment-ring)))
    (if (<= len 0)
	    (progn (message "Empty comment ring") (ding))
      ;; Don't use `erase-buffer' because we don't want to `widen'.
      (delete-region (point-min) (point-max))
      (setq log-edit-comment-ring-index (log-edit-new-comment-index arg len))
      (message "Comment %d" (1+ log-edit-comment-ring-index))
      (insert (ring-ref log-edit-comment-ring log-edit-comment-ring-index))
      (prot-vc-git-log-edit-comment t)
      (save-excursion
        (goto-char (point-min))
        (search-forward "# ---")
        (forward-line -1)
        (delete-blank-lines)
        (newline 2)))))

;;;###autoload
(defun prot-vc-git-log-edit-next-comment (arg)
  "Cycle forwards through comment history.
With a numeric prefix ARG, go forward ARG comments."
  (interactive "*p")
  (prot-vc-git-log-edit-previous-comment (- arg)))

(defvar prot-vc--log-edit-comment-hist '()
  "History of inputs for `prot-vc-git-log-edit-complete-comment'.")

(defun prot-vc--log-edit-complete-prompt (comments)
  "Select entry from COMMENTS."
  (completing-read
   "Select comment: "
   comments nil t nil 'prot-vc--log-edit-comment-hist))

;;;###autoload
(defun prot-vc-git-log-edit-complete-comment ()
  "Insert text from Log Edit history ring using completion."
  (interactive)
  (let* ((newline (propertize "^J" 'face 'escape-glyph))
         (ring (ring-elements log-edit-comment-ring))
         (completions
          (mapcar (lambda (s)
                    (string-replace "\n" newline s))
                  ring))
         (selection (prot-vc--log-edit-complete-prompt completions))
         (comment (string-replace newline "\n" selection)))
    (add-to-history 'prot-vc--log-edit-comment-hist comment)
    (delete-region (point-min) (point-max))
    (insert comment)
    (prot-vc-git-log-edit-comment t)
    (save-excursion
      (goto-char (point-min))
      (search-forward "# ---")
      (forward-line -1)
      (delete-blank-lines)
      (newline 2))))

(defun prot-vc-git-log-remove-comment ()
  "Remove Git Log Edit comment, empty lines; keep final newline."
  (let ((buffer (get-buffer "*vc-log*"))) ; REVIEW: This is fragile
    (with-current-buffer (when (buffer-live-p buffer) buffer)
      (save-excursion
        (goto-char (point-min)))
      (when (derived-mode-p 'log-edit-mode)
        (flush-lines "^#")))))

;;;###autoload
(defun prot-vc-git-log-edit-toggle-amend ()
  "Toggle 'Amend' header for current Log Edit buffer.

Setting the header to 'yes' means that the current commit will
edit the previous one.

Unlike `vc-git-log-edit-toggle-amend', only change the state of
the 'Amend' header, without attempting to alter the contents of
the buffer."
  (interactive)
  (when (log-edit-toggle-header "Amend" "yes")))

(defun prot-vc--buffer-string-omit-comment ()
  "Remove Git comment and empty lines from buffer string."
  (let* ((buffer (get-buffer "*vc-log*"))
         (string (when buffer
                   (with-current-buffer buffer
                     (buffer-substring-no-properties (point-min) (point-max))))))
    (when string
      (replace-regexp-in-string "^#.*" "" string))))

(defvar log-edit-comment-ring)
(autoload 'ring-empty-p "ring")
(autoload 'ring-ref "ring")
(autoload 'ring-insert "ring")

(defun prot-vc-git-log-edit-remember-comment (&optional comment)
  "Store Log Edit text or optional COMMENT.
Remove special Git comment block before storing the genuine
commit message."
  (let ((commit (or comment (gensym))))
    (setq commit (prot-vc--buffer-string-omit-comment))
    (when (or (ring-empty-p log-edit-comment-ring)
              (not (equal commit (ring-ref log-edit-comment-ring 0))))
      (ring-insert log-edit-comment-ring commit))))

(declare-function log-edit-show-diff "log-edit")

(defvar prot-vc--current-window-configuration nil
  "Current window configuration for use with Log Edit.")

(defvar prot-vc--current-window-configuration-point nil
  "Point in current window configuration for use with Log Edit.")

(defun prot-vc--store-window-configuration ()
  "Store window configuration before calling `vc-start-logentry'.
This should be called via `prot-vc-git-pre-log-edit-hook'."
  (setq prot-vc--current-window-configuration (current-window-configuration))
  (setq prot-vc--current-window-configuration-point (point)))

(defvar prot-vc-git-pre-log-edit-hook nil
  "Hook that runs right before `vc-start-logentry'.")

(defun prot-vc-git-pre-log-edit (&rest _)
  "Run `prot-vc-git-pre-log-edit-hook'.
To be used as advice before `vc-start-logentry'."
  (run-hooks 'prot-vc-git-pre-log-edit-hook))

(defun prot-vc--log-edit-restore-window-configuration ()
  "Set window configuration to the pre Log Edit state."
  (when prot-vc--current-window-configuration
    (set-window-configuration prot-vc--current-window-configuration))
  (when prot-vc--current-window-configuration-point
    (goto-char prot-vc--current-window-configuration-point)))

(defun prot-vc--log-edit-diff-window-configuration ()
  "Show current diff for Git Log Edit buffer."
  (let ((buffer (get-buffer "*vc-log*")))
    (with-current-buffer (if (buffer-live-p buffer)
                             buffer
                           (window-buffer (get-mru-window)))
      (delete-other-windows)
      (when (ignore-errors ; This condition saves us from error on new repos
              (process-lines "git" "--no-pager" "diff-index" "-p" "HEAD" "--"))
        (log-edit-show-diff))
      (other-window -1))))

(defun prot-vc--kill-log-edit ()
  "Local hook to restore windows when Log Edit buffer is killed."
  (when (or (derived-mode-p 'log-edit-mode)
            (derived-mode-p 'diff-mode))
    (add-hook 'kill-buffer-hook #'prot-vc--log-edit-restore-window-configuration 0 t)))

(defvar prot-vc-git-log-edit-done-hook nil
  "Hook that runs after `prot-vc-git-log-edit-done'.")

;; FIXME: Why does `prot-vc-git-log-remove-comment' not work when added
;; to `log-edit-done-hook'?
;;;###autoload
(defun prot-vc-git-log-edit-done ()
  "Remove Git Log Edit comments and commit change set.
This is a thin wrapper around `log-edit-done', which first calls
`prot-vc-git-log-remove-comment'."
  (interactive)
  (prot-vc-git-log-remove-comment)
  (log-edit-done)
  (run-hooks 'prot-vc-git-log-edit-done-hook))

(defface prot-vc-git-log-edit-file-name
  '((default :inherit font-lock-comment-face)
    (((class color) (min-colors 88) (background light))
     :foreground "#2a486a")
    (((class color) (min-colors 88) (background dark))
     :foreground "#b0d6f5")
    (t :foreground "cyan"))
  "Face for file names in VC Git Log Edit buffers.")

(defface prot-vc-git-log-edit-local-branch-name
  '((default :inherit font-lock-comment-face)
    (((class color) (min-colors 88) (background light))
     :foreground "#0031a9")
    (((class color) (min-colors 88) (background dark))
     :foreground "#2fafff")
    (t :foreground "blue"))
  "Face for local branch name in VC Git Log Edit buffers.")

(defface prot-vc-git-log-edit-remote-branch-name
  '((default :inherit font-lock-comment-face)
    (((class color) (min-colors 88) (background light))
     :foreground "#55348e")
    (((class color) (min-colors 88) (background dark))
     :foreground "#cfa6ff")
    (t :foreground "magenta"))
  "Face for remote branch name in VC Git Log Edit buffers.")

(defconst prot-vc-git-log-edit-font-lock
  '(("^#.*"
     (0 'font-lock-comment-face))
    ("^#.*`\\(.+?\\)'.*`\\(.+?\\)'"
     (1 'prot-vc-git-log-edit-local-branch-name t)
     (2 'prot-vc-git-log-edit-remote-branch-name t))
    ("^#[\s\t][\s\t]+\\(.+\\)"
     (1 'prot-vc-git-log-edit-file-name t)))
  "Fontification rules for Log Edit buffers.")

(defun prot-vc-git-log-edit-extra-keywords ()
  "Apply `prot-vc-git-log-edit-font-lock' to Log Edit buffers."
  (font-lock-flush (point-min) (point-max))
  (font-lock-add-keywords nil prot-vc-git-log-edit-font-lock nil))

(autoload 'vc-git-log-view-mode "vc-git")
(autoload 'vc-git-checkin "vc-git")
(declare-function log-edit-show-files "log-edit")
(declare-function log-edit-kill-buffer "log-edit")
(declare-function log-edit-done "log-edit")
(declare-function log-edit-remember-comment "log-edit")
(declare-function vc-git-log-edit-toggle-amend "log-edit")
(defvar vc-git-log-edit-mode-map)

;;;###autoload
(define-minor-mode prot-vc-git-setup-mode
  "Extend `vc-git' Log View and Log Edit buffers.

Tweak the format of expanded commit messages in Log View buffers.  The
full information now includes a diff stat as well as all affected file
paths.  Those files can then be used for file-at-point operations like
`project-find-file'.

Make Log Edit window configurations split between the message
composition buffer and the corresponding diff view: the previous window
configuration is restored upon the successful conclusion of the commit
or its termination by means of `log-edit-kill-buffer'.

Append a comment block to the Log Edit buffer with information about the
files being committed and the branch they are a part of.  When
`prot-vc-git-log-edit-show-commits' is non-nil, also include a commit
log.  The number of commits in that log is controlled by
`prot-vc-git-log-edit-show-commit-count'.

For Log Edit buffers, bind C-c C-c to `prot-vc-git-log-edit-done' which
is designed to remove the comment block before checking in the changes.
Rebind other keys in the same vein.  `prot-vc-git-log-edit-done' calls
the normal hook `prot-vc-git-log-edit-done-hook' which is used to
restore the window layout.

Set up font-lock directives to make the aforementioned block look like a
comment in Log Edit buffers.  Also highlight file and branch names
inside the comment block."
  :init-value nil
  :global t
  (if prot-vc-git-setup-mode
      (progn
        ;; Log view expanded commits
        (advice-add #'vc-git-log-view-mode :after #'prot-vc-git-log-view-add-hook)
        (add-hook 'prot-vc-git-log-view-mode-hook #'prot-vc-git-expand-function)
        ;; Append comment block in Log edit showing branch and files.
        ;; This means that we no longer need the files' window to pop up
        ;; automatically
        (add-hook 'log-edit-hook #'prot-vc-git-log-edit-comment)
        (remove-hook 'log-edit-hook #'log-edit-show-files)
        ;; Window configuration with just the commit and the diff
        ;; (restores previous state after finalising or aborting the
        ;; commit).
        (advice-add #'vc-start-logentry :before #'prot-vc-git-pre-log-edit)
        (add-hook 'prot-vc-git-pre-log-edit-hook #'prot-vc--store-window-configuration)
        (advice-add #'log-edit-remember-comment :around #'prot-vc-git-log-edit-remember-comment)
        (let ((map vc-git-log-edit-mode-map))
          (define-key map (kbd "C-c C-c") #'prot-vc-git-log-edit-done)
          (define-key map (kbd "C-c C-e") #'prot-vc-git-log-edit-toggle-amend)
          (define-key map (kbd "M-p") #'prot-vc-git-log-edit-previous-comment)
          (define-key map (kbd "M-n") #'prot-vc-git-log-edit-next-comment)
          (define-key map (kbd "M-s") #'prot-vc-git-log-edit-complete-comment)
          (define-key map (kbd "M-r") #'prot-vc-git-log-edit-complete-comment))
        (add-hook 'log-edit-mode-hook #'prot-vc--kill-log-edit)
        (add-hook 'prot-vc-git-log-edit-done-hook #'prot-vc--log-edit-restore-window-configuration)
        (add-hook 'log-edit-hook #'prot-vc--log-edit-diff-window-configuration)
        ;; Extra font lock rules for Log Edit comment block
        (add-hook 'log-edit-hook #'prot-vc-git-log-edit-extra-keywords))
    (advice-remove #'vc-git-log-view-mode #'prot-vc-git-log-view-add-hook)
    (remove-hook 'prot-vc-git-log-view-mode-hook #'prot-vc-git-expand-function)
    (remove-hook 'log-edit-hook #'prot-vc-git-log-edit-comment)
    (add-hook 'log-edit-hook #'log-edit-show-files)
    (advice-remove #'vc-start-logentry #'prot-vc-git-pre-log-edit)
    (remove-hook 'prot-vc-git-pre-log-edit-hook #'prot-vc--store-window-configuration)
    (advice-remove #'log-edit-remember-comment #'prot-vc-git-log-edit-remember-comment)
    (let ((map vc-git-log-edit-mode-map))
      (define-key vc-git-log-edit-mode-map (kbd "C-c C-c") #'log-edit-done)
      (define-key vc-git-log-edit-mode-map (kbd "C-c C-e") #'vc-git-log-edit-toggle-amend)
      (define-key map (kbd "M-p") #'log-edit-previous-comment)
      (define-key map (kbd "M-n") #'log-edit-next-comment)
      (define-key map (kbd "M-s") #'log-edit-comment-search-forward)
      (define-key map (kbd "M-r") #'log-edit-comment-search-backward))
    (remove-hook 'log-edit-mode-hook #'prot-vc--kill-log-edit)
    (remove-hook 'prot-vc-git-log-edit-done-hook #'prot-vc--log-edit-restore-window-configuration)
    (remove-hook 'log-edit-hook #'prot-vc--log-edit-diff-window-configuration)
    (remove-hook 'log-edit-hook #'prot-vc-git-log-edit-extra-keywords)))

(provide 'prot-vc)
;;; prot-vc.el ends here
