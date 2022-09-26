;;; prot-vc.el --- Extensions to vc.el for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022  Protesilaos Stavrou

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos.com/emacs/dotemacs
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
;; in my Emacs setup: https://protesilaos.com/emacs/dotemacs.
;;
;; Make sure to also inspect prot-diff.el for a more complete view of
;; what I have on the topic of version control.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.



;; NOTE 2022-01-05: I plan to rewrite the whole file to bring it up to
;; my current standard (don't forget what the Commentary of every such
;; files explains: I do all this for educational or recreational
;; purposes).  The overall functionality will stay in place.


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

(defcustom prot-vc-git-patch-apply-args (list "--3way")
  "List of strings to pass as arguments to 'git am'."
  :type '(repeat string)
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
(defun prot-vc-git-patch-apply (patch project &optional args)
  "Apply PATCH to current project using 'git am'.

PROJECT is a path to the root of a git repo, automatically
defaulting to the current project.  If none is found or if this
command is called with a prefix argument (\\[universal-argument])
prompt for a project instead.

When called non-interactively, ARGS is a list of strings with
command line flags for 'git am'.  Otherwise it takes the value of
`prot-vc-git-patch-apply-args'."
  (interactive
   (list
    (read-file-name "Path to patch: ")
    (when (or current-prefix-arg
              (null (prot-vc--current-project)))
      (project-prompt-project-dir))))
  ;; FIXME 2021-06-30: Avoid calling `prot-vc--current-project' twice
  (let* ((default-directory (or project (prot-vc--current-project)))
         (buf-name prot-vc-shell-output)
         (buf (get-buffer-create buf-name))
         (resize-mini-windows nil)
         (arguments (or args prot-vc-git-patch-apply-args))
         (arg-string (mapconcat #'identity arguments " ")))
    (shell-command (format "git am %s %s" arg-string patch) buf)))

;;;###autoload
(defun prot-vc-git-patch-create-dwim (&optional arg)
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
      (vc-git-region-history-mode)
      (setq-local log-view-vc-backend 'git))))

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

;; XXX NOTE XXX 2021-07-31: Those are meant to enable `revert-buffer'
;; inside of `prot-vc-git-blame-region-or-file'.  I do not know whether
;; this is a good approach.  It seems very convoluted and fragile.  But
;; anyway, I tried and it seems to work.
(defvar prot-vc--blame-beg nil)
(defvar prot-vc--blame-end nil)
(defvar prot-vc--blame-file nil)
(defvar prot-vc--blame-origin nil)

;;;###autoload
(defun prot-vc-git-blame-region-or-file (beg end &optional file)
  "Git blame lines in region between BEG and END.
Optionally specify FILE, else default to the current one."
  (interactive "r")
  (let* ((buf-name prot-vc-shell-output)
         (buf (get-buffer-create buf-name))
         (f (or file buffer-file-name))
         (backend (vc-backend f))
         (rev (vc-working-revision f))
         (e (if (region-active-p) beg (point-min)))
         (b (if (region-active-p) end (- (point-max) 1)))
         (beg-line (line-number-at-pos b t))
         (end-line (line-number-at-pos e t))
         (default-directory (prot-vc--current-project))
         (origin (current-buffer))
         (resize-mini-windows nil))
    (shell-command
     (format "git blame -L %d,%d -- %s" beg-line end-line f) buf)
    ;; FIXME 2021-07-31: Learn how to implement a cleaner
    ;; `revert-buffer'.  See NOTE above.
    (setq-local prot-vc--blame-beg beg
                prot-vc--blame-end end
                prot-vc--blame-file f
                prot-vc--blame-origin origin)
    (with-current-buffer buf-name
      (unless (equal major-mode 'vc-annotate-mode)
        (vc-annotate-mode))
      ;; FIXME 2021-07-31: Same issue with `revert-buffer'.
      (setq-local revert-buffer-function
                  (lambda (_ignore-auto _noconfirm)
                    (let ((inhibit-read-only t))
                      (with-current-buffer origin
                        (prot-vc-git-blame-region-or-file prot-vc--blame-beg
                                                          prot-vc--blame-end
                                                          prot-vc--blame-file)))))
      (setq-local vc-annotate-backend backend)
      (setq-local vc-annotate-parent-file f)
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

(defun prot-vc-git--prompt-remote ()
  "Helper prompt for `prot-vc-git-push'."
  (if-let ((remotes (process-lines "git" "remote"))
           ((length> remotes 1))) ; `length>' is Emacs 28+
      (completing-read "Select Git remote: " remotes)
    "origin"))

(declare-function vc-git--pushpull "vc-git" (command prompt extra-args))
(declare-function vc-git-push "vc-git" (prompt))

;;;###autoload
(defun prot-vc-git-push (prompt)
  "Substitute for `vc-git-push' with the same meaning for PROMPT."
  (vc-git--pushpull "push" prompt (unless prompt `(,(prot-vc-git--prompt-remote)))))

(advice-add #'vc-git-push :override #'prot-vc-git-push)

;;;; User Interface setup

(defun prot-vc-git-expanded-log-entry (revision)
  "Expand git commit message for REVISION.
This is a tweaked variant of `vc-git-expanded-log-entry' and
should be used as its override function."
  (with-temp-buffer
    (apply 'vc-git-command t nil nil (list "log" revision "--stat" "-1" "--"))
    (goto-char (point-min))
    (unless (eobp)
      (while (re-search-forward "^" nil t)
        (replace-match "  ")
        (forward-line))
      (concat "\n" (buffer-string)))))

(declare-function vc-git-expanded-log-entry "vc-git" (revision))

(advice-add #'vc-git-expanded-log-entry :override #'prot-vc-git-expanded-log-entry)

(provide 'prot-vc)
;;; prot-vc.el ends here
