;;; prot-vc.el --- Extensions to vc.el for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2021  Protesilaos Stavrou

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
;; This covers my vc.el extensions, for use in my Emacs setup:
;; https://protesilaos.com/dotemacs.
;;
;; Make sure to also inspect prot-project.el and prot-diff.el for a more
;; complete view of what I have on the topic of version control.

;;; Code:

(require 'vc)
(require 'prot-common)

;;;; Customisation options

(defgroup prot-vc ()
  "Extensions for vc.el and related libraries."
  :group 'project)

(defcustom prot-vc-log-limit 20
  "Limit commits in `prot-vc-custom-log' and others."
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

(autoload 'log-edit-files "log-edit")

(defun prot-vc--log-edit-files-prompt ()
  "Helper completion for `prot-vc-extract-file-name'."
  (let ((files (log-edit-files)))
    (completing-read
     "Derive shortname from: " files nil nil)))

;;;###autoload
(defun prot-vc-log-extract-file-name ()
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
  "History for `prot-vc-log-insert-commits'.")

;;;###autoload
(defun prot-vc-log-insert-commits ()
  "Insert at point number of commits starting from git HEAD.
If in a version-controlled directory, the commit log is based on
the root of the project, else a prompt for project selection is
produced with `project-current'."
  (interactive)
  (let ((default-directory (or (prot-vc--current-project)
                               (cdr (project-current t))))
        (number (number-to-string
                 (read-number "Insert N commits from HEAD: " 5
                              'prot-vc--log-insert-num-hist))))
    (insert
     (with-temp-buffer
       (apply 'vc-git-command t nil nil (list "log" "--pretty=format:%h %cs %s"
                                              "-n" number "--"))
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

;;;###autoload
(defun prot-vc-log-view-toggle-entry-all ()
  "Run `log-view-toggle-entry-display' on all commits."
  (interactive)
  (let ((oldlines (count-lines (point-min) (point-max)))
        (point (point))
        (newlines))
    ;; FIXME: how to handle the case of long logs where performance
    ;; takes a hit?  Using line count is not reliable: we need the
    ;; number of commits and then say if NUM > LIMIT then prompt with
    ;; yes or no on whether to proceed.
    (save-excursion
      (goto-char (point-max))
      (while (not (eq (line-number-at-pos) 1))
        (log-view-msg-prev)
        (log-view-toggle-entry-display))
      (goto-char point)
      (setq newlines (count-lines (point-min) (point-max))))
    (when (> newlines oldlines)
      (log-view-msg-next))
    (recenter)))

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
  "Create patch for commit at point in `log-view'.
With optional prefix ARG (\\[universal-argument]), or if no
commit is available at or around point, prompt for one with
completion.  In that case, the list of candidates is confined to
`prot-vc-log-limit'."
  (interactive "P")
  (let* ((commit-at-point (cadr (log-view-current-entry (point) t)))
         (commit (if (or arg (not commit-at-point))
                     (prot-vc--log-commit-hash
                      (prot-vc--log-commit-prompt "Prepare patch for commit: " t))
                   commit-at-point))
         (vc-dir (or (prot-vc--current-project)
                     default-directory))
         (dirs (append (list vc-dir) prot-vc-patch-output-dirs))
         (out-dir
          (completing-read
           "Output directory: "
           (prot-common-completion-table 'file dirs)
           nil t nil 'prot-vc--patch-output-hist))
         (buf (get-buffer-create prot-vc-shell-output)))
    (shell-command
     (format "git format-patch -1 %s -o %s --" commit out-dir) buf)
    (message "Prepared patch for `%s' and sent it to %s"
             (propertize commit 'face 'bold)
             (propertize out-dir 'face 'success))
    (add-to-history 'prot-vc--commit-hist commit)
    (add-to-history 'prot-vc--patch-output-hist out-dir)))

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
      (diff-mode))
    (add-to-history 'prot-vc--commit-hist commit)))

(defun prot-vc--file-rev (file &optional limit)
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
(defun prot-vc-find-revision (&optional limit)
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
               (prot-vc--file-rev buffer-file-name num))))
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
        (vc-annotate-mode))
      (setq-local vc-annotate-backend backend)
      (setq-local vc-annotate-parent-file file)
      (setq-local vc-annotate-parent-rev rev)
      (setq-local vc-annotate-parent-display-mode 'scale)
      (vc-annotate-display-select buf 'fullscale))))

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

;;;###autoload
(defun prot-vc-git-expand-function ()
  "Set `log-view-expanded-log-entry-function' for `vc-git'."
  (setq-local log-view-expanded-log-entry-function
              #'prot-vc-git-expanded-log-entry))

(defvar prot-vc-git-log-view-mode-hook nil
  "Hook that runs after `vc-git-log-view-mode'.")

(defun prot-vc-git-log-view-add-hook (&rest _)
  "Run `prot-vc-git-log-view-mode-hook'."
  (run-hooks 'prot-vc-git-log-view-mode-hook))

(autoload 'vc-git-log-view-mode "vc-git")

;;;###autoload
(define-minor-mode prot-vc-git-setup-mode
  "Extend `vc-git'."
  :init-value nil
  :global t
  (if prot-vc-git-setup-mode
      (progn
        (advice-add #'vc-git-log-view-mode :after #'prot-vc-git-log-view-add-hook)
        (add-hook 'prot-vc-git-log-view-mode-hook #'prot-vc-git-expand-function))
    (advice-remove #'vc-git-log-view-mode #'prot-vc-git-log-view-add-hook)
    (remove-hook 'prot-vc-git-log-view-mode-hook #'prot-vc-git-expand-function)))

;;;; Utilities

;; TODO 2021-02-03: Make this setup optional.

(declare-function log-edit-add-field "log-edit")

;; FIXME: The comment block should be formatted in a separate function.
(defun prot-vc-git-log-edit-comment ()
  "Add comment block to Git Log Edit buffer."
  (let* ((eoh (save-excursion (rfc822-goto-eoh) (point)))
         (branch-name (process-lines "git" "branch" "--show-current"))
         (branch (or (car branch-name) "Detached HEAD"))
         (remote-name (when branch-name
                        (car (process-lines "git" "branch" "-r")))) ; REVIEW Is this reliable?
         (remote (cadr (split-string remote-name "->" t "[\s\t]+")))
         (files (mapconcat (lambda (x)
                             (concat "#   " x))
                           (log-edit-files)
                           "\n")))
    (save-excursion
      (when (<= (point) eoh)
	    (goto-char eoh)
	    (when (looking-at "\n") (forward-char 1)))
      (log-edit-add-field "Summary" "")
      (insert
       (format "\n\n# %s `%s' tracking `%s':\n#\n%s\n#\n# %s"
               "Files to be committed to branch"
               branch remote
               files
               "All lines starting with `#' are ignored.")))
    (rfc822-goto-eoh)
    (when (looking-at "\n") (forward-char -1))))

(add-hook 'log-edit-hook #'prot-vc-git-log-edit-comment)

(defun prot-vc-git-log-remove-comment ()
  "Remove Git Log Edit comment and empty lines.
Ensure a final newline."
  (let ((buffer (get-buffer "*vc-log*")))
    (with-current-buffer (if (buffer-live-p buffer)
                             buffer
                           (window-buffer (get-mru-window)))
      (save-excursion
        (goto-char (point-min)))
      (flush-lines "\\(^#\\|^\n\\)")
      (or (= (point-min) (point-max))
          (= (char-before (point-max)) ?\n)
          (save-excursion
            (goto-char (point-max))
            (insert "\n"))))))

(declare-function log-edit-show-diff "log-edit")

(defvar prot-vc--windows-current nil
  "Current window configuration.")

(defun prot-vc-log-diff-window-configuration ()
  "Show current diff for Git Log Edit buffer."
  (let ((buffer (get-buffer "*vc-log*")))
    (setq prot-vc--windows-current (current-window-configuration))
    (with-current-buffer (if (buffer-live-p buffer)
                             buffer
                           (window-buffer (get-mru-window)))
      (delete-other-windows)
      (log-edit-show-diff)
      (other-window -1))))

(add-hook 'log-edit-hook #'prot-vc-log-diff-window-configuration)

;; FIXME: Why does `prot-vc-git-log-remove-comment' not work when added
;; to `log-edit-done-hook'?
;;;###autoload
(defun prot-vc-git-log-edit-done ()
  "Remove Git Log Edit comments and commit change set.
This is a thin wrapper around `log-edit-done', which first calls
`prot-vc-git-log-remove-comment'."
  (interactive)
  (prot-vc-git-log-remove-comment)
  (call-interactively 'log-edit-done)
  (set-window-configuration prot-vc--windows-current))

(defface prot-vc-git-log-edit-file-name
  '((default :inherit font-lock-comment-face)
    (((class color) (min-colors 88) (background light))
     :foreground "#00538b")
    (((class color) (min-colors 88) (background dark))
     :foreground "#00d3d0")
    (t :foreground "cyan"))
  "Face for remote branch name in VC Git Log Edit buffers.")

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

(defun prot-vc-log-edit-extra-keywords ()
  "Apply `prot-vc-git-log-edit-font-lock' to Log Edit buffers."
  (font-lock-flush (point-min) (point-max))
  (font-lock-add-keywords nil prot-vc-git-log-edit-font-lock nil))

(add-hook 'log-edit-hook #'prot-vc-log-edit-extra-keywords)

(provide 'prot-vc)
;;; prot-vc.el ends here
