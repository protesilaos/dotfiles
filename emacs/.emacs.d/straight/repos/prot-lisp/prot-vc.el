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

;;; Code:

(require 'vc)

;;;; Customisation options

(defgroup prot-vc ()
  "Extensions for vc.el and related libraries."
  :group 'project)

(defcustom prot-vc-log-limit 20
  "Limit commits in `prot-vc-custom-log' and others."
  :type 'integer
  :group 'prot-vc)

(defcustom prot-vc-shell-output "*prot-vc-shell-output*"
  "Name of buffer for VC-related shell output."
  :type 'string
  :group 'prot-vc)

(defcustom prot-vc-patch-output-dirs (list "~/" "~/Desktop/")
  "List of directories to save `prot-vc-patch-dwim' output."
  :type 'list
  :group 'prot-vc)

(defcustom prot-vc-advice-vc-git t
  "Whether to pass advice to `vc-git' functions."
  :type 'list
  :group 'prot-vc)

;;;; Commands

;;;###autoload
(defun prot-vc-project-or-dir (&optional arg)
  "Run `vc-dir' for the current project root.
With optional prefix ARG (\\[universal-argument]), use the
`default-directory' instead."
  (interactive "P")
  (let* ((root (or (vc-root-dir)
                   (locate-dominating-file "." ".git")))
         (dir (if arg default-directory root)))
    (vc-dir dir)))

(declare-function log-view-current-entry "log-view")
(declare-function dired-get-marked-files "dired")

(defun prot-vc--commit-num ()
  "Determime whether NUM is a positive integer."
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
                 "Select files in current dir: " files nil t))))
         (backend (vc-backend set)))
    (vc-print-log-internal backend set nil nil lim 'with-diff)))

;;;###autoload
(defun prot-vc-log-kill-hash ()
  "Save to `kill-ring' contextual commit hash in `vc-print-log'."
  (interactive)
  (let ((commit (cadr (log-view-current-entry (point) t))))
    (kill-new (format "%s" commit))
    (message "Copied: %s" commit)))

(defvar prot-vc--patch-commit-hist '()
  "Minibuffer history for `prot-vc-patch-dwim' commits.")

(defvar prot-vc--patch-output-hist '()
  "Minibuffer history for `prot-vc-patch-dwim' output.")

;; TODO: abstract `process-lines' and make format customisable
;; REVIEW: using __ %h __ works but is a quick and dirty hack
(defun prot-vc--log-commit-prompt (&optional prompt)
  "Select git log commit with completion using optional PROMPT."
  (let ((text (or prompt "Select a commit: "))
        (vc (or (vc-root-dir)
                (locate-dominating-file "." ".git")))
        (num (format "%s" prot-vc-log-limit)))
    (if vc
        (completing-read
         text
         (process-lines "git" "log" "--pretty=format:%d __ %h __ %ad %an: %s" "-n" num)
         nil t nil 'prot-vc--patch-commit-hist)
      (error "'%s' is not under version control" default-directory))))

(defun prot-vc--log-commit-hash ()
  "Extract commit hash from `prot-vc--log-commit-prompt'."
  (let ((commit (prot-vc--log-commit-prompt)))
    (string-match "__ \\([a-z0-9]*\\) __" commit) ; see above "review" comment
    (match-string-no-properties 1 commit)))

;;;###autoload
(defun prot-vc-patch-dwim (&optional arg)
  "Create patch for commit at point in `log-view'.
With optional prefix ARG (\\[universal-argument]) prompt for
commit with completion."
  (interactive "P")
  (let* ((commit-at-point (cadr (log-view-current-entry (point) t)))
         (commit (if (or arg (not commit-at-point))
                     (prot-vc--log-commit-hash)
                   commit-at-point))
         (vc-dir (or (vc-root-dir)
                     (locate-dominating-file "." ".git")
                     default-directory))
         (dirs (append (list vc-dir) prot-vc-patch-output-dirs))
         (out-dir
          (completing-read "Output directory: "
                           dirs nil t nil 'prot-vc--patch-output-hist))
         (buf (get-buffer-create prot-vc-shell-output)))
    (shell-command
     (format "git format-patch -1 %s -o %s --" commit out-dir) buf)
    (message "Prepared patch for `%s' and sent it to %s"
             (propertize commit 'face 'bold)
             (propertize out-dir 'face 'success))
    (add-to-history 'prot-vc--patch-commit-hist commit)
    (add-to-history 'prot-vc--patch-output-hist out-dir)))

;; This is a tweaked variant of `vc-git-expanded-log-entry'
(defun prot-vc-git-expanded-log-entry (revision)
  "Expand git commit message for REVISION."
  (with-temp-buffer
    (apply 'vc-git-command t nil nil (list "log" revision "--stat" "-1" "--"))
    (goto-char (point-min))
    (unless (eobp)
      ;; Indent the expanded log entry.
      (while (re-search-forward "^  " nil t)
        (replace-match "")
        (forward-line))
      (buffer-string))))

(defun prot-vc-git-expand-function ()
  "Set `log-view-expanded-log-entry-function' for `vc-git'."
  (setq-local log-view-expanded-log-entry-function
              #'prot-vc-git-expanded-log-entry))

(defvar prot-vc-git-log-view-mode-hook nil
  "Hook that runs after `vc-git-log-view-mode'.")

(defun prot-vc-git-log-view-add-hook (&rest _)
  "Run `prot-vc-git-log-view-mode-hook'."
  (run-hooks 'prot-vc-git-log-view-mode-hook))

(declare-function vc-git-log-view-mode "vc-git")

;;;###autoload
(define-minor-mode prot-vc-git-setup-mode
  "Extend `vc-git'."
  :init-value nil
  :global t
  (if (and prot-vc-advice-vc-git prot-vc-git-setup-mode)
      (progn
        (advice-add #'vc-git-log-view-mode :after #'prot-vc-git-log-view-add-hook)
        (add-hook 'prot-vc-git-log-view-mode-hook #'prot-vc-git-expand-function))
    (advice-remove #'vc-git-log-view-mode #'prot-vc-git-log-view-add-hook)
    (remove-hook 'prot-vc-git-log-view-mode-hook #'prot-vc-git-expand-function)))

(provide 'prot-vc)
;;; prot-vc.el ends here
