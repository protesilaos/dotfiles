;;; prot-diff.el --- Extensions to diff-mode.el for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2020-2022  Protesilaos Stavrou

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos.com/emacs/dotemacs
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
;; This covers my diff-mode.el extensions, for use in my Emacs setup:
;; https://protesilaos.com/emacs/dotemacs.
;;
;; Make sure to also inspect prot-vc.el and prot-project.el for a more
;; complete view of what I have on the topic of version control.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

(require 'diff-mode)

(defgroup prot-diff ()
  "Extensions for diff mode."
  :group 'diff)

(defun prot-diff-enable-outline-minor-mode ()
  "Enable `outline-minor-mode' with appropriate tweaks for diffs.
This basically gives you folding of diff hunks by means of the
`outline-cycle' command.

Add the function to the `diff-mode-hook'."
  (require 'outline)
  (let ((outline-minor-mode-highlight nil))
    (when (derived-mode-p 'diff-mode)
      (outline-minor-mode 1))))

;;;###autoload
(defun prot-diff-buffer-dwim (&optional arg)
  "Diff buffer with its file's last saved state, or run `vc-diff'.
With optional prefix ARG (\\[universal-argument]) enable
highlighting of word-wise changes (local to the current buffer)."
  (interactive "P")
  (let ((buf))
    (if (buffer-modified-p)
        (progn
          (diff-buffer-with-file (current-buffer))
          (setq buf "*Diff*"))
      (vc-diff)
      (setq buf "*vc-diff*"))
    (when arg
      (with-current-buffer (get-buffer buf)
        (unless diff-refine
          (setq-local diff-refine 'font-lock))))))

(defvar-local prot-diff--refine-diff-state 0
  "Current state of `prot-diff-refine-dwim'.")

;;;###autoload
(defun prot-diff-refine-cycle ()
  "Produce buffer-local, 'refined' or word-wise diffs in Diff mode.

Upon first invocation, refine the diff hunk at point or, when
none exists, the one closest to it.  On second call, operate on
the entire buffer.  And on the third time, remove all word-wise
fontification."
  (interactive)
  (let ((point (point)))
    (pcase prot-diff--refine-diff-state
      (0
       (diff-refine-hunk)
       (setq prot-diff--refine-diff-state 1))
      (1
       (setq-local diff-refine 'font-lock)
       (font-lock-flush)
       (goto-char point)
       (setq prot-diff--refine-diff-state 2))
      (_
       (revert-buffer)
       (goto-char point)
       (recenter)
       (setq prot-diff--refine-diff-state 0)))))

;;;###autoload
(defun prot-diff-narrow-dwim (&optional arg)
  "Use `diff-restrict-view', or widen when already narrowed.
By default the narrowing effect applies to the focused diff hunk.
With optional prefix ARG (\\[universal-argument]) do it for the
current file instead."
  (interactive "P")
  (when (derived-mode-p 'diff-mode)
    (if (buffer-narrowed-p)
        (progn
          (widen)
          (message "Widened the view"))
      (if arg
          (progn
            (diff-restrict-view arg)
            (message "Narrowed to file"))
        (diff-restrict-view)
        (message "Narrowed to diff hunk")))))

;;;###autoload
(defun prot-diff-hunk-kill-dwim (&optional beg end)
  "Kill current hunk or remove the plus or minus signs.
When the region is active, remove the plus or minus sign at the
start of the line.

When the region is not active but point is on a line that start
with a plus or minus sign, remove that sign.

Removing the plus or minus sign means any subsequent commit will
not account for them.

If no region is active and the point is not on a line that starts
with the plus or minus sign, call `diff-hunk-kill' interactively.

NOTE 2022-09-25 11:37 +0300: not tested thoroughly yet."
  (interactive "r" diff-mode)
  (when-let (((derived-mode-p 'diff-mode))
             (inhibit-read-only t))
    (cond
     ((region-active-p)
      (replace-regexp-in-region "^[+-]" " " beg end))
     ((progn (goto-char (line-beginning-position)) (looking-at "^[+-]"))
      (replace-match " "))
     (t (call-interactively #'diff-hunk-kill)))))

(defvar modus-themes-diffs)

;;;###autoload
(defun prot-diff-modus-themes-diffs ()
  "Configure `diff-font-lock-syntax' for accessibility.

A non-nil value for that variable will apply fontification to the
text while also trying to add the familiar diff styles.  This can
easily result in inaccessible colour combinations.

My Modus themes, which are designed for the highest accessibility
standard in legibility, provide an option that can work well with
such non-nil values.  Otherwise `diff-font-lock-syntax' should be
set to nil.

Run this function at the post theme load phase, such as with the
hook `modus-themes-after-load-theme-hook'."
  (if (and (featurep 'modus-themes)
	   (not (eq modus-themes-diffs 'bg-only)))
      (setq diff-font-lock-syntax nil)
    (setq diff-font-lock-syntax 'hunk-also)))

;;; Extend diff-mode font lock

(defface prot-diff-diffstat-added '((t :inherit diff-indicator-added))
  "Face for diffstat added indicators (+).")

(defface prot-diff-diffstat-removed '((t :inherit diff-indicator-removed))
  "Face for diffstat removed indicators (-).")

(defface prot-diff-commit-header '((t :inherit bold))
  "Face for diff commit header keys like 'Author:'.")

(defface prot-diff-commit-hash '((t :inherit log-view-message))
  "Face for diff commit unique identifier (hash).")

(defface prot-diff-commit-author '((t :inherit change-log-name))
  "Face for diff commit author name.")

(defface prot-diff-commit-email '((t :inherit change-log-email))
  "Face for diff commit author email.")

(defface prot-diff-commit-date '((t :inherit change-log-date))
  "Face for diff commit date.")

(defface prot-diff-commit-subject '((t :inherit change-log-name))
  "Face for diff commit message subject.")

(defconst prot-diff-keywords
  '(("\\(^[^+@-]?\\)\\(.*?\s+|\s+\\)\\([0-9]*\\) \\(\\++\\)"
     (4 'prot-diff-diffstat-added))
    ("\\(^[^+-]?\\)\\(\\+\\{3\\}\\) \\([ab].*?\\)"
     (2 'prot-diff-diffstat-added))
    ("\\(^[^+-]?\\)\\(-+\\{3\\}\\) \\([ab].*?\\)"
     (2 'prot-diff-diffstat-removed))
    ("\\(^[^+@-]?\\)\\(.*?\s+|\s+\\)\\([0-9]*\\) \\(\\++\\)?\\(-+\\)"
     (5 'prot-diff-diffstat-removed))
    ("^---\n"
     (0 'prot-diff-commit-header))
    ("\\(^commit \\)\\(.*\\)"
     (1 'prot-diff-commit-header)
     (2 'prot-diff-commit-hash))
    ("\\(^Author: \\)\\(.*\\)\\(<\\)\\(.*\\)\\(>\\)"
     (1 'prot-diff-commit-header)
     (2 'prot-diff-commit-author)
     (3 'prot-diff-commit-email)
     (4 'prot-diff-commit-email)
     (5 'prot-diff-commit-email))
    ("\\(^From:\\|^To:\\|^Cc:\\) ?\\(.*\\)?\\(<\\)\\(.*\\)\\(>\\)"
     (1 'prot-diff-commit-header)
     (2 'prot-diff-commit-author)
     (3 'prot-diff-commit-email)
     (4 'prot-diff-commit-email)
     (5 'prot-diff-commit-email))
    ("\\(^Subject:\\) \\(.*\\)"
     (1 'prot-diff-commit-header)
     (2 'prot-diff-commit-subject))
    ("\\(^From\\)\\( [0-9a-zA-Z]+ \\)\\(.*\\)"
     (1 'prot-diff-commit-header)
     (2 'prot-diff-commit-hash)
     (3 'prot-diff-commit-date))
    ("\\(^Message-Id:\\) \\(<.+>\\)"
     (1 'prot-diff-commit-header)
     (2 'prot-diff-commit-hash))
    ("\\(^Date: \\)\\(.*\\)"
     (1 'prot-diff-commit-header)
     (2 'prot-diff-commit-date)))
  "Extra font-lock patterns for diff mode.")

;;;###autoload
(define-minor-mode prot-diff-extra-keywords
  "Apply extra font-lock rules to diff buffers."
  :init-value nil
  :global t
  (if (and prot-diff-extra-keywords (derived-mode-p 'diff-mode))
      (progn
        (font-lock-flush (point-min) (point-max))
        (font-lock-add-keywords nil prot-diff-keywords nil)
        (add-hook 'diff-mode-hook #'prot-diff-extra-keywords))
    (font-lock-remove-keywords nil prot-diff-keywords)
    (remove-hook 'diff-mode-hook #'prot-diff-extra-keywords)
    (font-lock-flush (point-min) (point-max))))

(provide 'prot-diff)
;;; prot-diff.el ends here
