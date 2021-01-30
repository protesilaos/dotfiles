;;; prot-diff.el --- Extensions to diff-mode.el for my dotemacs -*- lexical-binding: t -*-

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
;; This covers my diff-mode.el extensions, for use in my Emacs setup:
;; https://protesilaos.com/dotemacs.

;;; Code:

(require 'diff-mode)

(defgroup prot-diff ()
  "Extensions for diff mode."
  :group 'diff)

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

;;;###autoload
(defun prot-diff-refine-dwim (&optional arg)
  "Produce word-wise, 'refined' diffs in `diff-mode' buffer.

Operate on the entire buffer by default.  With optional prefix
ARG (\\[universal-argument]), act on the hunk at point.  When the
region is active, fontify the diff hunks encompassed by it.

If any such fontification is already present, revert the buffer
and place point back where it was."
  (interactive "P")
  (let ((position (point))
        (beg (or (when (mark) (region-beginning)) (point-min)))
        (end (or (when (mark) (region-end)) (point-max))))
    (when (derived-mode-p 'diff-mode)
      (cond
       ((and arg (not (region-active-p)))
        (diff-refine-hunk)
        (setq-local diff-refine 'font-lock))
       ((eq (buffer-local-value 'diff-refine (current-buffer)) 'font-lock)
        (revert-buffer)
        (goto-char position)
        (recenter))
       (t
        (setq-local diff-refine 'font-lock)
        (when (region-active-p) (deactivate-mark))
        (font-lock-flush beg end)
        (goto-char position))))))

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
  (if (eq modus-themes-diffs 'bg-only)
      (setq diff-font-lock-syntax 'hunk-also)
    (setq diff-font-lock-syntax nil)))

;;; Extend diff-mode font lock

(defface prot-diff-diffstat-added
  '((((class color) (min-colors 88) (background light))
     :foreground "#006800")
    (((class color) (min-colors 88) (background dark))
     :foreground "#44bc44")
    (t :foreground "green"))
  "Face for diffstat added indicators (+).")

(defface prot-diff-diffstat-removed
  '((((class color) (min-colors 88) (background light))
     :foreground "#a60000")
    (((class color) (min-colors 88) (background dark))
     :foreground "#ff8059")
    (t :foreground "red"))
  "Face for diffstat removed indicators (-).")

(defface prot-diff-diffstat-file-changed
  '((((class color) (min-colors 88) (background light))
     :foreground "#5e3a20")
    (((class color) (min-colors 88) (background dark))
     :foreground "#d0ba95")
    (t :foreground "yellow"))
  "Face for diffstat changed files.")

(defface prot-diff-hunk-file-added
  '((((class color) (min-colors 88) (background light))
     :foreground "#104410")
    (((class color) (min-colors 88) (background dark))
     :foreground "#88cf88")
    (t :foreground "green"))
  "Face for diff hunk file added.")

(defface prot-diff-hunk-file-removed
  '((((class color) (min-colors 88) (background light))
     :foreground "#7f1010")
    (((class color) (min-colors 88) (background dark))
     :foreground "#ffa0a0")
    (t :foreground "red"))
  "Face for diff hunk file removed.")

(defface prot-diff-commit-header
  '((((class color) (min-colors 88) (background light))
     :foreground "#000000")
    (((class color) (min-colors 88) (background dark))
     :foreground "#ffffff"))
  "Face for diff commit header keys like 'Author:'.")

(defface prot-diff-commit-message-title
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#001087")
    (((class color) (min-colors 88) (background dark))
     :foreground "#87c8ff"))
  "Face for diff commit message title (the commit's summary).")

(defface prot-diff-commit-message-body
  '((((class color) (min-colors 88) (background light))
     :foreground "#000000")
    (((class color) (min-colors 88) (background dark))
     :foreground "#ffffff"))
  "Face for diff commit message body.")

(defface prot-diff-commit-hash
  '((((class color) (min-colors 88) (background light))
     :foreground "#184034")
    (((class color) (min-colors 88) (background dark))
     :foreground "#bfebe0")
    (t :inherit shadow))
  "Face for diff commit unique identifier (hash).")

(defface prot-diff-commit-author
  '((((class color) (min-colors 88) (background light))
     :foreground "#00538b")
    (((class color) (min-colors 88) (background dark))
     :foreground "#00d3d0")
    (t :foreground "cyan"))
  "Face for diff commit author name.")

(defface prot-diff-commit-email
  '((((class color) (min-colors 88) (background light))
     :foreground "#0031a9")
    (((class color) (min-colors 88) (background dark))
     :foreground "#2fafff")
    (t :foreground "blue"))
  "Face for diff commit author email.")

(defface prot-diff-commit-date
  '((((class color) (min-colors 88) (background light))
     :foreground "#55348e")
    (((class color) (min-colors 88) (background dark))
     :foreground "#cfa6ff")
    (t :foreground "magenta"))
  "Face for diff commit date.")

;; NOTE 2021-01-30: These work in all scenaria I tried, but there may
;; still be errors or omissions.
(defconst prot-diff-keywords
  '(("\\(^[^+@-]?\\)\\(.*?\s+|\s+\\)\\([0-9]*\\) \\(\\++\\)"
     (2 'prot-diff-diffstat-file-changed)
     (4 'prot-diff-diffstat-added))
    ("\\(^[^+-]?\\)\\(\\+\\{3\\}\\) \\(.*\\)"
     (2 'prot-diff-diffstat-added)
     ;; (3 'prot-diff-hunk-file-added)
     )
    ("\\(^[^+-]?\\)\\(-+\\{3\\}\\) \\(.*\\)"
     (2 'prot-diff-diffstat-removed)
     ;; (3 'prot-diff-hunk-file-removed)
     )
    ("\\(^[^+@-]?\\)\\(.*?\s+|\s+\\)\\([0-9]*\\) \\(\\++\\)?\\(-+\\)"
     (2 'prot-diff-diffstat-file-changed)
     (5 'prot-diff-diffstat-removed))
    ("\\([0-9]+ files? changed,.*\\)"
     (0 'prot-diff-diffstat-file-changed)
     ;; ;; NOTE: Or comment out the above and use the following
     ;; "\\([0-9]+ files? changed,\\) \\(.*(\\+)\\)\\(, \\)?\\(.*(-)\\)?"
     ;; (1 'prot-diff-diffstat-file-changed)
     ;; (2 'prot-diff-hunk-file-added)
     ;; (3 'prot-diff-diffstat-file-changed)
     ;; (4 'prot-diff-hunk-file-removed)
     )
    ("^\s\\{4\\}\\(.+\n\s\\{4\\}\n\\)"  ; Fragile?
     (1 'prot-diff-commit-message-title))
    ("^\s\\{4\\}\\([a-zA-Z]+.*?\\)\n"   ; Is this bound to fail?
     (1 'prot-diff-commit-message-body))
    ("^---\n"
     (0 'prot-diff-commit-header))
    ("\\(^commit \\)\\(.*\\)"
     (1 'prot-diff-commit-header)
     (2 'prot-diff-commit-hash))
    ("\\(^Author: \\)\\(.*\\)\\(<\\)\\(.*\\)\\(>\\)"
     (1 'prot-diff-commit-header)
     (2 'prot-diff-commit-author)
     (3 'prot-diff-commit-header)
     (4 'prot-diff-commit-email)
     (5 'prot-diff-commit-header))
    ("\\(^Date: \\)\\(.*\\)"
     (1 'prot-diff-commit-header)
     (2 'prot-diff-commit-date)))
  "Extra font-lock patterns for diff mode.")

;;;###autoload
(define-minor-mode prot-diff-extra-keywords
  "Apply extra font-lock rules to diff buffers."
  :init-value nil
  :global t
  (if prot-diff-extra-keywords
      (progn
        (font-lock-flush (point-min) (point-max))
        (font-lock-add-keywords nil prot-diff-keywords nil)
        (add-hook 'diff-mode-hook #'prot-diff-extra-keywords))
    (font-lock-remove-keywords nil prot-diff-keywords)
    (remove-hook 'diff-mode-hook #'prot-diff-extra-keywords)
    (font-lock-flush (point-min) (point-max))))

(provide 'prot-diff)
;;; prot-diff.el ends here
