;;; prot-diff.el --- Extensions to diff-mode.el for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2020  Protesilaos Stavrou

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
        (beg (or (region-beginning) (point-min)))
        (end (or (region-end) (point-max))))
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

(provide 'prot-diff)
;;; prot-diff.el ends here
