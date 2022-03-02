;;; logos.el --- Simple presentations with page breaks or outlines -*- lexical-binding: t -*-

;; Copyright (C) 2022  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://gitlab.com/protesilaos/logos
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
;; With all user options disabled (the out-of-the-box design), this
;; package provides a simple approach to handling presentations using
;; nothing but the `page-delimiter' together with some commands to move
;; between pages whether narrowing is in effect or not.
;;
;; What constitutes a page delimiter is determined by the user options
;; `logos-outlines-are-pages' and `logos-outline-regexp-alist'.  By
;; default, this only corresponds to the ^L character (which can be
;; inserted using the standard keys with C-q C-l).
;;
;; Logos does not define any key bindings.  Try something like this, if
;; you want:
;;
;;     (let ((map global-map))
;;       (define-key map [remap narrow-to-region] #'logos-narrow-dwim)
;;       (define-key map [remap forward-page] #'logos-forward-page-dwim)
;;       (define-key map [remap backward-page] #'logos-backward-page-dwim))
;;
;; By default those key bindings are: C-x n n, C-x ], C-x [.  The
;; `logos-narrow-dwim' is not necessary if you already know how to
;; narrow effectively.
;;
;; Logos provides some optional aesthetic tweaks which come into effect
;; when the buffer-local `logos-focus-mode' is enabled.  These will hide
;; the mode line (`logos-hide-mode-line'), enable `scroll-lock-mode'
;; (`logos-scroll-lock'), and use `variable-pitch-mode' in
;; non-programming buffers (`logos-variable-pitch').  All these
;; variables are buffer-local.
;;
;; To position the buffer in the center of the window, use the
;; `olivetti' package by Paul W. Rankin.  Sample glue code:
;;
;;     (defun my-logos--olivetti-mode ()
;;       "Toggle `olivetti-mode'."
;;       (if (or (bound-and-true-p olivetti-mode)
;;               (null (logos--focus-p)))
;;           (olivetti-mode -1)
;;         (olivetti-mode 1)))
;;     
;;     (add-hook 'logos-focus-mode-hook #'my-logos--olivetti-mode)
;;
;; Logos is the familiar word derived from Greek (watch my presentation
;; on philosophy about Cosmos, Logos, and the living universe:
;; <https://protesilaos.com/books/2022-02-05-cosmos-logos-living-universe/>),
;; though it also stands for these two perhaps equally insightful
;; backronyms about the mechanics of this package:
;;
;; 1. ^L Only Generates Ostensible Slides
;; 2. Logos Optionally Garners Outline Sections

;;; Code:

;;;; General utilities

(defgroup logos ()
  "Simple presentations with page breaks."
  :group 'editing)

(defcustom logos-outlines-are-pages nil
  "When non-nil, every outline heading is a page delimiter.
What constitutes an outline is determined by the user option
`logos-outline-regexp-alist'.

When this variable is nil, pages are demarcated by the
`page-delimiter'."
  :type 'boolean
  :group 'logos)

(defconst logos--page-delimiter (default-value 'page-delimiter)
  "The default value of `page-delimiter'.")

(defcustom logos-outline-regexp-alist   ; TODO 2022-03-02: more sensible outlines?
  `((emacs-lisp-mode . "^;;;+ ")
    (org-mode . "^\\*+ +")
    (t . ,(or outline-regexp logos--page-delimiter)))
  "Alist of major mode and regular expression of the outline.
Only used when `logos-outlines-are-pages' is non-nil."
  :type `(alist :key-type symbol :value-type string) ; TODO 2022-03-02: ensure symbol is mode?
  :group 'logos)

(defcustom logos-hide-mode-line nil
  "When non-nil hide the modeline.
This is only relevant when `logos-focus-mode' is enabled."
  :type 'boolean
  :group 'logos)

(make-variable-buffer-local 'logos-hide-mode-line)

(defcustom logos-scroll-lock nil
  "When non-nil, use `scroll-lock-mode'.
This is only relevant when `logos-focus-mode' is enabled."
  :type 'boolean
  :group 'logos)

(make-variable-buffer-local 'logos-scroll-lock)

(defcustom logos-variable-pitch nil
  "When non-nil, `text-mode' buffers use `variable-pitch-mode'.
In programming modes the default font is always used, as that is
assumed to be a monospaced typeface.

This is only relevant when `logos-focus-mode' is enabled."
  :type 'boolean
  :group 'logos)

(make-variable-buffer-local 'logos-variable-pitch)

;;;; General utilities

(defun logos--focus-p ()
  "Return non-nil if `logos-focus-mode' is bound locally."
  (when (bound-and-true-p logos-focus-mode)
    (buffer-local-value 'logos-focus-mode (current-buffer))))

;;;; Page motions

(defun logos--outline-regexp ()
  "Return page delimiter from `logos-outline-regexp-alist'."
  (let ((outline logos-outline-regexp-alist))
    (or (alist-get major-mode outline)
        (alist-get t outline))))

(defun logos--page-delimiter ()
  "Determine the `page-delimiter'."
  (if logos-outlines-are-pages
      (setq-local page-delimiter (logos--outline-regexp))
    (setq-local page-delimiter logos--page-delimiter)))

(defun logos--narrow-to-page (count &optional back)
  "Narrow to COUNTth page with optional BACK motion."
  (if back
      (narrow-to-page (or (- count) -1))
    (narrow-to-page (or (abs count) 1)))
  ;; Avoids the problem of skipping pages while cycling back and forth.
  (goto-char (point-min)))

(defun logos--page-motion (&optional count back)
  "Routine for page motions.
With optional numeric COUNT move by that many pages.  With
optional BACK perform the motion backwards."
  (let ((cmd (if back #'backward-page #'forward-page)))
    (logos--page-delimiter)
    (if (buffer-narrowed-p)
        (logos--narrow-to-page count back)
      (funcall cmd count)
      (setq this-command cmd))))

;;;###autoload
(defun logos-forward-page-dwim (&optional count)
  "Move to next or COUNTth page forward.
If the buffer is narrowed, keep the effect while performing the
motion.  Always move point to the beginning of the narrowed
page."
  (interactive "p")
  (logos--page-motion count))

;;;###autoload
(defun logos-backward-page-dwim (&optional count)
  "Move to previous or COUNTth page backward.
If the buffer is narrowed, keep the effect while performing the
motion.  Always move point to the beginning of the narrowed
page."
  (interactive "p")
  (logos--page-motion count :back))

;;;; Narrowing
;; NOTE 2022-03-02: This section is most likely unnecessary, but let's
;; keep it for now.

(defun logos--window-bounds ()
  "Determine start and end points in the window."
  (list (window-start) (window-end)))

(defun logos--page-p ()
  "Return non-nil if there is a `page-delimiter' in the buffer.
This function does not use `widen': it only checks the accessible
portion of the buffer."
  (or (save-excursion (re-search-forward page-delimiter nil t))
      (save-excursion (re-search-backward page-delimiter nil t))))

(defun logos-narrow-visible-window ()
  "Narrow buffer to wisible window area.
Also check `logos-narrow-dwim'."
  (interactive)
  (let* ((bounds (logos--window-bounds))
         (window-area (- (cadr bounds) (car bounds)))
         (buffer-area (- (point-max) (point-min))))
    (if (/= buffer-area window-area)
        (narrow-to-region (car bounds) (cadr bounds))
      (user-error "Buffer fits in the window; won't narrow"))))

;;;###autoload
(defun logos-narrow-dwim ()
  "Do-what-I-mean narrowing.

If region is active, narrow the buffer to the region's
boundaries.

If pages are defined by virtue of `logos--page-p', narrow to
the current page boundaries.

If no region is active and no pages exist, narrow to the visible
portion of the window.

If narrowing is in effect, widen the view."
  (interactive)
  (unless mark-ring                  ; needed when entering a new buffer
    (push-mark (point) t nil))
  (cond
   ((and (use-region-p)
         (null (buffer-narrowed-p)))
    (narrow-to-region (region-beginning) (region-end)))
   ((logos--page-p)
    (narrow-to-page))
   ((null (buffer-narrowed-p))
    (logos-narrow-visible-window))
   ((widen))))

;;;; Optional "focus mode" and utilities

(define-minor-mode logos-focus-mode
  "Buffer-local mode for focused editing."
  :init-value nil
  :global nil
  :lighter " Λ" ; greek lambda majuscule
  (logos--setup))

(defun logos--setup ()
  "Setup aesthetics for presentation."
  (logos--variable-pitch-toggle)
  (logos--scroll-lock)
  (logos--hide-mode-line))

(defun logos--variable-pitch-toggle ()
  "Make text use `variable-pitch' face, except for programming."
  (when (and logos-variable-pitch
             (derived-mode-p 'text-mode))
    (if (or (bound-and-true-p buffer-face-mode)
            (null (logos--focus-p)))
        (variable-pitch-mode -1)
      (variable-pitch-mode 1))))

(defun logos--scroll-lock ()
  "Keep the point at the centre."
  (when logos-scroll-lock
    (if (or (bound-and-true-p scroll-lock-mode)
            (null (logos--focus-p)))
        (scroll-lock-mode -1)
      (recenter nil)
      (scroll-lock-mode 1))))

;; Based on Paul W. Rankin's code:
;; <https://gist.github.com/rnkn/a522429ed7e784ae091b8760f416ecf8>.
(defun logos--hide-mode-line ()
  "Toggle mode line visibility."
  (when logos-hide-mode-line
    (if (or (null mode-line-format)
            (null (logos--focus-p)))
        (kill-local-variable 'mode-line-format)
      (setq-local mode-line-format nil)
      (force-mode-line-update))))

(provide 'logos)
;;; logos.el ends here
