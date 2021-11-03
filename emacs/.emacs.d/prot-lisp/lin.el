;;; lin.el --- LIN Is Noticeable -*- lexical-binding: t -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://gitlab.com/protesilaos/lin
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
;; LIN locally remaps the `hl-line' face to a style that is optimal for
;; major modes where line selection is the primary mode of interaction.
;;
;; The idea is that `hl-line' cannot work equally well for contexts with
;; competing priorities: (i) line selection, or (ii) simple line
;; highlight.  In the former case, the current line needs to be made
;; prominent because it carries a specific meaning of some significance
;; in the given context.  Whereas in the latter case, the primary mode
;; of interaction does not revolve around the line highlight itself: it
;; may be because the focus is on editing text or reading through the
;; buffer's contents, so the current line highlight is more of a gentle
;; reminder of the point's location on the vertical axis.
;;
;; `lin-mode' only shows its effect when `hl-line-mode' is active or,
;; more specifically, when the `hl-line' face is used in the buffer.
;; `lin-mode' DOES NOT activate `hl-line-mode' and does not do anything
;; other than the aforementioned face remapping.
;;
;; Sample usage:
;;
;;    (add-hook 'elfeed-search-mode-hook #'lin-mode)
;;
;; Or for more mode hooks:
;;
;;    (dolist (hook '(elfeed-search-mode-hook notmuch-search-mode-hook))
;;      (add-hook hook #'lin-mode))
;;
;; Else invoke `lin-mode' interactively.
;;
;; Consult the manual for further details.  Or visit the documentation's
;; web page: <https://protesilaos.com/emacs/lin>.

;;; Code:

(require 'face-remap)

(defgroup lin ()
  "Locally remap `hl-line' face for greater flexility."
  :group 'convenience)

(defcustom lin-override-foreground nil
  "Do not override foreground colors.

Set to non-nil to always override the foreground colors on the
current line that is highlighted by `lin-mode'.

This requires `lin-mode' to be restarted wherever it is active.

When this option is nil, the `lin-hl' face is used.  Otherwise
the `lin-hl-override-fg' is applied."
  :type 'boolean
  :group 'lin)

(defface lin-hl
  '((default :foreground unspecified :underline nil :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#b0d8ff")
    (((class color) (min-colors 88) (background dark))
     :background "#103265")
    (t :inherit highlight))
  "Like `lin-hl-override-fg', but does not override foreground color.
Used only when `lin-override-foreground' is nil."
  :group 'lin)

(defface lin-hl-override-fg
  '((default :underline nil :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#b0d8ff" :foreground "#000000")
    (((class color) (min-colors 88) (background dark))
     :background "#103265" :foreground "#ffffff")
    (t :inherit highlight))
  "Face for locally remapped `hl-line' face via `lin-mode'.
Used only when `lin-override-foreground' is non-nil."
  :group 'lin)

(defvar-local lin--cookie nil
  "Cookie returned by `face-remap-add-relative'.")

(defun lin--source-face ()
  "Determine the source face, what to remap."
  (cond
   ((derived-mode-p 'mu4e-headers-mode)
    'mu4e-header-highlight-face)
   (t
    'hl-line)))

(defun lin--dest-face ()
  "Determine the destination face, what source must remap to.
This is controlled by `lin-override-foreground', which see."
  (if lin-override-foreground 'lin-hl-override-fg 'lin-hl))

(define-minor-mode lin-mode
  "Remap `hl-line' face to a local LIN face.
The overall style is controlled by `lin-override-foreground'."
  :local t
  :init-value nil
  (if lin-mode
      (setq lin--cookie
            (face-remap-add-relative (lin--source-face) (lin--dest-face)))
    (face-remap-remove-relative lin--cookie)))

(provide 'lin)

;;; lin.el ends here
