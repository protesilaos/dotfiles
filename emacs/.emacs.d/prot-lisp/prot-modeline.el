;;; prot-modeline.el --- Code for my custom mode line -*- lexical-binding: t -*-

;; Copyright (C) 2023  Protesilaos Stavrou

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos.com/emacs/dotemacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))

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
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

(defface prot-modeline-intense
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :background "#0000aa" :foreground "#ffffff")
    (((class color) (min-colors 88) (background dark))
     :background "#77aaff" :foreground "#000000")
    (t :inverse-video t))
  "Face for intense mode line constructs.")

(setq mode-line-defining-kbd-macro
      (propertize " KMacro " 'face 'prot-modeline-intense))

(defvar prot-modeline-buffer-identification
  (propertized-buffer-identification "%b")
  "Mode line construct for identifying the buffer being displayed.")

(defvar prot-modeline--line-and-column
  `((line-number-mode
     (column-number-mode
      (column-number-indicator-zero-based
       (:propertize
        mode-line-position-column-line-format
        ,@mode-line-position--column-line-properties)
       (:propertize
        (:eval (string-replace
                "%c" "%C" (car mode-line-position-column-line-format)))
        ,@mode-line-position--column-line-properties))
      (:propertize
	   mode-line-position-line-format
       ,@mode-line-position--column-line-properties))
     (column-number-mode
      (:propertize
       mode-line-position-column-format
       ,@mode-line-position--column-line-properties)
      (:propertize
       (:eval (string-replace
               "%c" "%C" (car mode-line-position-column-format)))
       ,@mode-line-position--column-line-properties)))
    " "
    (:propertize
     ("" mode-line-percent-position)
     mouse-face mode-line-highlight)
    " ")
  "Mode line construct for formatting `prot-modeline-position'.")

(defvar prot-modeline-position
  '(:eval
    (when (mode-line-window-selected-p)
      prot-modeline--line-and-column))
  "Mode line construct for the buffer position.")

(defvar prot-modeline-modes
  (list (propertize "%[" 'face 'error)
        '(:eval
          (propertize
           (capitalize
            (replace-regexp-in-string
             "-mode"
             ""
             (symbol-name major-mode)))
           'mouse-face 'mode-line-highlight))
        '("" mode-line-process)
        (propertize "%]" 'face 'error)
        " ")
  "Mode line construct for displaying major modes.")

(defvar prot-modeline-align-right
  '(:eval (propertize
           " " 'display
           `((space :align-to
                    (- (+ right right-fringe right-margin)
                       ,(string-width
                         (format-mode-line mode-line-misc-info)))))))
  "Mode line construct to align following elements to the right.
Read Info node `(elisp) Pixel Specification'.")

(defvar prot-modeline-kbd-macro
  '(:eval
    (when (and defining-kbd-macro (mode-line-window-selected-p))
      mode-line-defining-kbd-macro))
  "Mode line construct displaying `mode-line-defining-kbd-macro'.
Specific to the current window's mode line.")

(defvar prot-modeline-flymake
  '(:eval
    (when (and (bound-and-true-p flymake-mode)
               (mode-line-window-selected-p))
      flymake-mode-line-format))
  "Mode line construct displaying `flymake-mode-line-format'.
Specific to the current window's mode line.")

(defvar prot-modeline-misc-info
  '(:eval
    (when (mode-line-window-selected-p)
      mode-line-misc-info))
  "Mode line construct displaying `mode-line-misc-info'.
Specific to the current window's mode line.")

(defvar prot-modeline-vc-branch
  '(:eval
    (when-let* (((mode-line-window-selected-p))
                (branches (vc-git-branches))
                (branch (car branches))
                (state (vc-state (buffer-file-name) 'Git))
                (face (pcase state
                        ('added 'vc-locally-added-state)
                        ('edited 'vc-edited-state)
                        ('removed 'vc-removed-state)
                        ('missing 'vc-missing-state)
                        ('conflict 'vc-conflict-state)
                        ('locked 'vc-locked-state)
                        (_ 'vc-up-to-date-state))))
      (propertize (capitalize branch) 'face face)))
  "Mode line construct to return propertized VC branch.")

;; NOTE 2023-04-28: The `risky-local-variable' is critical, as those
;; variables will not work without it.
(dolist (construct '( prot-modeline-modes prot-modeline-align-right
                      prot-modeline-kbd-macro prot-modeline-flymake
                      prot-modeline-vc-branch prot-modeline-misc-info
                      prot-modeline-buffer-identification
                      prot-modeline-position))
  (put construct 'risky-local-variable t))

(provide 'prot-modeline)
;;; prot-modeline.el ends here
