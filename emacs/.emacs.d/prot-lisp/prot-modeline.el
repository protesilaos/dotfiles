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
  '((t :inherit (bold highlight)))
  "Face for intense mode line constructs.")

(setq mode-line-defining-kbd-macro
      (propertize " KMacro " 'face 'prot-modeline-intense))

(defvar prot-modeline-modes
  (list (propertize "%[" 'face 'error)
        `(:propertize ("" mode-name)
                      mouse-face mode-line-highlight
                      local-map ,mode-line-major-mode-keymap)
        '("" mode-line-process)
        (propertize "%]" 'face 'error)
        " ")
  "Mode line construct for displaying major modes.")

(defface prot-modeline-border-active
  '((t :inherit (variable-pitch font-lock-keyword-face) :inverse-video t))
  "Face for active mode line border (`prot-modeline-border').")

(defface prot-modeline-border-inactive
  '((t :inherit variable-pitch))
  "Face for inactive mode line border (`prot-modeline-border').")

(defvar prot-modeline-border
  `(:eval
    (propertize " " 'face
                (if (mode-line-window-selected-p)
                    'prot-modeline-border-active
                  'prot-modeline-border-inactive)))
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
  '(:eval (when (and defining-kbd-macro (mode-line-window-selected-p))
            mode-line-defining-kbd-macro))
  "Mode line construct displaying `mode-line-defining-kbd-macro'.
Specific to the current window's mode line.")

(defvar prot-modeline-flymake
  '(:eval (when flymake-mode
            flymake-mode-line-format))
  "Mode line construct displaying `flymake-mode-line-format'.
Specific to the current window's mode line.")

(defvar prot-modeline-misc-info
  '(:eval
    (when (mode-line-window-selected-p)
      mode-line-misc-info))
  "Mode line construct displaying `mode-line-misc-info'.
Specific to the current window's mode line.")

;; NOTE 2023-04-28: The `risky-local-variable' is critical, as those
;; variables will not work without it.
(dolist (construct '( prot-modeline-modes prot-modeline-align-right
                      prot-modeline-kbd-macro prot-modeline-flymake
                      prot-modeline-misc-info prot-modeline-border))
  (put construct 'risky-local-variable t))

(provide 'prot-modeline)
;;; prot-modeline.el ends here
