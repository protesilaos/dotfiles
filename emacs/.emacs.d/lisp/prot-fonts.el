;;; prot-fonts.el --- Font configurations for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2020  Protesilaos Stavrou

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos.com/dotemacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3"))

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
;; This set of configurations pertains to my font settings, for use in
;; my Emacs setup: https://protesilaos.com/dotemacs.
;;
;; Note that this package "requires" Emacs 24.3 or higher, though I only
;; tested it with versions 27 and 28.

;;; Code:

;;; Customisation options
(defgroup prot-fonts ()
  "Font-related configurations for my dotemacs."
  :group 'font)

;; NOTE: "Hack" and "Iosevka Comfy" are, in my case, personal builds of
;; Hack and Iosevka respectively:
;;
;; 1. https://gitlab.com/protesilaos/hack-font-mod
;; 2. https://gitlab.com/protesilaos/iosevka-comfy
(defcustom prot-fonts-typeface-sets-alist
  '((laptop . (105 "Hack" "DejaVu Sans"))
    (desktop . (110 "Hack" "Inter"))
    (reader . (150 "Iosevka Comfy" "FiraGO"))
    (presentation . (190 "Iosevka Comfy" "FiraGO")))
  "Alist of desired typefaces and their point sizes.

Each association consists of an arbitrary display type or context
mapped to a font height (as and integer that is 10x the point
size), followed by the family names (as strings) of mono and
proportionately spaced typefaces.  The latter two are assigned to
the `fixed-pitch' and `variable-pitch' faces respectively.

It is assumed that those typefaces already exist on the system,
otherwise an error will be displayed when trying to set them."
  :group 'prot-fonts
  :type 'alist)

(defcustom prot-fonts-monospaced-list
  '("Hack" "DejaVu Sans Mono" "Iosevka Comfy" "Source Code Pro"
    "Ubuntu Mono" "Fantasque Sans Mono" "Fira Code" "Monoid")
  "List of typefaces for coding.

It is assumed that those already exist on the system, otherwise
an error will be displayed when trying to set one of them."
  :group 'prot-fonts
  :type 'list)

(defcustom prot-fonts-heights-list
  '(100 105 110 120 130 140 150 160)
  "List of font heights for `prot-fonts-set-font-size-family'."
  :group 'prot-fonts
  :type 'list)

(defcustom prot-fonts-line-spacing-alist
  '(("Source Code Pro" . 1)
    ("Ubuntu Mono" . 2))
  "Font families in need of extra line spacing.

The alist defines a font family as a string and the desired
integer to pass to the `line-spacing' variable."
  :group 'prot-fonts
  :type 'alist)

(defcustom prot-fonts-laptop-desktop-keys-list '(laptop desktop)
  "Symbols for `prot-fonts-fonts-per-monitor'.
This is a list whose first item denotes the smallest desirable
entry in `prot-fonts-typeface-sets-alist' for use on a laptop or
just smaller monitor, while the second points to a larger
display's key in that same alist."
  :group 'prot-fonts
  :type 'list)

(defcustom prot-fonts-max-small-resolution-width 1366
  "Maximum width for use in `prot-fonts-fonts-per-monitor'."
  :group 'prot-fonts
  :type 'integer)

(defcustom prot-fonts-bold-weight-alist
  '(("Iosevka Comfy" . semibold)
    ("Source Code Pro" . semibold))
  "Font families in need of a different weight for `bold'.

The alist defines a font family as a string and the desired style
to pass to the `bold' face's weight property."
  :group 'prot-fonts
  :type 'alist)

;;; Variables

(defvar prot-fonts-set-typeface-hook nil
  "Hook that is called after setting fonts.")

;;; Functions

(defun prot-fonts--set-face-attribute (face family)
  "Set FACE font to FAMILY."
  (set-face-attribute `,face nil :family (format "%s" family)))

;;;###autoload
(defun prot-fonts-set-fonts (&optional height font-mono font-var)
  "Set default font size using presets.

HEIGHT is the font's height as 10x its point size.  FONT-MONO
should be a monospaced typeface, due to the alignment
requirements of the `fixed-pitch' face.  FONT-VAR could be a
proportionately spaced typeface or even a monospaced one, since
the `variable-pitch' it applies to is not supposed to be
spacing-sensitive.  Both families must be represented as a string
holding the family's name."
  (interactive)
  (if window-system
      (let* ((data prot-fonts-typeface-sets-alist)
             (displays (mapcar #'car prot-fonts-typeface-sets-alist))
             (display-strings (mapcar (lambda (x)
                                        (format "%s" (car x)))
                                      prot-fonts-typeface-sets-alist))
             (choice (or height
                         (intern
                          (completing-read "Pick display size: "
                                           display-strings nil t))))
             (size (or height
                       (nth 1 (assoc choice data))))
             (mono (or font-mono
                       (if (member choice displays)
                           (nth 2 (assoc choice data))
                         nil)))
             (var (or font-var
                      (if (member choice displays)
                          (nth 3 (assoc choice data))
                        nil))))
        (set-face-attribute 'default nil :family mono :height size)
        (prot-fonts--set-face-attribute 'fixed-pitch mono)
        (prot-fonts--set-face-attribute 'variable-pitch var)
        (run-hooks 'prot-fonts-set-typeface-hook))
    (user-error "Not running a graphical Emacs; cannot set fonts")))

;;;###autoload
(defun prot-fonts-set-font-size-family ()
  "Set point size and main typeface.
This command is mostly intended for testing typefaces defined in
`prot-fonts-monospaced-list' at common heights specified in
`prot-fonts-heights-list'."
  (interactive)
  (if window-system
      (let* ((fonts prot-fonts-monospaced-list)
             (font (completing-read "Select main font: " fonts nil t))
             (nums prot-fonts-heights-list)
             (sizes (mapcar 'number-to-string nums))
             (size (completing-read "Select or insert number: " sizes nil))
             (var (face-attribute 'variable-pitch :family)))
        (set-face-attribute 'default nil :family font :height (string-to-number size))
        (set-face-attribute 'fixed-pitch nil :family font)
        (prot-fonts--set-face-attribute 'variable-pitch var)
        (run-hooks 'prot-fonts-set-typeface-hook))
    (user-error "Not running a graphical Emacs; cannot set fonts")))

;;;###autoload
(defun prot-fonts-set-fonts-dwim (&optional arg)
  "Set fonts interactively.
With optional prefix ARG (\\[universal-argument]) call
`prot-fonts-set-font-size-family' else default to
`prot-fonts-set-fonts'.

This is just a wrapper around `prot-fonts-set-fonts' and
`prot-fonts-set-font-size-family', whose sole purpose is to
economise on dedicated key bindings."
  (interactive "P")
  (if arg
      (prot-fonts-set-font-size-family)
    (prot-fonts-set-fonts)))

(defmacro prot-fonts--font-adjustment (fn doc alist cond1 cond2)
  "Macro for functions that employ `prot-fonts-set-typeface-hook'.
FN is the name of the resulting function.  DOC is its docstring.
ALIST is an assosiation list of cons cells.  COND1 and COND2 is
the body of an `if' statement's 'if' and 'then' part
respectively."
  `(defun ,fn ()
     ,doc
     (let* ((data ,alist)
            (fonts (mapcar #'car data))
            (font (face-attribute 'default :family))
            (x (cdr (assoc font data))))
       (if (member font fonts)
           ,cond1
         ,cond2))))

(prot-fonts--font-adjustment
 prot-fonts-line-spacing
 "Determine desirable `line-spacing', based on font family."
 prot-fonts-line-spacing-alist
 (setq-default line-spacing `,x)
 (setq-default line-spacing nil))

;; XXX: This will not work with every theme, but only those that
;; inherit the `bold' face instead of specifying a weight property.
;; The intent is to configure this once and have it propagate wherever
;; a heavier weight is displayed.  My Modus themes handle this
;; properly.
(prot-fonts--font-adjustment
 prot-fonts-bold-face
 "Determine weight for the `bold' face, based on font family."
 prot-fonts-bold-weight-alist
 (set-face-attribute 'bold nil :weight `,x)
 (set-face-attribute 'bold nil :weight 'bold))

(defun prot-fonts--display-type-for-monitor (&optional smaller larger)
  "Determine typeface specs based on monitor width.
Optional SMALLER and LARGER are two keys that point to entries in
`prot-fonts-typeface-sets-alist'.  The default uses the relevant
keys from `prot-fonts-laptop-desktop-keys-list'."
  (let* ((keys prot-fonts-laptop-desktop-keys-list)
         (face-specs prot-fonts-typeface-sets-alist)
         (small (or smaller (nth 0 keys)))
         (large (or larger (nth 1 keys)))
         (max-width prot-fonts-max-small-resolution-width)
         (spec (if (<= (display-pixel-width) max-width)
                   small
                 large)))
    (unless (assoc spec face-specs)
      (error (concat "Key <<%s>> in `prot-fonts-laptop-desktop-keys-list' "
                     "does not reference anything in "
                     "`prot-fonts-typeface-sets-alist'")
             spec))
    spec))

;;;###autoload
(defun prot-fonts-fonts-per-monitor ()
  "Use font settings based on screen size."
  (when window-system
    (let* ((display (prot-fonts--display-type-for-monitor))
           (data prot-fonts-typeface-sets-alist)
           (size (cadr (assoc `,display data)))
           (mono (nth 2 (assoc `,display data)))
           (var (nth 3 (assoc `,display data))))
      (set-face-attribute 'default nil :family mono :height size)
      (prot-fonts--set-face-attribute 'fixed-pitch mono)
      (prot-fonts--set-face-attribute 'variable-pitch var))
    (run-hooks 'prot-fonts-set-typeface-hook)))

(provide 'prot-fonts)
;;; prot-fonts.el ends here
