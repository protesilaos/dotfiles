;;; fontaine.el --- Set font configurations using presets -*- lexical-binding: t -*-

;; Copyright (C) 2022  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://git.sr.ht/~protesilaos/fontaine
;; Mailing list: https://lists.sr.ht/~protesilaos/fontaine
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
;; Fontaine lets the user specify presets of font configurations and set
;; them on demand.  The user option `fontaine-presets' holds all such
;; presets.
;;
;; Presets consist of a list of properties that govern the family, weight,
;; and height of the faces `default', `fixed-pitch', `variable-pitch',
;; `bold', and `italic'.  Each preset is identified by a user-defined
;; symbol as the car of a property list.  It looks like this (check the
;; default value of `fontaine-presets' for how everything is pieced
;; together):
;;
;;     (regular
;;      :default-family "Hack"
;;      :default-weight normal
;;      :default-height 100
;;      :fixed-pitch-family "Fira Code"
;;      :fixed-pitch-weight nil ; falls back to :default-weight
;;      :fixed-pitch-height 1.0
;;      :variable-pitch-family "Noto Sans"
;;      :variable-pitch-weight normal
;;      :variable-pitch-height 1.0
;;      :bold-family nil ; use whatever the underlying face has
;;      :bold-weight bold
;;      :italic-family "Source Code Pro"
;;      :italic-slant italic
;;      :line-spacing 1)
;;
;; The doc string of `fontaine-presets' explains all properties in detail.
;;
;; The command `fontaine-set-preset' applies the desired preset.  If there
;; is only one available, it implements it outright.  Otherwise it produces
;; a minibuffer prompt with completion among the available presets.  When
;; called from Lisp, the `fontaine-set-preset' requires a PRESET argument,
;; such as:
;;
;;     (fontaine-set-preset 'regular)
;;
;; The command `fontaine-set-face-font' prompts with completion for a face
;; and then asks the user to specify the value of the relevant properties.
;; The list of faces to choose from is the same as that implied by the
;; `fontaine-presets'.  Properties to change and their respective values
;; will depend on the face.  For example, the `default' face requires a
;; natural number for its height attribute, whereas every other face needs
;; a floating point (understood as a multiple of the default height).  This
;; command is for interactive use only and is supposed to be used for
;; previewing certain styles before eventually codifying them as presets.
;;
;; As for the name of this package, it is the French word for "fountain"
;; which, in turn, is what the font or source is.  However, I will not
;; blame you if you can only interpret it as a descriptive acronym: FONTs
;; Are Irrelevant in Non-graphical Emacs (because that is actually true).

;;; Code:

(defgroup fontaine ()
  "Set font configurations using presets."
  :group 'font)

(defconst fontaine--weights-widget
  '(choice
    (const :tag "Normal" normal)
    (const :tag "Regular (same as normal)" regular)
    (const :tag "Thin" thin)
    (const :tag "Ultra-light" ultralight)
    (const :tag "Extra-light" extralight)
    (const :tag "Light" light)
    (const :tag "Semi-light" semilight)
    (const :tag "Medium" medium)
    (const :tag "Semi-bold" semibold)
    (const :tag "Bold" bold)
    (const :tag "Extra-bold" extrabold)
    (const :tag "Ultra-bold" ultrabold))
  "Widget with font weights for `fontaine-presets'.")

(defcustom fontaine-presets
  '((regular
     :default-family "Hack"
     :default-weight normal
     :default-height 100
     :fixed-pitch-family "Fira Code"
     :fixed-pitch-weight nil ; falls back to :default-weight
     :fixed-pitch-height 1.0
     :variable-pitch-family "Noto Sans"
     :variable-pitch-weight normal
     :variable-pitch-height 1.0
     :bold-family nil ; use whatever the underlying face has
     :bold-weight bold
     :italic-family "Source Code Pro"
     :italic-slant italic
     :line-spacing 1)
    (large
     :default-family "Iosevka"
     :default-weight normal
     :default-height 150
     :fixed-pitch-family nil ; falls back to :default-family
     :fixed-pitch-weight nil ; falls back to :default-weight
     :fixed-pitch-height 1.0
     :variable-pitch-family "FiraGO"
     :variable-pitch-weight normal
     :variable-pitch-height 1.05
     :bold-family nil ; use whatever the underlying face has
     :bold-weight bold
     :italic-family nil ; use whatever the underlying face has
     :italic-slant italic
     :line-spacing 1))
  "Alist of desired typographic properties.

The car of each cell is an arbitrary symbol that identifies
and/or describes the set of properties (e.g. 'small', 'reading').

The cdr is a plist that specifies the typographic properties of
the faces `default', `fixed-pitch', `variable-pitch', `bold', and
`italic'.  It also covers the `line-spacing' variable.

The properties in detail:

- `:default-family' is the family of the `default' face.  If not
  specified, it falls back to Monospace.

- `:default-weight' is the weight of the `default' face.  The
  fallback value is `normal'.  Available weights are `normal' or
  `regular', `thin', `ultralight', `extralight', `light',
  `semilight', `medium', `semibold', `extrabold', `ultrabold' and
  must be supported by the underlying typeface.

- `:default-height' is the height of the `default' face.  The
  fallback value is 100 (the height is 10x the point size).

- `:fixed-pitch-family', `:fixed-pitch-weight',
  `:fixed-pitch-height' apply to the `fixed-pitch' face.  Their
  fallback values are `:default-family', `:default-weight', and
  1.0, respectively.

- The `:variable-pitch-family', `:variable-pitch-weight', and
  `:variable-pitch-height' apply to the `variable-pitch' face.
  They all fall back to the respective default values, as
  described above.

- The `:bold-family' and `:italic-family' are the font families
  of the `bold' and `italic' faces, respectively.  Only set them
  if you want to override that of the underlying face.

- The `:bold-weight' specifies the weight of the `bold' face.
  Its fallback value is bold, meaning the weight, not the face.

- The `:italic-slant' specifies the slant of the `italic' face.
  Its fallback value is italic, in reference to the slant, not
  the face.  Acceptable values are `italic', `oblique', `normal',
  `reverse-italic', `reverse-oblique' and must be supported by
  the underlying typeface.

- The `:line-spacing' specifies the value of the `line-spacing'
  variable.

Note that all the properties for `bold' and `italic' will only
have a noticeable effect if the active theme does not hardcode a
weight and a slant, but instead inherits the relevant face (such
as the `modus-themes').

Also note that a height attribute for anything other than the
`default' face must be set to a floating point, which is
understood as a multiple of the default height (this allows all
faces to scale harmoniously).  The `:default-height' always is a
natural number.

Use the desired preset with the command `fontaine-set-preset'."
  :group 'fontaine
  :type `(alist
          :value-type
          (plist :options
                 (((const :tag "Default font family" :default-family) string)
                  ((const :tag "Default weight" :default-weight) ,fontaine--weights-widget)
                  ((const :tag "Default height" :default-height) natnum)

                  ((const :tag "Fixed pitch font family" :fixed-pitch-family) string)
                  ((const :tag "Fixed pitch regular weight" :fixed-pitch-weight) ,fontaine--weights-widget)
                  ((const :tag "Fixed pitch height" :fixed-pitch-height) float)

                  ((const :tag "Variable pitch font family" :variable-pitch-family) string)
                  ((const :tag "Variable pitch regular weight" :variable-pitch-weight) ,fontaine--weights-widget)
                  ((const :tag "Variable pitch height" :variable-pitch-height) float)

                  ((const :tag "Font family of the `bold' face" :bold-family) string)
                  ((const :tag "Weight for the `bold' face" :bold-weight) ,fontaine--weights-widget)

                  ((const :tag "Font family of the `italic' face" :italic-family) string)
                  ((const :tag "Slant for the `italic' face" :italic-slant)
                   (choice
                    (const italic)
                    (const oblique)
                    (const normal)
                    (const reverse-italic)
                    (const reverse-oblique)))

                  ((const :tag "Line spacing" :line-spacing) ,(get 'line-spacing 'custom-type))))
          :key-type symbol))

(defcustom fontaine-latest-state-file
  (locate-user-emacs-file "fontaine-latest-state.eld")
  "File to save the value of `fontaine-set-preset'.
Saving is done by the `fontaine-store-latest-preset' function."
  :type 'file
  :group 'fontaine)

;;;; General utilities

(defun fontaine--set-face-attributes (face family &optional weight height)
  "Set FACE font to FAMILY, with optional HEIGHT and WEIGHT."
  (let ((family (or family "Monospace"))
        (height (or height (if (eq face 'default) 100 1.0)))
        (weight (or weight 'normal)))
    ;; ;; Read this: <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=45920>
    ;; ;; Hence why the following fails.  Keeping it for posterity...
    ;; (set-face-attribute face nil :family family :weight weight :height height)
    (if (eq (face-attribute face :weight) weight)
          (internal-set-lisp-face-attribute face :family family 0)
      (internal-set-lisp-face-attribute face :weight weight 0)
      (internal-set-lisp-face-attribute face :family family 0)
      (internal-set-lisp-face-attribute face :weight weight 0))
    (internal-set-lisp-face-attribute face :height height 0)))

(defun fontaine--set-italic-slant (family slant)
  "Set FAMILY and SLANT of `italic' face."
  (let ((slant (or slant 'italic))
        (family (or family 'unspecified)))
    (if (eq (face-attribute 'italic :slant) slant)
        (internal-set-lisp-face-attribute 'italic :family family 0)
      (internal-set-lisp-face-attribute 'italic :slant slant 0)
      (internal-set-lisp-face-attribute 'italic :family family 0)
      (internal-set-lisp-face-attribute 'italic :slant slant 0))))

;;;; Apply preset configurations

(defun fontaine--apply-default-preset (preset)
  "Set `default' face attributes based on PRESET."
  (if-let ((properties (alist-get preset fontaine-presets)))
      (progn
        (fontaine--set-face-attributes
         'default
         (plist-get properties :default-family)
         (plist-get properties :default-weight)
         (plist-get properties :default-height))
        (setq-default line-spacing (plist-get properties :line-spacing)))
    (user-error "%s is not in `fontaine-presets'" preset)))

(defun fontaine--apply-fixed-pitch-preset (preset)
  "Set `fixed-pitch' face attributes based on PRESET."
  (if-let ((properties (alist-get preset fontaine-presets)))
      (fontaine--set-face-attributes
       'fixed-pitch
       (or (plist-get properties :fixed-pitch-family) (plist-get properties :default-family))
       (or (plist-get properties :fixed-pitch-weight) (plist-get properties :default-weight))
       (or (plist-get properties :fixed-pitch-height) 1.0))
    (user-error "%s is not in `fontaine-presets'" preset)))

(defun fontaine--apply-variable-pitch-preset (preset)
  "Set `variable-pitch' face attributes based on PRESET."
  (if-let ((properties (alist-get preset fontaine-presets)))
      (fontaine--set-face-attributes
       'variable-pitch
       (or (plist-get properties :variable-pitch-family) (plist-get properties :default-family))
       (or (plist-get properties :variable-pitch-weight) (plist-get properties :default-weight))
       (or (plist-get properties :variable-pitch-height) 1.0))
    (user-error "%s is not in `fontaine-presets'" preset)))

(defun fontaine--apply-bold-preset (preset)
  "Set `bold' face attributes based on PRESET."
  (if-let ((properties (alist-get preset fontaine-presets)))
      (fontaine--set-face-attributes
       'bold
       (or (plist-get properties :bold-family) 'unspecified)
       (or (plist-get properties :bold-weight) 'bold))
    (user-error "%s is not in `fontaine-presets'" preset)))

(defun fontaine--apply-italic-preset (preset)
  "Set `italic' face attributes based on PRESET."
  (if-let ((properties (alist-get preset fontaine-presets)))
      (fontaine--set-italic-slant
       (or (plist-get properties :italic-family) 'unspecified)
       (or (plist-get properties :italic-slant) 'italic))
    (user-error "%s is not in `fontaine-presets'" preset)))

(defvar fontaine--font-display-hist '()
  "History of inputs for display-related font associations.")

(defun fontaine--set-fonts-prompt ()
  "Prompt for font set (used by `fontaine-set-fonts')."
  (let* ((def (nth 1 fontaine--font-display-hist))
         (prompt (if def
                     (format "Apply font configurations from PRESET [%s]: " def)
                   "Apply font configurations from PRESET: ")))
    (intern
     (completing-read
      prompt
      fontaine-presets
      nil t nil 'fontaine--font-display-hist def))))

(defvar fontaine-set-preset-hook nil
  "Hook that runs after setting fonts.")

(defvar fontaine--current-preset nil
  "Current font set in `fontaine-presets'.")

;;;###autoload
(defun fontaine-set-preset (preset)
  "Set font configurations specified in PRESET.
PRESET is a symbol that represents the car of a list in
`fontaine-presets'.  If there is only one available, apply it
outright, else prompt with completion."
  (interactive
   (list
    (if (= (length fontaine-presets) 1)
        (caar fontaine-presets)
      (fontaine--set-fonts-prompt))))
  (if (not window-system)
      (user-error "Cannot use this in a terminal emulator; try the Emacs GUI")
    (fontaine--apply-default-preset preset)
    (fontaine--apply-fixed-pitch-preset preset)
    (fontaine--apply-variable-pitch-preset preset)
    (fontaine--apply-bold-preset preset)
    (fontaine--apply-italic-preset preset)
    (setq fontaine--current-preset preset)
    (add-to-history 'fontaine--preset-history (format "%s" preset))
    (run-hooks 'fontaine-set-preset-hook)))

;;;; Modify individual faces

(defconst fontaine--font-faces
  '(default fixed-pitch variable-pitch bold italic)
  "List of faces whose typographic attributes we may change.")

(defconst fontaine--font-weights
  '( normal regular thin ultralight extralight light semilight
     medium semibold bold extrabold ultrabold)
  "List of font weights.")

(defconst fontaine--font-slants
  '( normal oblique italic reverse-oblique reverse-italic)
  "List of font slants.")

(defvar fontaine--face-history '()
  "Minibuffer history of `fontaine-set-face-font'.")

(defvar fontaine--font-family-history '()
  "Minibuffer history of selected font families.")

;; We have `font-family-list', which is unfiltered.
(defun fontaine--family-list-fixed-pitch (&optional frame)
  "Return a list of available monospaced font families.
Target FRAME, if provided as an optional argument."
  (seq-uniq
   (seq-map
    (lambda (fam)
      (symbol-name (aref fam 0)))
    (seq-filter
     (lambda (fam)
       (aref fam 5))
     ;; FIXME 2022-04-26: Is `x-family-fonts' the right call here, or
     ;; will it not work in some Emacs builds?
     ;;
     ;; NOTE 2022-04-26: `x-family-fonts' and `x-list-fonts' accept a
     ;; pattern, but I cannot find how to use it properly to filter out
     ;; certain families.
     (x-family-fonts nil frame)))))

(defun fontaine--family-list-variable-pitch (&optional frame)
  "Return a list of available proportionately spaced font families.
Target FRAME, if provided as an optional argument."
  (seq-uniq
   (seq-map
    (lambda (fam)
      (symbol-name (aref fam 0)))
    (seq-filter
     (lambda (fam)
       (null (aref fam 5)))
     (x-family-fonts nil frame)))))

(defvar fontaine--natnum-history '()
  "Minibuffer history for natural numbers.")

(defun fontaine--set-default ()
  "Set `default' attributes interactively."
  (let ((family (completing-read "Font family of `default' face: "
                                 (font-family-list) nil t
                                 nil 'fontaine--font-family-history))
        (weight (intern (completing-read "Select weight for `default': "
                                         fontaine--font-weights nil)))
        (height (read-number "Height of `default' face (must be a natural number): "
                             nil 'fontaine--natnum-history)))
    (if (natnump height)
        (fontaine--set-face-attributes 'default family weight)
      (user-error "Height of `default' face must be a natural number"))))

(defvar fontaine--float-history '()
  "Minibuffer history for floating point numbers.")

(defun fontaine--set-fixed-pitch ()
  "Set `fixed-pitch' attributes interactively."
  (let ((family (completing-read "Font family of `fixed-pitch': "
                                 (fontaine--family-list-fixed-pitch) nil t
                                 nil 'fontaine--font-family-history))
        (weight (intern (completing-read "Select weight for `fixed-pitch': "
                                         fontaine--font-weights nil)))
        (height (read-number "Height of `fixed-pitch' face (must be a floating point): "
                             1.0 'fontaine--float-history)))
    (if (floatp height)
        (fontaine--set-face-attributes 'fixed-pitch family weight)
      (user-error "Height of `fixed-pitch' face must be a floating point"))))

(defun fontaine--set-variable-pitch ()
  "Set `variable-pitch' attributes interactively."
  (let ((family (completing-read "Font family of `variable-pitch': "
                                 (fontaine--family-list-variable-pitch) nil t
                                 nil 'fontaine--font-family-history))
        (weight (intern (completing-read "Select weight for `variable-pitch': "
                                         fontaine--font-weights nil)))
        (height (read-number "Height of `variable-pitch' face (must be a floating point): "
                             1.0 'fontaine--float-history)))
    (if (floatp height)
        (fontaine--set-face-attributes 'variable-pitch family weight)
      (user-error "Height of `variable-pitch' face must be a floating point"))))

(defun fontaine--set-bold ()
  "Set `bold' attributes interactively."
  (let ((weight (intern (completing-read "Select weight for `bold': "
                                         fontaine--font-weights nil t))))
    (fontaine--set-face-attributes 'bold 'unspecified weight)))

(defun fontaine--set-italic ()
  "Set `italic' attributes interactively."
  (let ((slant (intern (completing-read "Select slant for `italic': "
                                        fontaine--font-slants nil t))))
    (fontaine--set-italic-slant 'unspecified slant)))

;;;###autoload
(defun fontaine-set-face-font (face)
  "Set font and other attributes of FACE.
If FACE is not part of `fontaine--font-faces', fall back to
calling `fontaine-set-preset'.

Changing the `bold' and `italic' faces only has a noticeable
effect if the underlying does not hardcode a weight and slant but
inherits from those faces instead (e.g. the `modus-themes')."
  (declare (interactive-only t))
  (interactive
   (list
    (intern
     (completing-read "Which face to change? "
                      fontaine--font-faces nil t nil
                      'fontaine--face-history))))
  (pcase face
    ('bold (fontaine--set-bold))
    ('default (fontaine--set-default))
    ('fixed-pitch (fontaine--set-fixed-pitch))
    ('italic (fontaine--set-italic))
    ('variable-pitch (fontaine--set-variable-pitch))
    (_ (call-interactively #'fontaine-set-preset))))

;;;; Store and restore preset

(defvar fontaine--preset-history '()
  "Minibuffer history of preset configurations.")

;;;###autoload
(defun fontaine-store-latest-preset ()
  "Write latest cursor state to `fontaine-latest-state-file'.
Can be assigned to `kill-emacs-hook'."
  (when-let ((hist fontaine--preset-history))
    (with-temp-file fontaine-latest-state-file
      (insert ";; Auto-generated file; don't edit -*- mode: "
              (if (<= 28 emacs-major-version)
                  "lisp-data"
                "emacs-lisp")
              " -*-\n")
      (pp (intern (car hist)) (current-buffer)))))

(defvar fontaine-recovered-preset nil
  "Recovered value of latest store cursor preset.")

;;;###autoload
(defun fontaine-restore-latest-preset ()
  "Restore latest preset set by `fontaine-set-preset'."
  (when-let* ((file fontaine-latest-state-file)
              ((file-exists-p file)))
    (setq fontaine-recovered-preset
          (unless (zerop
                   (or (file-attribute-size (file-attributes file))
                       0))
            (with-temp-buffer
              (insert-file-contents file)
              (read (current-buffer)))))))


(provide 'fontaine)
;;; fontaine.el ends here
