;;; prot-prism.el --- Tweaks for prism.el -*- lexical-binding: t -*-

;; Copyright (C) 2021  Protesilaos Stavrou

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos.com/emacs/dotemacs
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
;; Tweaks for the `prism.el' library of Adam Porter (alphapapa).
;; Intended for my Emacs setup: <https://protesilaos.com/emacs/dotemacs/>.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

(require 'prism nil t)
(require 'modus-themes nil t)

(defgroup prot-prism ()
  "Tweaks for cursor appearance."
  :group 'faces)

(declare-function modus-themes-with-colors "modus-themes" (&rest body))

(defcustom prot-prism-negative-space-sensitive-modes '(sh-mode yaml-mode)
  "Modes where negative space or indentation indicates depth."
  :type '(repeat symbol)
  :group 'prot-prism)

(defcustom prot-prism-presets-function #'prot-prism--colors
  "Function that returns alist of style presets.
The car of the alist is a number, indicating `prism-num-faces'.
The cdr is a list of strings that represent color names or
values.  The latter use hexadecimal RGB notation."
  :type 'symbol
  :group 'prot-prism)

(defun prot-prism--colors ()
  "Return alist with color presets.
See `prot-prism-presets-function'."
  ;; NOTE 2021-09-18: The `modus-themes-with-colors' is documented at
  ;; length in the themes' manual.
  (modus-themes-with-colors
    `(("4"  . ,(list blue
                     magenta
                     magenta-alt-other
                     green-alt))
      ("8"  . ,(list blue
                     magenta
                     magenta-alt-other
                     cyan-alt-other
                     fg-main
                     blue-alt
                     red-alt-other
                     cyan))
      ("16" . ,(list fg-main
                     magenta
                     cyan-alt-other
                     magenta-alt-other
                     blue
                     magenta-alt
                     cyan-alt
                     red-alt-other
                     green
                     fg-main
                     cyan
                     yellow
                     blue-alt
                     red-alt
                     green-alt-other
                     fg-special-warm)))))

(add-hook 'modus-themes-after-load-theme-hook #'prot-prism--colors)

(defvar prot-prism--preset-hist '()
  "Minibuffer history for `prot-prism-set-colors'.")

(defun prot-prism--set-colors-prompt ()
  "Helper prompt for `prot-prism-set-colors'."
  (let* ((hist prot-prism--preset-hist)
         (default (when hist (nth 0 hist))))
    (completing-read
     (format "Outline style [%s]: " default)
     (mapcar #'car (funcall prot-prism-presets-function))
     nil t nil 'prot-prism--preset-hist default)))

(autoload 'prism-set-colors "prism")
(defvar prism-num-faces)
(declare-function prism-mode "prism")
(declare-function prism-whitespace-mode "prism")

;;;###autoload
(defun prot-prism-set-colors (preset)
  "Set prism colors to PRESET in `prot-prism-presets-function'."
  (interactive (list (prot-prism--set-colors-prompt)))
  (let* ((alist (funcall prot-prism-presets-function))
         (num (car (assoc preset alist)))
         (colors (cdr (assoc preset alist))))
    (setq prism-num-faces (string-to-number num))
    (prism-set-colors
      :desaturations '(0) ; do not change---may lower the contrast ratio
      :lightens '(0)      ; same
      :colors colors)
    (if (member major-mode prot-prism-negative-space-sensitive-modes)
        (prism-whitespace-mode 1)
      (prism-mode 1))
    (add-to-history 'prot-prism--preset-hist num)))

;;;###autoload
(defun prot-prism-disable ()
  "Disable Prism coloration."
  (interactive)
  (if (or (member major-mode prot-prism-negative-space-sensitive-modes)
          (bound-and-true-p prism-whitespace-mode))
      (prism-whitespace-mode -1)
    (prism-mode -1)))

(provide 'prot-prism)
;;; prot-prism.el ends here
