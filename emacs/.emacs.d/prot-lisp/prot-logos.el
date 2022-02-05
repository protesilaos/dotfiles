;;; prot-logos.el --- Extensions for my dotemacs to help read, write, present -*- lexical-binding: t -*-

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
;; Extensions to help me read, write, present.  For use in my Emacs
;; setup: https://protesilaos.com/emacs/dotemacs.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

(defgroup prot-logos ()
  "Setup for reading and presenting text-heavy buffers."
  :group 'files)

(defcustom prot-logos-org-presentation nil
  "Org files should switch to presentation view.
This concerns cases where variable `prot-logos-focus-mode' is set
to non-nil and determines whether headings should be converted
into pseudo slides and indentation be adjusted accordingly."
  :type 'boolean
  :group 'prot-logos)

(defcustom prot-logos-variable-pitch nil
  "Non-programming buffers should switch to `variable-pitch-mode'.
In programming modes the default font is always used, as that is
assumed to be a monospaced typeface."
  :type 'boolean
  :group 'prot-logos)

(defcustom prot-logos-scroll-lock nil
  "Use centred scrolling while in focused view."
  :type 'boolean
  :group 'prot-logos)

(defcustom prot-logos-hidden-modeline nil
  "Hide the modeline."
  :type 'boolean
  :group 'prot-logos)


(defcustom prot-logos-affect-prot-cursor nil
  "Change the cursor style.
This expects the `prot-cursor.el' library."
  :type 'boolean
  :group 'prot-logos)

(defcustom prot-logos-prettify-line-breaks nil
  "Change ^L to `prot-logos-line-break'."
  :type 'boolean
  :group 'prot-logos)

;; Based on Paul W. Rankin's idea that was shared on the
;; emacs-humanities mailing list:
;; <https://lists.gnu.org/archive/html/emacs-humanities/2021-01/msg00011.html>.
(defvar prot-logos-line-break (vconcat (make-string 30 ?\s) "* * *")
  "Style for line break override.")

(defvar prot-logos--focus-mode-hook nil
  "Hook that runs from function `prot-logos-focus-mode'.")

;;;###autoload
(define-minor-mode prot-logos-focus-mode
  "Buffer-local wrapper mode for presentations.
Other tools should hook into `prot-logos--focus-mode-hook' to
introduce their effects.  Otherwise this minor mode has no effect
on its own."
  :init-value nil
  :global nil
  :lighter " -Λ-"           ; greek lambda majuscule
  (run-hooks 'prot-logos--focus-mode-hook))

(autoload 'buffer-face-mode "face-remap")
(autoload 'variable-pitch-mode "face-remap")

(defun prot-logos--variable-pitch-toggle ()
  "Make text use `variable-pitch' face, except for programming."
  (when (and prot-logos-variable-pitch
             (derived-mode-p 'text-mode))
    (if (or (bound-and-true-p buffer-face-mode)
            (not (bound-and-true-p prot-logos-focus-mode)))
        (variable-pitch-mode -1)
      (variable-pitch-mode 1))))

(add-hook 'prot-logos--focus-mode-hook #'prot-logos--variable-pitch-toggle)

(autoload 'olivetti-mode "olivetti")

(defun prot-logos--olivetti-toggle ()
  "Toggle the variable `olivetti-mode', if available."
  (if (or (bound-and-true-p olivetti-mode)
          (not (bound-and-true-p prot-logos-focus-mode)))
      (olivetti-mode -1)
    (olivetti-mode 1)))

(add-hook 'prot-logos--focus-mode-hook #'prot-logos--olivetti-toggle)

(defun prot-logos--line-break ()
  "Make ^L use `prot-logos-line-break'."
  (if (and (bound-and-true-p prot-logos-focus-mode)
           prot-logos-prettify-line-breaks)
      ;; TODO 2022-02-05: Maybe setq-local is better?  If yes, we need a
      ;; `defvar' cookie to store its value.
      (aset (or standard-display-table
                (setq standard-display-table (make-display-table)))
            ?\^L prot-logos-line-break)
    (setq standard-display-table nil)))

(add-hook 'prot-logos--focus-mode-hook #'prot-logos--line-break)

(defface prot-logos-fringe
  `((((class color) (background light))
     :background "#ffffff")
    (((class color) (background dark))
     :background "#000000")
    (t :background ,(face-attribute 'default :background)))
  "Face to remove background from fringes.
Only tested with the Modus themes.")

(defvar-local prot-logos--fringe-cookie nil
  "Cookie returned by `face-remap-add-relative'.")

(declare-function face-remap-add-relative "face-remap" (face &rest specs))
(declare-function face-remap-remove-relative "face-remap" (cookie))

(defun prot-logos--fringe-toggle ()
  "Toggle fringe visibility."
  (if (bound-and-true-p prot-logos-focus-mode)
      (progn
        (set-window-fringes (selected-window) 0 0)
        (setq prot-logos--fringe-cookie
              (face-remap-add-relative 'olivetti-fringe 'prot-logos-fringe)))
    (set-window-fringes (selected-window) nil)
    (face-remap-remove-relative prot-logos--fringe-cookie)))

(add-hook 'prot-logos--focus-mode-hook #'prot-logos--fringe-toggle)

(autoload 'org-tree-slide-mode "org-tree-slide")

(defun prot-logos--org-tree-slide-mode ()
  "Toggle variable `org-tree-slide-mode' if loaded and needed."
  (let* ((buf (window-buffer (get-mru-window)))
         (mode (with-current-buffer buf major-mode)))
    (when (and prot-logos-org-presentation
               (eq mode 'org-mode))
      (if (or (bound-and-true-p org-tree-slide-mode)
              (not (bound-and-true-p prot-logos-focus-mode)))
          (org-tree-slide-mode -1)
        (org-tree-slide-mode 1)))))

(add-hook 'prot-logos--focus-mode-hook #'prot-logos--org-tree-slide-mode)

(autoload 'org-indent-mode "org")

(defun prot-logos--org-indent-mode ()
  "Toggle variable `org-tree-slide-mode' if loaded and needed."
  (let* ((buf (window-buffer (get-mru-window)))
         (mode (with-current-buffer buf major-mode)))
    (when (and prot-logos-org-presentation
               (eq mode 'org-mode))
      (if (or (bound-and-true-p org-indent-mode)
              (not (bound-and-true-p prot-logos-focus-mode)))
          (org-indent-mode -1)
        (org-indent-mode 1)))))

(add-hook 'prot-logos--focus-mode-hook #'prot-logos--org-indent-mode)

(defun prot-logos--scroll-lock ()
  "Keep the point at the centre."
  (when prot-logos-scroll-lock
    (if (or (bound-and-true-p scroll-lock-mode)
            (not (bound-and-true-p prot-logos-focus-mode)))
        (scroll-lock-mode -1)
      (recenter nil)
      (scroll-lock-mode 1))))

(add-hook 'prot-logos--focus-mode-hook #'prot-logos--scroll-lock)

;; Based on Paul W. Rankin's code:
;; https://gist.github.com/rnkn/a522429ed7e784ae091b8760f416ecf8
(defun prot-logos--hidden-modeline ()
  "Toggle mode line visibility."
  (when prot-logos-hidden-modeline
    (if (or (null mode-line-format)
            (not (bound-and-true-p prot-logos-focus-mode)))
        (kill-local-variable 'mode-line-format)
      (setq-local mode-line-format nil)
      (force-mode-line-update))))

(add-hook 'prot-logos--focus-mode-hook #'prot-logos--hidden-modeline)

(defvar prot-logos--prot-cursor-preset nil
  "Current `prot-cursor-presets' preset.")

(defvar prot-cursor--style-hist)
(defvar prot-cursor--recovered-preset)
(declare-function prot-cursor-set-cursor "prot-cursor")

(defun prot-logos--prot-cursor-preset ()
  "Change cursor style using `prot-cursor.el'."
  (when prot-logos-affect-prot-cursor
    (if (bound-and-true-p prot-logos-focus-mode)
        (progn
          (cond
           (prot-cursor--style-hist
            (setq prot-logos--prot-cursor-preset
                  (intern (car prot-cursor--style-hist))))
           (prot-cursor--recovered-preset
            (setq prot-logos--prot-cursor-preset
                  prot-cursor--recovered-preset)))
          (prot-cursor-set-cursor 'box))
      (prot-cursor-set-cursor prot-logos--prot-cursor-preset))))

(add-hook 'prot-logos--focus-mode-hook #'prot-logos--prot-cursor-preset)

(provide 'prot-logos)
;;; prot-logos.el ends here
