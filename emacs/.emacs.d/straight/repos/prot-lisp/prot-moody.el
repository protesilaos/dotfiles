;;; prot-moody.el --- Extensions to moody.el for my dotemacs -*- lexical-binding: t -*-

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
;; This covers my moody.el extensions, for use in my Emacs setup:
;; https://protesilaos.com/dotemacs.

;;; Code:

(require 'prot-common)
(require 'prot-fonts)
(when (featurep 'moody)
  (require 'moody))

(defgroup prot-moody ()
  "Tweaks for moody.el."
  :group 'mode-line)

(defun prot-moody--height ()
  "Set Moody height to an even number.
Bind this to a hook that gets called after loading/changing the
mode line's typeface (or the default one if they are the same)."
  (let* ((font (face-font 'mode-line))
         (height (truncate (* 1.65 (aref (font-info font) 2))))
         (height-even (if (prot-common-number-even-p height) height (+ height 1))))
    (if font
        height-even
      24)))

(defvar moody-mode-line-height)

(defun prot-moody--mode-line-height ()
  "Set Moody height to the value of `prot-moody--height'."
  (let ((height (prot-moody--height)))
    (setq moody-mode-line-height height)))

(declare-function moody-replace-mode-line-buffer-identification "moody")
(declare-function moody-replace-vc-mode "moody")

;;;###autoload
(define-minor-mode prot-moody-set-height
  "Toggle Moody for the mode line and configure its fonts."
  :init-value nil
  :global t
  (if prot-moody-set-height
      (progn
        (moody-replace-mode-line-buffer-identification)
        (moody-replace-vc-mode)
        (add-hook 'prot-fonts-set-typeface-hook #'prot-moody--mode-line-height)
        (run-hooks 'prot-fonts-set-typeface-hook))
    (let ((format (default-value 'mode-line-format)))
      (when (member 'moody-mode-line-buffer-identification format)
        (moody-replace-mode-line-buffer-identification 'reverse))
      (when (member '(vc-mode moody-vc-mode) format)
        (moody-replace-vc-mode 'reverse)))
    (remove-hook 'prot-fonts-set-typeface-hook #'prot-moody--mode-line-height)))

(provide 'prot-moody)
;;; prot-moody.el ends here
