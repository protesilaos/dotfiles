;;; prot-pulse.el --- Extend pulse.el for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2020  Protesilaos Stavrou

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos.com/dotemacs
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
;; Extensions to the built-in `pulse.el' library for my Emacs
;; configuration: <https://protesilaos.com/dotemacs/>.

;;; Code:

(require 'pulse)

(defgroup prot-pulse ()
  "Extensions for `pulse.el'."
  :group 'editing)

(defface prot-pulse-line
  '((default :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#ffccff" :foreground "#770077")
    (((class color) (min-colors 88) (background dark))
     :background "#71206a" :foreground "#ffcaf0")
    (t :inverse-video t))
  "Default face `prot-pulse-pulse-line'."
  :group 'prot-pulse)

;;;###autoload
(defun prot-pulse-pulse-line (&optional face)
  "Temporarily highlight the current line with optional FACE."
  (interactive)
  (let ((start (if (eobp)
                   (line-beginning-position 0)
                 (line-beginning-position)))
        (end (line-beginning-position 2))
        (pulse-delay .04)
        (face (or face 'prot-pulse-line)))
    (pulse-momentary-highlight-region start end face)))

(provide 'prot-pulse)
;;; prot-pulse.el ends here
