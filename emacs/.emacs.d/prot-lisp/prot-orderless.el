;;; prot-orderless.el --- Extensions for Orderless -*- lexical-binding: t -*-

;; Copyright (C) 2020-2023  Protesilaos Stavrou

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
;; Extensions for the Orderless completion style for use in my Emacs
;; setup: <https://protesilaos.com/emacs/dotemacs/>.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

;;;; Style dispatchers

(defun prot-orderless-literal (word _index _total)
  "Read WORD= as a literal string."
  (when (string-suffix-p "=" word)
    ;; The `orderless-literal' is how this should be treated by
    ;; orderless.  The `substring' form omits the `=' from the
    ;; pattern.
    `(orderless-literal . ,(substring word 0 -1))))

(defun prot-orderless-file-ext (word _index _total)
  "Expand WORD. to a file suffix when completing file names."
  (when (and minibuffer-completing-file-name
             (string-suffix-p "." word))
    `(orderless-regexp . ,(format "\\.%s\\'" (substring word 0 -1)))))

(defun prot-orderless-beg-or-end (word _index _total)
  "Expand WORD~ to \\(^WORD\\|WORD$\\)."
  (when-let (((string-suffix-p "~" word))
             (word (substring word 0 -1)))
    `(orderless-regexp . ,(format "\\(^%s\\|%s$\\)" word word))))

(provide 'prot-orderless)
;;; prot-orderless.el ends here
