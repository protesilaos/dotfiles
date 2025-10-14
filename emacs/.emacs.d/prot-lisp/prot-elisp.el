;;; prot-elisp.el --- Emacs Lisp extras for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2025  Protesilaos Stavrou

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos.com/emacs/dotemacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))

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
;; Extensions for Emacs Lisp, intended for use in my Emacs setup:
;; https://protesilaos.com/emacs/dotemacs.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

;;;###autoload
(defun prot-elisp-eval-this-expression (expression)
  "Do `eval-expression' on EXPRESSION.
If EXPRESSION is not a list and is the symbol of a function, then make
it a list before evaluation.

If EXPRESSION is nil, prompt for one."
  (interactive (list (read (thing-at-point 'sexp :no-properties))))
  (if expression
      (eval-expression expression)
    (call-interactively 'eval-expression)))

;;;###autoload
(defun prot-elisp-eval-and-print-last-sexp ()
  "Evaluate and print expression before point like `eval-print-last-sexp'.
Prepend a comment to the return value.  If there is no expression before
point, prompt for one, print it at point, and then prepare its return as
mentioned."
  (declare (interactive-only t))
  (interactive)
  (let* ((string (thing-at-point 'sexp :no-properties))
         (comment-p (string-prefix-p ";" string))
         (expression (if comment-p
                         (read--expression "Nothing at point; read expression: ")
                       (read string))))
    (when comment-p
      (insert (format "%S" expression)))
    (insert (format "\n;; => %s\n" (eval expression)))))

(define-derived-mode prot-elisp-macroexpand-mode emacs-lisp-mode "MacroExpand"
  "Like `emacs-lisp-mode' but for macroexpanded forms."
  :interactive nil
  (read-only-mode 1)
  (display-line-numbers-mode 1))

;;;###autoload
(defun prot-elisp-pp-last-sexp ()
  "Like `pp-macroexpand-last-sexp' but with a generic `display-buffer'.
Now use `display-buffer-alist' like the Lisp gods intended."
  (declare (interactive-only t))
  (interactive)
  (if-let* ((thing (thing-at-point 'sexp :no-properties))
            (expression (read thing))
            (buffer (get-buffer-create "*prot-elisp-macroexpand*"))
            (inhibit-read-only t))
      (progn
        (with-current-buffer buffer
          (erase-buffer)
          (insert (format "%s" (macroexpand-1 expression)))
          (prot-elisp-macroexpand-mode)
          (pp-buffer))
        (display-buffer buffer))
    (user-error "No expression to macroexpand")))

(provide 'prot-elisp)
;;; prot-elisp.el ends here
