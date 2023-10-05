;;; prot-pair.el --- Insert character pair around symbol or region -*- lexical-binding: t -*-

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
;; Insert character pair around symbol or region using minibuffer
;; completion.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

(defgroup prot-pair nil
  "Insert character pair around symbol or region."
  :group 'editing)

(defcustom prot-pair-pairs
  '((?'  :description "Single quotes"           :pair (?' . ?'))
    (?\" :description "Double quotes"           :pair (?\" . ?\"))
    (?‘  :description "Single curly quotes"     :pair (?‘ . ?’))
    (?“  :description "Double curly quotes"     :pair (?“ . ?”))
    (?\> :description "Natural language quotes" :pair prot-pair-insert-natural-language-quotes)
    (?\( :description "Parentheses"             :pair (?\( . ?\)))
    (?{  :description "Curly brackets"          :pair (?{ . ?}))
    (?\[ :description "Square brackets"         :pair (?\[ . ?\]))
    (?\< :description "Angled brackets"         :pair (?\< . ?\>))
    (?@  :description "At signs"                :pair (?@ . ?@))
    (?=  :description "Equals signs"            :pair (?= . ?=))
    (?+  :description "Plus signs"              :pair (?+ . ?+))
    (?`  :description "Backticks"               :pair prot-pair-insert-backticks)
    (?~  :description "Tildes"                  :pair (?~ . ?~))
    (?*  :description "Asterisks"               :pair (?* . ?*))
    (?/  :description "Forward slashes"         :pair (?/ . ?/))
    (?_  :description "Underscores"             :pair (?_ . ?_)))
  "Alist of pairs for use with `prot-pair-insert'.
Each element in the list is a list whose `car' is a character and
the `cdr' is a plist with a `:description' and `:pair' keys.  The
`:description' is a string used to describe the character/pair in
interactive use, while `:pair' is a cons cell referencing the
opening and closing characters.

The value of `:pair' can also be the unquoted symbol of a
function.  The function is called with no arguments and must
return a cons cell of two characters.  Examples of such functions
are `prot-pair-insert-natural-language-quotes' and
`prot-pair-insert-backticks'"
  :type '(alist
          :key-type character
          :value-type (plist :options (((const :tag "Pair description" :description) string)
                                       ((const :tag "Characters" :pair)
                                        (choice (cons character character) function)))))
  :group 'prot-pair)

(defun prot-pair-insert-backticks ()
  "Return pair of backticks for `prot-pair-pairs'.
When the major mode is derived from `lisp-mode', return a pair of
backtick and single quote, else two backticks."
  (if (derived-mode-p 'lisp-mode 'lisp-data-mode)
      (cons ?` ?')
    (cons ?` ?`)))

(defun prot-pair-insert-natural-language-quotes ()
  "Return pair of quotes for `prot-pair-pairs', per natural language."
  ;; There are more here: <https://en.wikipedia.org/wiki/Quotation_mark>.
  ;; I cover the languages I might type in.
  (cond
   ((and current-input-method
         (string-match-p "\\(greek\\|french\\|spanish\\)" current-input-method))
    (cons ?« ?»))
   (t (cons ?\" ?\"))))

(defvar prot-pair--insert-history nil
  "Minibuffer history of `prot-pair--insert-prompt'.")

(defun prot-pair--annotate (character)
  "Annotate CHARACTER with its description in `prot-pair-pairs'."
  (when-let ((char (if (characterp character) character (string-to-char character)))
             (plist (alist-get char prot-pair-pairs))
             (description (plist-get plist :description)))
    (format "  %s" description)))

(defun prot-pair--get-pair (character)
  "Get the pair of corresponding to CHARACTER."
  (when-let ((char (if (characterp character) character (string-to-char character)))
             (plist (alist-get char prot-pair-pairs))
             (pair (plist-get plist :pair)))
    pair))

(defun prot-pair--insert-prompt ()
  "Prompt for pair among `prot-pair-pairs'."
  (let ((default (car prot-pair--insert-history))
        (candidates (mapcar (lambda (char) (char-to-string (car char))) prot-pair-pairs))
        (completion-extra-properties `(:annotation-function ,#'prot-pair--annotate)))
    (completing-read
     (format-prompt "Select pair" default)
     candidates nil :require-match
     nil 'prot-pair--insert-history default)))

(defun prot-pair--insert-bounds ()
  "Return boundaries of symbol at point or active region."
  (if (region-active-p)
      (cons (region-beginning) (region-end))
    (bounds-of-thing-at-point 'symbol)))

;;;###autoload
(defun prot-pair-insert (pair n)
  "Insert N number of PAIR around object at point.
PAIR is one among `prot-pair-pairs'.  The object at point is
either a symbol or the boundaries of the active region.  N is a
numeric prefix argument, defaulting to 1 if none is provided in
interactive use."
  (interactive
   (list
    (prot-pair--get-pair (prot-pair--insert-prompt))
    (prefix-numeric-value current-prefix-arg)))
  (let* ((bounds (prot-pair--insert-bounds))
         (beg (car bounds))
         (end (1+ (cdr bounds))) ; 1+ because we want the character after it
         (characters (if (functionp pair) (funcall pair) pair)))
    (dotimes (_ n)
      (save-excursion
        (goto-char beg)
        (insert (car characters))
        (goto-char end)
        (setq end (1+ end))
        (insert (cdr characters))))
    (goto-char (+ end (1- n)))))

;;;###autoload
(defun prot-pair-delete ()
  "Delete pair following or preceding point.
For Emacs version 28 or higher, the feedback's delay is
controlled by `delete-pair-blink-delay'."
  (interactive)
  (if (eq (point) (cdr (bounds-of-thing-at-point 'sexp)))
      (delete-pair -1)
    (delete-pair 1)))

(provide 'prot-pair)
;;; prot-pair.el ends here
