;;; prot-common.el --- Common functions for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2020-2022  Protesilaos Stavrou

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
;; Common functions for my Emacs: <https://protesilaos.com/emacs/dotemacs/>.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

(eval-when-compile
  (require 'subr-x))

(defgroup prot-common ()
  "Auxiliary functions for my dotemacs."
  :group 'editing)

;;;###autoload
(defun prot-common-number-even-p (n)
  "Test if N is an even number."
  (if (numberp n)
      (= (% n 2) 0)
    (error "%s is not a number" n)))

;;;###autoload
(defun prot-common-number-integer-p (n)
  "Test if N is an integer."
  (if (integerp n)
      n
    (error "%s is not an integer" n)))

;;;###autoload
(defun prot-common-number-integer-positive-p (n)
  "Test if N is a positive integer."
  (if (prot-common-number-integer-p n)
      (> n 0)
    (error "%s is not a positive integer" n)))

;; Thanks to Gabriel for providing a cleaner version of
;; `prot-common-number-negative': <https://github.com/gabriel376>.
;;;###autoload
(defun prot-common-number-negative (n)
  "Make N negative."
  (if (and (numberp n) (> n 0))
      (* -1 n)
    (error "%s is not a valid positive number" n)))

;;;###autoload
(defun prot-common-reverse-percentage (number percent change-p)
  "Determine the original value of NUMBER given PERCENT.

CHANGE-P should specify the increase or decrease.  For simplicity,
nil means decrease while non-nil stands for an increase.

NUMBER must satisfy `numberp', while PERCENT must be `natnump'."
  (unless (numberp number)
    (user-error "NUMBER must satisfy numberp"))
  (unless (natnump percent)
    (user-error "PERCENT must satisfy natnump"))
  (let* ((pc (/ (float percent) 100))
         (pc-change (if change-p (+ 1 pc) pc))
         (n (if change-p pc-change (float (- 1 pc-change)))))
    ;; FIXME 2021-12-21: If float, round to 4 decimal points.
    (/ number n)))

;;;###autoload
(defun prot-common-percentage-change (n-original n-final)
  "Find percentage change between N-ORIGINAL and N-FINAL numbers.

When the percentage is not an integer, it is rounded to 4
floating points: 16.666666666666664 => 16.667."
  (unless (numberp n-original)
    (user-error "N-ORIGINAL must satisfy numberp"))
  (unless (numberp n-final)
    (user-error "N-FINAL must satisfy numberp"))
  (let* ((difference (float (abs (- n-original n-final))))
         (n (* (/ difference n-original) 100))
         (round (floor n)))
    ;; FIXME 2021-12-21: Any way to avoid the `string-to-number'?
    (if (> n round) (string-to-number (format "%0.4f" n)) round)))

;;;###autoload
(defun prot-common-rotate-list-of-symbol (symbol)
  "Rotate list value of SYMBOL by moving its car to the end.
Return the first element before performing the rotation.

This means that if `sample-list' has an initial value of `(one
two three)', this function will first return `one' and update the
value of `sample-list' to `(two three one)'.  Subsequent calls
will continue rotating accordingly."
  (unless (symbolp symbol)
    (user-error "%s is not a symbol" symbol))
  (when-let* ((value (symbol-value symbol))
              (list (and (listp value) value))
              (first (car list)))
    (set symbol (append (cdr list) (list first)))
    first))

;;;###autoload
(defun prot-common-empty-buffer-p ()
  "Test whether the buffer is empty."
  (or (= (point-min) (point-max))
      (save-excursion
        (goto-char (point-min))
        (while (and (looking-at "^\\([a-zA-Z]+: ?\\)?$")
                    (zerop (forward-line 1))))
        (eobp))))

;;;###autoload
(defun prot-common-minor-modes-active ()
  "Return list of active minor modes for the current buffer."
  (let ((active-modes))
    (mapc (lambda (m)
            (when (and (boundp m) (symbol-value m))
              (push m active-modes)))
          minor-mode-list)
    active-modes))

;;;###autoload
(defun prot-common-truncate-lines-silently ()
  "Toggle line truncation without printing messages."
  (let ((inhibit-message t))
    (toggle-truncate-lines t)))

;;;###autoload
(defun prot-common-disable-hl-line ()
  "Disable Hl-Line-Mode (for hooks)."
  (hl-line-mode -1))

;;;###autoload
(defun prot-common-window-bounds ()
  "Determine start and end points in the window."
  (list (window-start) (window-end)))

;;;###autoload
(defun prot-common-page-p ()
  "Return non-nil if there is a `page-delimiter' in the buffer."
  (or (save-excursion (re-search-forward page-delimiter nil t))
      (save-excursion (re-search-backward page-delimiter nil t))))

;;;###autoload
(defun prot-common-read-data (file)
  "Read Elisp data from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (read (current-buffer))))

;; Thanks to Omar Antolín Camarena for providing this snippet!
;;;###autoload
(defun prot-common-completion-table (category candidates)
  "Pass appropriate metadata CATEGORY to completion CANDIDATES.

This is intended for bespoke functions that need to pass
completion metadata that can then be parsed by other
tools (e.g. `embark')."
  (lambda (string pred action)
    (if (eq action 'metadata)
        `(metadata (category . ,category))
      (complete-with-action action candidates string pred))))

;; Thanks to Igor Lima for the `prot-common-crm-exclude-selected-p':
;; <https://github.com/0x462e41>.
;; This is used as a filter predicate in the relevant prompts.
(defvar crm-separator)

;;;###autoload
(defun prot-common-crm-exclude-selected-p (input)
  "Filter out INPUT from `completing-read-multiple'.
Hide non-destructively the selected entries from the completion
table, thus avoiding the risk of inputting the same match twice.

To be used as the PREDICATE of `completing-read-multiple'."
  (if-let* ((pos (string-match-p crm-separator input))
            (rev-input (reverse input))
            (element (reverse
                      (substring rev-input 0
                                 (string-match-p crm-separator rev-input))))
            (flag t))
      (progn
        (while pos
          (if (string= (substring input 0 pos) element)
              (setq pos nil)
            (setq input (substring input (1+ pos))
                  pos (string-match-p crm-separator input)
                  flag (when pos t))))
        (not flag))
    t))

;; The `prot-common-line-regexp-p' and `prot-common--line-regexp-alist'
;; are contributed by Gabriel: <https://github.com/gabriel376>.  They
;; provide a more elegant approach to using a macro, as shown further
;; below.
(defvar prot-common--line-regexp-alist
  '((empty . "[\s\t]*$")
    (indent . "^[\s\t]+")
    (non-empty . "^.+$")
    (list . "^\\([\s\t#*+]+\\|[0-9]+[^\s]?[).]+\\)")
    (heading . "^[=-]+"))
  "Alist of regexp types used by `prot-common-line-regexp-p'.")

(defun prot-common-line-regexp-p (type &optional n)
  "Test for TYPE on line.
TYPE is the car of a cons cell in
`prot-common--line-regexp-alist'.  It matches a regular
expression.

With optional N, search in the Nth line from point."
  (save-excursion
    (goto-char (point-at-bol))
    (and (not (bobp))
         (or (beginning-of-line n) t)
         (save-match-data
           (looking-at
            (alist-get type prot-common--line-regexp-alist))))))

;; The `prot-common-shell-command-with-exit-code-and-output' function is
;; courtesy of Harold Carr, who also sent a patch that improved
;; `prot-eww-download-html' (from the `prot-eww.el' library).
;;
;; More about Harold: <http://haroldcarr.com/about/>.
(defun prot-common-shell-command-with-exit-code-and-output (command &rest args)
  "Run COMMAND with ARGS.
Return the exit code and output in a list."
  (with-temp-buffer
    (list (apply 'call-process command nil (current-buffer) nil args)
          (buffer-string))))

(defvar prot-common-url-regexp
  (concat
   "\\b\\(\\(www\\.\\|\\(s?https?\\|ftp\\|file\\|gopher\\|"
   "nntp\\|news\\|telnet\\|wais\\|mailto\\|info\\):\\)"
   "\\(//[-a-z0-9_.]+:[0-9]*\\)?"
   (let ((chars "-a-z0-9_=#$@~%&*+\\/[:word:]")
	     (punct "!?:;.,"))
     (concat
      "\\(?:"
      ;; Match paired parentheses, e.g. in Wikipedia URLs:
      ;; http://thread.gmane.org/47B4E3B2.3050402@gmail.com
      "[" chars punct "]+" "(" "[" chars punct "]+" ")"
      "\\(?:" "[" chars punct "]+" "[" chars "]" "\\)?"
      "\\|"
      "[" chars punct "]+" "[" chars "]"
      "\\)"))
   "\\)")
  "Regular expression that matches URLs.
Copy of variable `browse-url-button-regexp'.")

(autoload 'auth-source-search "auth-source")

;;;###autoload
(defun prot-common-auth-get-field (host prop)
  "Find PROP in `auth-sources' for HOST entry."
  (when-let ((source (auth-source-search :host host)))
    (if (eq prop :secret)
       (funcall (plist-get (car source) prop))
      (plist-get (flatten-list source) prop))))

(provide 'prot-common)
;;; prot-common.el ends here
