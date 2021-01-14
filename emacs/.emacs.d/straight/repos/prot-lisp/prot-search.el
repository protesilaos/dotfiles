;;; prot-search.el --- Extensions to isearch.el and replace.el for my dotemacs -*- lexical-binding: t -*-

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
;; This covers my isearch.el and replace.el extensions, for use in my
;; Emacs setup: https://protesilaos.com/dotemacs.

;;; Code:

(require 'isearch)
(require 'replace)

;;;; Isearch

;;;###autoload
(defun prot-search-isearch-other-end ()
  "End current search in the opposite side of the match.
Particularly useful when the match does not fall within the
confines of word boundaries (e.g. multiple words)."
  (interactive)
  (isearch-done)
  (when isearch-other-end
    (goto-char isearch-other-end)))

;;;###autoload
(defun prot-search-isearch-abort-dwim ()
  "Delete failed `isearch' input, single char, or cancel search.

This is a modified variant of `isearch-abort' that allows us to
perform the following, based on the specifics of the case: (i)
delete the entirety of a non-matching part, when present; (ii)
delete a single character, when possible; (iii) exit current
search if no character is present and go back to point where the
search started."
  (interactive)
  (if (eq (length isearch-string) 0)
      (isearch-cancel)
    (isearch-del-char)
    (while (or (not isearch-success) isearch-error)
      (isearch-pop-state)))
  (isearch-update))

(defmacro prot-search-isearch-occurrence (name edge &optional doc)
  "Construct function for moving to `isearch' occurrence.
NAME is the name of the function.  EDGE is either the beginning
or the end of the buffer.  Optional DOC is the resulting
function's docstring."
  `(defun ,name (&optional arg)
     ,doc
     (interactive "p")
     (let ((x (or arg 1))
           (command (intern (format "isearch-%s-of-buffer" ,edge))))
       (isearch-forward-symbol-at-point)
       (funcall command x))))

(prot-search-isearch-occurrence
 prot-search-isearch-beginning-of-buffer
 "beginning"
 "Run `isearch-beginning-of-buffer' for the symbol at point.
With numeric ARG, move to ARGth occurrence counting from the
beginning of the buffer.")

(prot-search-isearch-occurrence
 prot-search-isearch-end-of-buffer
 "end"
 "Run `isearch-end-of-buffer' for the symbol at point.
With numeric ARG, move to ARGth occurrence counting from the
end of the buffer.")

;;;; Replace/Occur

;; TODO: make this work backwardly when given a negative argument
(defun prot-search-isearch-replace-symbol ()
  "Run `query-replace-regexp' for the symbol at point."
  (interactive)
  (isearch-forward-symbol-at-point)
  (isearch-query-replace-regexp))

(defvar prot-search-url-regexp
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

(declare-function goto-address-mode "goto-addr")

;;;###autoload
(defun prot-search-occur-urls ()
  "Produce buttonised list of all URLs in the current buffer."
  (interactive)
  (add-hook 'occur-hook #'goto-address-mode)
  (occur prot-search-url-regexp "\\&")
  (remove-hook 'occur-hook #'goto-address-mode))

;;;###autoload
(defun prot-search-occur-browse-url ()
  "Point browser at a URL in the buffer using completion.
Which web browser to use depends on the value of the variable
`browse-url-browser-function'.

Also see `prot-search-occur-url'."
  (interactive)
  (let ((matches nil))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp prot-search-url-regexp nil t)
        (push (match-string-no-properties 0) matches)))
    (funcall browse-url-browser-function
             (completing-read "Browse URL: " matches nil t))))

(provide 'prot-search)
;;; prot-search.el ends here
