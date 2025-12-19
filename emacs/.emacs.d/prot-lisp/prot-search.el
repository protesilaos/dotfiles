;;; prot-search.el --- Extensions to isearch, replace, grep for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2020-2025  Protesilaos Stavrou

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
;; This covers my isearch.el, replace.el, and grep.el extensions, for
;; use in my Emacs setup: <https://protesilaos.com/emacs/dotemacs>.

;;; Code:

(require 'isearch)
(require 'replace)
(require 'grep)
(require 'prot-common)

(defgroup prot-search ()
  "Setup for Isearch, Occur, and related."
  :group 'search)

;; NOTE 2021-09-16: Based on my git config for headings in diffs.  Read:
;; <https://protesilaos.com/codelog/2021-01-26-git-diff-hunk-elisp-org/>.
(defcustom prot-search-outline-regexp-alist
  '((emacs-lisp-mode . "^\\((\\|;;;+ \\)")
    (org-mode . "^\\(\\*+ +\\|#\\+[Tt][Ii][Tt][Ll][Ee]:\\)"))
  "Alist of regular expressions per major mode.

For best results the key must be a symbol that corresponds to a
major mode.

To be used by `prot-search-occur-outline'."
  :type 'alist
  :group 'prot-search)

(defcustom prot-search-todo-keywords
  (concat "TODO\\|FIXME\\|NOTE\\|REVIEW\\|XXX\\|KLUDGE"
          "\\|HACK\\|WARN\\|WARNING\\|DEPRECATED\\|BUG")
  "Regexp with search to-do keywords."
  :type 'string
  :group 'prot-search)

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

;;;###autoload
(defun prot-search-isearch-repeat-forward (&optional arg)
  "Move forward, keeping point at the beginning of the match.
Optionally move to ARGth match in the given direction."
  (interactive "p")
  (when (and isearch-forward isearch-other-end)
    (goto-char isearch-other-end))
  (isearch-repeat-forward (or arg 1)))

;;;###autoload
(defun prot-search-isearch-repeat-backward (&optional arg)
  "Move backward, keeping point at the beginning of the match.
Optionally move to ARGth match in the given direction."
  (interactive "p")
  (when (and (not isearch-forward) isearch-other-end)
    (goto-char isearch-other-end))
  (isearch-repeat-backward (or arg 1)))

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

(defvar prot-search-markup-replacements
  '((elisp-to-org-code "`\\(.*?\\)'" "~\\1~")
    (elisp-to-org-verbatim "`\\(.*?\\)'" "=\\1=")
    (org-to-elisp-quote "[=~]\\(.*?\\)[=~]" "`\\1'")
    (org-to-markdown-code "[=~]\\(.*?\\)[=~]" "`\\1`"))
  "Common markup replacement patterns.")

(defvar prot-search--replace-markup-history '()
  "Minibuffer history of `prot-search-replace-markup'.")

(defun prot-search--replace-markup-prompt ()
  "Prompt for `prot-search-replace-markup'."
  (let* ((def (nth 0 prot-search--replace-markup-history))
         (prompt (if def
                     (format "Replace markup TYPE [%s]: " def)
                   "Replace markup TYPE: ")))
    (intern
     (completing-read
      prompt
      ;; TODO 2022-05-01: maybe older Emacs versions need to explicitly
      ;; map through the car of each list?
      prot-search-markup-replacements
      nil t nil 'prot-search--replace-markup-history def))))

(defun prot-search-replace-markup (type)
  "Perform TYPE of markup replacement.
TYPE is the car of a list in `prot-search-markup-replacements'.

When used interactively, prompt for completion among the
available types.

When the region is active, only perform replacements within its
boundaries, else start from point to the end of the buffer."
  (interactive (list (prot-search--replace-markup-prompt)))
  (if-let* ((types prot-search-markup-replacements)
            ((memq type (mapcar #'car types)))
            (association (alist-get type types))
            (search (nth 0 association))
            (replace (nth 1 association)))
      (if (use-region-p)
          (replace-regexp-in-region search replace (region-beginning) (region-end))
        (while (re-search-forward search nil t)
          (replace-match replace)))
    (user-error "`%s' is not part of `prot-search-markup-replacements'" type)))

;; NOTE 2023-01-14: See my `substitute' package instead of the
;; following: <https://github.com/protesilaos/substitute>.

;; (defun prot-search-isearch-replace-symbol ()
;;   "Run `query-replace-regexp' for the symbol at point."
;;   (interactive)
;;   (isearch-forward-symbol-at-point)
;;   (isearch-query-replace-regexp))

(autoload 'goto-address-mode "goto-addr")

;;;###autoload
(defun prot-search-occur-urls ()
  "Produce buttonised list of all URLs in the current buffer."
  (interactive)
  (let ((buf-name (format "*links in <%s>*" (buffer-name))))
    (add-hook 'occur-hook #'goto-address-mode)
    (occur-1 prot-common-url-regexp "\\&" (list (current-buffer)) buf-name)
    (remove-hook 'occur-hook #'goto-address-mode)))

;;;###autoload
(defun prot-search-occur-browse-url ()
  "Point browser at a URL in the buffer using completion.
Which web browser to use depends on the value of the variable
`browse-url-browser-function'.

Also see `prot-search-occur-urls'."
  (interactive)
  (let ((matches nil))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp prot-common-url-regexp nil t)
        (push (match-string-no-properties 0) matches)))
    (funcall browse-url-browser-function
             (completing-read "Browse URL: " matches nil t))))

(defvar prot-search--occur-outline-hist '()
  "Minibuffer history of `prot-search-occur-outline'.")

(defun prot-search--occur-outline-prompt ()
  "Helper prompt for `prot-search-occur-outline'."
  (let* ((alist prot-search-outline-regexp-alist)
         (key (car (assoc major-mode alist)))
         (default (or key (nth 1 prot-search--occur-outline-hist))))
    (completing-read
     (format "Outline style [%s]: " default)
     (mapcar #'car alist)
     nil nil nil 'prot-search--occur-outline-hist default)))

(defvar-local prot-search--remap-cookie nil
  "Current local value of `prot-search--remap-match-face'.")

(defface prot-search-match '((t :inherit default))
  "Face intended to override `match' buffer-locally.")

(defun prot-search--remap-match-face (buf)
  "Remap `match' to `prot-search-match' in BUF."
  (with-current-buffer buf
    (setq prot-search--remap-cookie
          (face-remap-add-relative 'match 'prot-search-match))))

;;;###autoload
(defun prot-search-occur-outline (&optional arg)
  "Produce buffer outline from `prot-search-outline-regexp-alist'.

With optional prefix ARG (\\[universal-argument]), prompt for a
preset among the entries in `prot-search-outline-regexp-alist'.

ARG may also be a string (or regular expression) when called from
Lisp."
  (interactive "P")
  (let* ((regexp (when (and arg (not (stringp arg)))
                   (prot-search--occur-outline-prompt)))
         (rx (cond
              ((stringp arg)
               arg)
              ((and arg (string= major-mode regexp))
               (alist-get regexp prot-search-outline-regexp-alist))
              ((assoc major-mode prot-search-outline-regexp-alist)
               (alist-get major-mode prot-search-outline-regexp-alist))
              (t (user-error "Unknown outline style"))))
         (buf-name (format "*outline of <%s>*" (buffer-name))))
    (occur-1 rx nil (list (current-buffer)) buf-name)
    ;; Because we are producing an outline, we do not need to know what
    ;; the exact matches are.
    (prot-search--remap-match-face buf-name)
    (add-to-history 'prot-search--occur-outline-hist regexp)))

;;;###autoload
(defun prot-search-occur-todo-keywords (&optional context)
  "Produce Occur buffer with `prot-search-todo-keywords'.
With optional numeric prefix argument for CONTEXT, show as many
lines before and after each match.

When called from Lisp CONTEXT must satisfy `natnump'.  A faulty
value is read as 0.

Also see `prot-search-grep-todo-keywords'."
  (interactive "P")
  (let* ((case-fold-search nil)
         (num (cond
               (current-prefix-arg
	            (prefix-numeric-value current-prefix-arg))
               (t (if (natnump context) context 0))))
         (buf-name (format "*keywords in <%s>*" (buffer-name))))
    (occur-1 prot-search-todo-keywords num (list (current-buffer)) buf-name)))

;;;; Outline

(defun prot-search--get-outline ()
  "Return alist of outline outline-regexp and positions."
  (let* ((outline-regexp (format "^\\(?:%s\\)" (or (bound-and-true-p outline-regexp) "[*\^L]+")))
         (heading-alist (bound-and-true-p outline-heading-alist))
         (level-fun (or (bound-and-true-p outline-level)
                        (lambda () ;; as in the default from outline.el
                          (or (cdr (assoc (match-string 0) heading-alist))
                              (- (match-end 0) (match-beginning 0))))))
         candidates)
    (save-excursion
      (goto-char (point-min))
      (while (if (bound-and-true-p outline-search-function)
                 (funcall outline-search-function)
               (re-search-forward outline-regexp nil t))
        (push
         (format "%-5s %s"
                 (line-number-at-pos (point))
                 (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
         candidates)
        (goto-char (1+ (line-end-position)))))
    (if candidates
        (nreverse candidates)
      (user-error "No outline"))))

(defun prot-search--outline-prompt ()
  "Prompt for outline among headings retrieved by `prot-search--get-outline'."
  (completing-read
   "Go to outline: "
   (prot-common-completion-table-no-sort 'imenu (prot-search--get-outline))
   nil :require-match))

(defvar prot-search-outline-hook nil
  "Normal hook to run at the end of `prot-search-outline'.")

;;;###autoload
(defun prot-search-outline ()
  "Go to the line of the given outline using completion."
  (interactive)
  (when-let* ((selection (prot-search--outline-prompt))
              (line (string-to-number (car (split-string selection "\t")))))
    (goto-char (point-min))
    (forward-line (- line 1))
    (run-hooks 'prot-search-outline-hook)))

;;;; Grep

(defvar prot-search--grep-hist nil
  "Input history of grep searches.")

(defun prot-search-grep-prompt (&optional recursive)
  "Prompt for grep pattern.
With optional RECURSIVE, indicate that the search will be called
recursively."
  (read-regexp
   (concat (if recursive
               (propertize "Recursive" 'face 'warning)
             "Local")
           " grep for PATTERN: ")
   nil 'prot-search--grep-hist))

;;;###autoload
(defun prot-search-grep (regexp &optional recursive)
  "Run grep for REGEXP.
Search in the current directory using `lgrep'.  With optional
prefix argument (\\[universal-argument]) for RECURSIVE, run a
search starting from the current directory with `rgrep'."
  (interactive
   (list
    (prot-search-grep-prompt current-prefix-arg)
    current-prefix-arg))
  (unless grep-command
    (grep-compute-defaults))
  (if recursive
      (rgrep regexp "*" default-directory)
    (lgrep regexp "*" default-directory)))

;;;###autoload
(defun prot-search-grep-todo-keywords (&optional arg)
  "Use `prot-search-grep' to find `prot-search-todo-keywords'.

With optional prefix ARG use git-grep instead for the entire
repository (runs `prot-search-git-grep-todo-keywords').  If Git
is not available on the system, run `prot-search-grep'
recursively, starting from the current directory.

Also see `prot-search-occur-todo-keywords'."
  (interactive "P")
  (cond
   (arg
    (if (executable-find "git")
        (prot-search-git-grep-todo-keywords)
      (prot-search-grep prot-search-todo-keywords t)))
   (t
    (prot-search-grep prot-search-todo-keywords))))

;; NOTE 2022-01-30: We could use `project-find-regexp' but I prefer
;; grep's editable buffers.  Besides, where is the fun in that when we
;; can use `compilation-start' instead?
;;;###autoload
(defun prot-search-git-grep-todo-keywords ()
  "Use the git-grep mechanism for `prot-search-todo-keywords'."
  (interactive)
  (let ((regexp prot-search-todo-keywords)
        (default-directory (or (vc-root-dir)
                               (locate-dominating-file "." ".git")
                               default-directory)))
    (compilation-start
     (format "git --no-pager grep -n --color=auto -r -I -E -e %s" regexp)
     'grep-mode
     (lambda (mode) (format "*prot-search-git-%s for '%s'" mode regexp))
     t)))

(defun prot-search--add-revert-function (buffer mode fn regexp)
  "Append `revert-buffer-function' for FN with REGEXP to MODE BUFFER variables.
See `prot-search-find-grep-buffer' (or related) for the kind of
BUFFER this works with."
  (with-current-buffer buffer
    (setq-local revert-buffer-function
                (lambda (_ignore-auto _noconfirm)
                  (funcall fn regexp))
                ;; FIXME 2023-04-04: The `compile-command' does not
                ;; feel right here.  We do it because in grep-mode the
                ;; g key runs `recompile' which falls back to the
                ;; `compile-command'.  We want it to do the same thing
                ;; as `revert-buffer'.
                compile-command `(funcall ',fn ,regexp))
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (when (re-search-forward (format "-*- mode: %s;" mode) (line-end-position) :no-error 1)
        (insert
         (format " revert-buffer-function: %S; compile-command %S;"
                 `(lambda (_ignore-auto _noconfirm)
                    (,fn ,regexp))
                 `(funcall ,fn ,regexp)))))))

(defun prot-search--start-compilation (args mode buffer command query)
  "Run compilation with ARGS for MODE in BUFFER given COMMAND running QUERY."
  (compilation-start
   args
   (intern (format "%s-mode" mode))
   (lambda (_mode) buffer)
   :highlight-regexp)
  (prot-search--add-revert-function buffer mode command query))

(defvar prot-search--find-grep-hist '()
  "Minibuffer history for `prot-search-find-grep-buffer' and related.")

(defmacro prot-search-make-search (command docstring prompt function mode)
  "Produce COMMAND with DOCSTRING given PROMPT, FUNCTION, and MODE."
  `(defun ,command (query)
     ,(format
       "%s.

Place the output in a buffer that runs `%s'.  Store the
invocation of this command with REGEXP in a buffer-local
variable.  When the buffer is written to a file, per
`write-file', the `revert-buffer' command (typically bound to
`g') can be used to re-run the search.  The buffer contains
information about the search results, including the exact command
line flags that were used, the time the results were produced,
and the number of matches.  All matching entries are buttonized
and function as links to the context they reference."
       docstring mode)
     (interactive
      (list
       (read-regexp ,prompt nil 'prot-search--find-grep-hist)))
     (let ((args (,function query))
           (buffer-name (format "*prot-search-find for '%s'*" query)))
       (prot-search--start-compilation args ,mode buffer-name ',command query))))

(defun prot-search--find-grep-args (regexp)
  "Return find args to produce grep results for REGEXP."
  (concat
   "find " default-directory
   " -not " (shell-quote-argument "(")
   " -path " (shell-quote-argument "*/.git*")
   " -prune " (shell-quote-argument ")")
   " -type f"
   " -exec grep -nHE --color=auto " regexp " "
   (shell-quote-argument "{}")
   " " (shell-quote-argument ";") " "))

;;;###autoload (autoload 'prot-search-find-grep-buffer "prot-search")
(prot-search-make-search
 prot-search-find-grep-buffer
 "Combine find with grep to produce a buffer for REGEXP matches"
 "Find files matching REGEXP and show a grep buffer: "
 prot-search--find-grep-args
 "grep")

(defun prot-search--find-grep-files-args (regexp)
  "Return find args to produce file listing with contents matching REGEXP."
  (concat
   "find " default-directory
   " -not " (shell-quote-argument "(")
   " -path " (shell-quote-argument "*/.git*")
   " -prune " (shell-quote-argument ")")
   " -type f"
   " -exec grep -qo --color=auto " regexp " "
   (shell-quote-argument "{}")
   " "
   (shell-quote-argument ";") " "
   "-ls"))

;;;###autoload (autoload 'prot-search-find-grep-files-buffer "prot-search")
(prot-search-make-search
 prot-search-find-grep-files-buffer
 "Combine find with grep to produce a buffer for files matching REGEXP"
 "Find files with contents matching REGEXP and show a file listing: "
 prot-search--find-grep-files-args
 "dired")

(defun prot-search--find-file-names-args (regexp)
  "Return find args to produce file listing with file names matching REGEXP."
  (concat
   "find " default-directory
   " -not " (shell-quote-argument "(")
   " -path " (shell-quote-argument "*/.git*")
   " -prune " (shell-quote-argument ")")
   " -type f"
   " -iname '*" regexp "*'"
   " -exec ls -AFhldvN --group-directories-first --time-style=long-iso --color=auto --hyperlink=never "
   (shell-quote-argument "{}")
   " "
   (shell-quote-argument ";")))

;;;###autoload (autoload 'prot-search-find-files-buffer "prot-search")
(prot-search-make-search
 prot-search-find-files-buffer
 "Use find to produce a buffer for file names matching REGEXP"
 "Find files with name matching REGEXP and show a file listing: "
 prot-search--find-file-names-args
 "dired")

;; (defun prot-search-find-grep-file (regexp)
;;   "Use find to produce list of files that include REGEXP."
;;   (interactive
;;    (list
;;     (read-regexp "Find and grep for REGEXP: " nil 'prot-search--find-grep-hist)))
;;   (let ((files (process-lines "find"
;;                               "-type" "f"
;;                               "-exec" "grep" "-nHE" "--color=auto" (format "'%s" regexp) " "
;;                               "-ls" " "
;;                               "{};")
;;                ))
;;     (find-file (completing-read "Find file: "files))))

(provide 'prot-search)
;;; prot-search.el ends here
