;;; usls.el --- Unassuming Sidenotes of Little Significance -*- lexical-binding: t -*-

;; Copyright (C) 2020-2021  Protesilaos Stavrou

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://gitlab.com/protesilaos/usls
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))

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
;; Consult the project's README.

;;; Code:

(require 'cl-lib)
(require 'ffap)
(require 'thingatpt)

;;; User-facing options

(defgroup usls ()
  "Simple tool for plain text notes."
  :group 'files
  :prefix "usls-")

(defcustom usls-directory (expand-file-name "~/Documents/notes/")
  "Directory for storing personal notes."
  :group 'usls
  :type 'directory)

(defcustom usls-known-categories '("economics" "philosophy" "politics")
  "List of strings with predefined categories for `usls-new-note'.

The implicit assumption is that a category is a single word.  If
you need a category to be multiple words long, use underscores to
separate them.  Do not use hyphens, as those are assumed to
demarcate distinct categories, per `usls--inferred-categories'.

Also see `usls-categories' for a dynamically generated list that
gets combined with this one in relevant prompts."
  :group 'usls
  :type '(repeat string))

(defcustom usls-subdir-support nil
  "Enable support for subdirectories in `usls-directory'.

The default workflow of USLS is to maintain a flat directory
where all the notes are stored in.  This allows us to omit the
common filesystem path and only show file names.

When set to non-nil, the usls workflow can handle subdirectories
at the expense of making all file names more verbose, as it needs
to include the complete path.

NOTE: such subdirectories must be created manually to make sure
that no destructive filesystem operations are performed by
accident."
  :group 'usls
  :type 'boolean)

(defcustom usls-file-type-extension ".txt"
  "File type extension for new USLS notes.

Available options cover plain text (.txt), Markdown (.md), and
Org (.org) formats."
  :group 'usls
  :type '(choice
          (const :tag "Plain text format" ".txt")
          (const :tag "Markdown format" ".md")
          (const :tag "Org format" ".org")))

(defcustom usls-file-region-separator 'line
  "Separator for `usls-new-note' delimiting the captured region.

The default value of 'line' produces a horizontal rule depending
on the `usls-file-type-extension'.

* For plain text and Markdown this results in the following
  string (without the quotes): '\\n\\n* * *\\n\\n'.  It means to put
  two new lines before and two after the three space-separated
  asterisks.  In practice, that means an empty line before and
  after.  This notation is a common way to denote a horizontal
  rule or page/section break and is a standard in Markdown.

* For Org files it produces five consecutive hyphens with
  newlines before and after ('\\n\\n-----\\n\\n').  This is the
  valid syntax for a horizontal rule in Org mode.

Option 'heading' produces a heading that is formatted according
to `usls-file-type-extension'.  Its text is 'Reference':

* For plain text, the formatting of the heading involves a series
  of hyphens below the heading's text, followed by an empty line.
  The length of the hyphens is equal to that of the heading's
  text.

* For Markdown and Org the heading is formatted per the
  respective major mode's syntax, plus an empty line before and
  after.

It is also possible to provide a string of your own.  This should
contain just the text that you wish to turn into a heading.  For
example, you want to use the word 'Captured region' instead of
'Reference', so provide only that.  Your input will be processed
according to `usls-file-type-extension' to offer the correct
heading format.  The result will mimic that of the aforementioned
options.

The level of the heading is controlled by the customisation
option `usls-file-region-separator-heading-level' and defaults to
1 (one # for Markdown or one * for Org)."
  :group 'usls
  :type '(choice
          (const :tag "Line with surrounding space (default)" line)
          (const :tag "A 'Reference' heading" heading)
          (string :tag "A heading with text of your choice")))

(defcustom usls-file-region-separator-heading-level 1
  "Heading level for `usls-file-region-separator'.
Has effect when `usls-file-type-extension' is either that for
Markdown or Org types."
  :group 'usls
  :type 'integer)

(defcustom usls-custom-header-function nil
  "Function to format headers for new files (EXPERIMENTAL!!!).

It should accept five arguments and catenate them as a string,
preferably with the appropriate new lines in place.  The
arguments are: title, date, categories, filename, id.  Those are
supplied by `usls-new-note'.

While all five arguments will be passed to this function, not all
of them need to be part of the output.  Users may prefer, for
example, to only include a title, a date, and a category.

For ideas on how to format such a function, refer to the source
code of `usls--file-meta-header'.

Although this customisation can be set globally, another viable
use-case is to `let' bind it in wrapper functions around
`usls-new-note'.  In that scenario, it could be desirable to also
set the value of `usls-file-type-extension', so as to generate a
different type of note than the default: such as to write
something in '.tex' while the default extension remains in tact.
In this case, users are expected to define a wrapper for
`usls-new-note' like this (without the backslashes that appear in
the source of this docstring):

  (defun my-usls-new-note-for-tex ()
    (let ((usls-file-type-extension \".tex\")
          (usls-custom-header-function #'my-usls-custom-header))
      (usls-new-note)))"
  :group 'usls
  :type '(choice (const nil) function))

;;; Main variables

(defconst usls-id "%Y%m%d_%H%M%S"
  "Format of ID prefix of a note's filename.")

(defconst usls-id-regexp "\\([0-9_]+\\{15\\}\\)"
  "Regular expression to match `usls-id'.")

(defconst usls-category-regexp "--\\([0-9A-Za-z_-]*\\)--"
  "Regular expression to match `usls-categories'.")

(defconst usls-file-regexp
  (concat usls-id-regexp usls-category-regexp "\\(.*\\)\\.\\(txt\\|md\\|org\\)")
  "Regular expression to match file names from `usls-new-note'.")

(defvar usls--file-link-regexp "^\\(@@\\|\\^^\\) \\(.*\\.\\)\\(txt\\|md\\|org\\)"
  "Regexp for file links.")

;;;; Input history lists

(defvar usls--title-history '()
  "Used internally by `usls-new-note' to record titles.")

(defvar usls--category-history '()
  "Used internally by `usls-new-note' to record categories.")

(defvar usls--file-history '()
  "Used internally by `usls-find-file' to record file names.")

(defvar usls--link-history '()
  "Used internally by `usls-id-insert' to record links.")

(defvar usls--subdirectory-history '()
  "Used internally by `usls-new-note' to record subdirectories.")

;;; Basic utilities

;; Contributed by Omar Antolín Camarena in another context:
;; <https://github.com/oantolin>.
(defun usls--completion-table (category candidates)
  "Pass appropriate metadata CATEGORY to completion CANDIDATES."
  (lambda (string pred action)
    (if (eq action 'metadata)
        `(metadata (category . ,category))
      (complete-with-action action candidates string pred))))

(defvar crm-separator)

;; Contributed by Igor Lima in another context :
;; <https://github.com/0x462e41>.
(defun usls-crm-exclude-selected-p (input)
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

(defvar usls-mode)

(defun usls--barf-non-text-usls-mode ()
  "Throw error if not in a proper USLS buffer."
  (unless (and usls-mode (derived-mode-p 'text-mode))
    (user-error "Not in a writable USLS buffer; aborting")))

;;;; File name helpers

(defun usls--directory ()
  "Valid name format for `usls-directory'."
  (file-name-as-directory usls-directory))

(defun usls--extract (regexp str)
  "Extract REGEXP from STR."
  (with-temp-buffer
    (insert str)
    (when (re-search-forward regexp nil t -1)
      (match-string 1))))

(defvar usls--punctuation-regexp "[][{}!@#$%^&*()_=+'\"?,.\|;:~`‘’“”]*"
  "Regular expression of punctionation that should be removed.")

(defun usls--slug-no-punct (str)
  "Convert STR to a file name slug."
  (replace-regexp-in-string usls--punctuation-regexp "" str))

(defun usls--slug-hyphenate (str)
  "Replace spaces with hyphens in STR.
Also replace multiple hyphens with a single one and remove any
trailing hyphen."
  (replace-regexp-in-string
   "-$" ""
   (replace-regexp-in-string
    "-\\{2,\\}" "-"
    (replace-regexp-in-string "--+\\|\s+" "-" str))))

(defun usls--sluggify (str)
  "Make STR an appropriate file name slug."
  (downcase (usls--slug-hyphenate (usls--slug-no-punct str))))

;;;; Files in directory

(defun usls--directory-files-flat ()
  "List `usls-directory' files, assuming flat directory."
  (let ((dotless directory-files-no-dot-files-regexp))
    (cl-remove-if
     (lambda (x)
       ;; TODO: generalise this for all VC backends?  Which ones?
       (or (string-match-p "\\.git" x)
           (file-directory-p x)))
     (directory-files (usls--directory) nil dotless t))))

(defun usls--directory-files-recursive ()
  "List `usls-directory' files, assuming directory tree."
    (cl-remove-if
     (lambda (x)
       ;; TODO: generalise this for all VC backends?  Which ones?
       (string-match-p "\\.git" x))
     (directory-files-recursively (usls--directory) ".*" nil t)))

(defun usls--directory-files ()
  "List directory files."
  (let ((path (usls--directory)))
    (unless (file-directory-p path)
      (make-directory path t))
    (if usls-subdir-support
        (usls--directory-files-recursive)
      (usls--directory-files-flat))))

(defun usls--directory-subdirs ()
  "Return list of subdirectories in `usls-directory'."
  (cl-remove-if-not
   (lambda (x)
     (file-directory-p x))
   (directory-files-recursively (usls--directory) ".*" t t)))

;; TODO: generalise this for all VC backends?  Which ones?
(defun usls--directory-subdirs-no-git ()
  "Remove .git directories from `usls--directory-subdirs'."
  (cl-remove-if
   (lambda (x)
     (string-match-p "\\.git" x))
   (usls--directory-subdirs)))

(defun usls--directory-subdirs-completion-table (dirs)
  "Match DIRS as a completion table."
  (let ((def (car usls--subdirectory-history))
        (table (usls--completion-table 'file dirs)))
    (completing-read
     (format "Subdirectory of new note [%s]: " def)
     table nil t nil 'usls--subdirectory-history def)))

(defun usls--directory-subdirs-prompt ()
  "Handle user input on choice of subdirectory."
  (let* ((subdirs
          (if (eq (usls--directory-subdirs-no-git) nil)
              (user-error "No subdirs in `%s'; create them manually"
                          (usls--directory))
            (usls--directory-subdirs-no-git)))
         (choice (usls--directory-subdirs-completion-table subdirs))
         (subdir (file-truename choice)))
    (add-to-history 'usls--subdirectory-history choice)
    subdir))

;;;; Categories

(defun usls--categories-in-files ()
  "Produce list of categories in `usls--directory-files'."
  (cl-remove-if nil
   (mapcar (lambda (x)
             (usls--extract usls-category-regexp x))
           (usls--directory-files))))

(defun usls--inferred-categories ()
  "Extract categories from `usls--directory-files'."
  (let ((sequence (usls--categories-in-files)))
    (mapcan (lambda (s)
              (split-string s "-" t))
            sequence)))

(defun usls-categories ()
  "Combine `usls--inferred-categories' with `usls-known-categories'."
  (delete-dups (append (usls--inferred-categories) usls-known-categories)))

(defun usls--categories-prompt ()
  "Prompt for one or more categories.
In the case of multiple entries, those are separated by the
`crm-sepator', which typically is a comma.  In such a case, the
output is sorted with `string-lessp'."
  (let* ((categories (usls-categories))
         (choice (completing-read-multiple
                  "File category: " categories
                  #'usls-crm-exclude-selected-p
                  nil nil 'usls--category-history)))
    (if (= (length choice) 1)
        (car choice)
      (sort choice #'string-lessp))))

(defun usls--categories-hyphenate (categories)
  "Format CATEGORIES output of `usls--categories-prompt'."
  (if (and (> (length categories) 1)
           (not (stringp categories)))
      (mapconcat #'downcase categories "-")
    categories))

(defun usls--categories-capitalize (categories)
  "`capitalize' CATEGORIES output of `usls--categories-prompt'."
  (if (and (> (length categories) 1)
           (not (stringp categories)))
      (mapconcat #'capitalize categories ", ")
    (capitalize categories)))

(defun usls--categories-add-to-history (categories)
  "Append CATEGORIES to `usls--category-history'."
  (if (and (listp categories)
           (> (length categories) 1))
      (let ((cats (delete-dups
                   (mapc (lambda (cat)
                           (split-string cat "," t))
                         categories))))
        (mapc (lambda (cat)
                (add-to-history 'usls--category-history cat))
              cats)
        (setq usls--category-history
              (cl-remove-if (lambda (x)
                              (string-match-p crm-separator x))
                            usls--category-history)))
    (add-to-history 'usls--category-history categories)))

;;; Templates

(defun usls--file-meta-header (title date categories filename id)
  "Front matter template based on `usls-file-type-extension'.

This helper function is meant to integrate with `usls-new-note'.
As such TITLE, DATE, CATEGORIES, FILENAME, ID are all retrieved
from there."
  (let ((cat (usls--categories-capitalize categories)))
    (pcase usls-file-type-extension
      ;; TODO: make those templates somewhat customisable.  We need to
      ;; determine what should be parametrised.
      (".md" `(concat "---" "\n"
                      "title:     " ,title "\n"
                      "date:      " ,date "\n"
                      "category:  " ,cat "\n"
                      "orig_name: " ,filename "\n"
                      "orig_id:   " ,id "\n"
                      "---" "\n\n"))
      (".org" `(concat "#+title:     " ,title "\n"
                       "#+date:      " ,date "\n"
                       "#+category:  " ,cat "\n"
                       "#+orig_name: " ,filename "\n"
                       "#+orig_id:   " ,id "\n\n"))
      (_ `(concat "title:     " ,title "\n"
                  "date:      " ,date "\n"
                  "category:  " ,cat "\n"
                  "orig_name: " ,filename "\n"
                  "orig_id:   " ,id "\n"
                  (make-string 26 ?-) "\n\n")))))

(defun usls--file-region-separator-heading-level (mark str)
  "Format MARK and STR for `usls--file-region-separator-str'.
MARK must be a single character string.  For multiple character
strings only the first one is used."
  (let ((num usls-file-region-separator-heading-level)
        (char (when (stringp mark)
                (string-to-char (substring mark 0 1)))))
    (format "\n\n%s %s\n\n" (make-string num char) str)))

(defun usls--file-region-separator-str ()
  "Produce region delimiter string for use in `usls-new-note'."
  (let* ((str (format "%s" usls-file-region-separator))
         (num (length str)))
    (pcase usls-file-region-separator
      ('line (pcase usls-file-type-extension
               (".org" (format "\n\n%s\n\n" (make-string 5 ?-)))
               (_ "\n\n* * *\n\n")))
      ('heading (pcase usls-file-type-extension
                  (".md" (usls--file-region-separator-heading-level "#" "Reference"))
                  (".org" (usls--file-region-separator-heading-level "*" "Reference"))
                  (_ (format "\n\nReference\n%s\n\n" (make-string 9 ?-)))))
      (_ (pcase usls-file-type-extension
           (".md" (usls--file-region-separator-heading-level "#" str))
           (".org" (usls--file-region-separator-heading-level "*" str))
           (_ (format "\n\n%s\n%s\n\n" str (make-string num ?-))))))))

;; This just silences the compiler for the subsequent function
(defvar eww-data)

;; TODO: get some link for gnus, mu4e?  What else?
(defun usls--file-region-source ()
  "Capture path to file or URL for `usls--file-region'."
  (cond
   ((derived-mode-p 'eww-mode)
    (if (plist-get eww-data :title)
        (format "%s <%s>\n\n" (plist-get eww-data :title) (plist-get eww-data :url))
      (concat (plist-get eww-data :url) "\n\n")))
   ((buffer-file-name)
    (concat (buffer-file-name) "\n\n"))
   (t
    "")))

(defun usls--file-region-separator (region)
  "`usls--file-region-separator-str' and `usls-new-note' REGION."
  `(concat
    (usls--file-region-separator-str)
    (usls--file-region-source)
    ,region))

(defun usls--file-region ()
  "Capture active region for use in `usls-new-note'."
  (if (use-region-p)
      (eval (usls--file-region-separator
             (buffer-substring-no-properties
              (region-beginning)
              (region-end))))
    ""))

(defun usls--file-region-append ()
  "Capture active region for use in `usls-append-region-buffer-or-file'."
  (if (use-region-p)
      (eval (buffer-substring-no-properties
             (region-beginning)
             (region-end)))
    ""))

;;; Commands and their helper functions

;;;; New note

(defun usls--format-file (path id categories slug extension)
  "Helper for `usls-new-note' to format file names.
PATH, ID, CATEGORIES, SLUG, AND EXTENSION are expected to be
supplied by `usls-new-note': they will all be converted into a
single string."
  (format "%s%s--%s--%s%s"
          path
          id
          categories
          slug
          extension))

;;;###autoload
(defun usls-new-note (&optional arg)
  "Create new note with the appropriate metadata and file name.
If the region is active, append it to the newly created file.

This command first prompts for a file title and then for a
category.  The latter supports completion.

To input multiple categories, separate them with a comma or
whatever the value of `crm-separator' is on your end.  While
inputting multiple categories, those already selected are removed
from the list of completion candidates, meaning that it is not
possible to select the same item twice.

With prefix key (\\[universal-argument]) as optional ARG also
prompt for a subdirectory of `usls-directory' to place the new
note in.  Subdirectories must already exist."
  (interactive "P")
  (let* ((subdir (when arg (usls--directory-subdirs-prompt)))
         (title (read-string "File title: " nil 'usls--title-history))
         (categories (usls--categories-prompt))
         (slug (usls--sluggify title))
         (path (file-name-as-directory (or subdir usls-directory)))
         (id (format-time-string usls-id))
         (filename (usls--format-file path id
                    (usls--categories-hyphenate categories)
                    slug usls-file-type-extension))
         (date (format-time-string "%F"))
         (region (usls--file-region)))
    (with-current-buffer (find-file filename)
      (insert (eval (if usls-custom-header-function
                        (funcall usls-custom-header-function title date
                                 categories filename id)
                      (usls--file-meta-header title date categories filename id))))
      (save-excursion (insert region)))
    (add-to-history 'usls--title-history title)
    (usls--categories-add-to-history categories)))

(defun usls--directory-files-not-current ()
  "Return list of files minus the current one."
  (cl-remove-if
   (lambda (x)
     (if usls-subdir-support
         (string= (abbreviate-file-name (buffer-file-name)) x)
       (string= (file-name-nondirectory (buffer-file-name)) x)))
   (usls--directory-files)))

;;;; Insert reference

(defun usls--insert-file-reference (file delimiter)
  "Insert formatted reference to FILE with DELIMITER."
  (save-excursion
    (goto-char (point-max))
    (newline 1)
    (insert
     (format "%s %s\n" delimiter file))))

(defun usls--delete-duplicate-links ()
  "Remove duplicate references to files."
  (delete-duplicate-lines
   (save-excursion
     (goto-char (point-min))
     (search-forward-regexp "\\(@@\\|\\^\\^\\) " nil t nil))
   (point-max)))

;;;###autoload
(defun usls-id-insert ()
  "Insert at point the identity of a file using completion."
  (interactive)
  (usls--barf-non-text-usls-mode)
  (let* ((files (usls--completion-table 'file (usls--directory-files-not-current)))
         (file (completing-read "Link to: " files nil t nil 'usls--link-history))
         (this-file (file-name-nondirectory (buffer-file-name)))
         (id (usls--extract usls-id-regexp file)))
    (insert (concat "^" id))
    (usls--insert-file-reference (format "%s" file) "^^")
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (usls--insert-file-reference this-file "@@")
        (usls--delete-duplicate-links))
      (save-buffer)
      (kill-buffer))
    (usls--delete-duplicate-links)
    (add-to-history 'usls--link-history file)))


;;;; Follow links

(defun usls--links ()
  "Gather links to files in the current buffer."
  (let ((links))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp usls--file-link-regexp nil t)
        (push
         (concat (match-string-no-properties 2)
                 (match-string-no-properties 3))
         links)))
    (cl-remove-duplicates links)))

;;;###autoload
(defun usls-follow-link ()
  "Visit link referenced in the note using completion.
If no links are available, call `usls-find-file'."
  (interactive)
  (usls--barf-non-text-usls-mode)
  (let* ((default-directory (usls--directory))
         (links (usls--links))
         (refs (when links (usls--completion-table 'file links))))
    (if refs
        (find-file
         (completing-read "Follow link: " refs nil t))
      (call-interactively 'usls-find-file))))

;;;; Find file

(defun usls--file-name (file)
  "Return properly formatted name of FILE."
  (if usls-subdir-support
     (file-truename file)
    (file-truename (concat (usls--directory) file))))

(defun usls--find-file-prompt ()
  "Completion prompt for `usls-find-file'."
  (let ((files (usls--completion-table 'file (usls--directory-files))))
    (completing-read "Visit file: " files nil t nil 'usls--file-history)))

;;;###autoload
(defun usls-find-file (file)
  "Visit a FILE in `usls-directory'.
When called interactively use completion."
  (interactive (list (usls--find-file-prompt)))
  (let* ((default-directory (usls--directory))
         (item (usls--file-name file)))
    (find-file item)
    (add-to-history 'usls--file-history file)))

;;;; Append to file

;; REVIEW: Maybe all those filtered lists can be simplified into maybe
;; one or two.  This feels needlessly complex.

(defun usls--window-buffer-list ()
  "Return list of windows."
  (mapcar (lambda (x)
            (window-buffer x))
          (window-list)))

(defun usls--window-buffer-file-names-list ()
  "Return file names in `usls--window-buffer-list'."
  (cl-remove-if nil
   (mapcar (lambda (x)
             (buffer-file-name x))
           (usls--window-buffer-list))))

(defun usls--window-usls-file-buffers ()
  "Return USLS files in `usls--window-buffer-file-names-list'."
  (let ((files (usls--directory-files-recursive))
        (buf-files (mapcar #'abbreviate-file-name (usls--window-buffer-file-names-list))))
    (cl-remove-if nil
     (mapcar (lambda (x)
               (when (member x files)
                 x))
             buf-files))))

(defun usls--window-usls-buffers ()
  "Return buffer names from `usls--window-usls-file-buffers'."
  (mapcar (lambda (x)
            (get-file-buffer x))
          (usls--window-usls-file-buffers)))

(defun usls--window-buffers-live ()
  "Return live windows matching `usls--window-usls-buffers'."
  (cl-remove-if-not (lambda (x)
                      (window-live-p x))
                    (mapcar (lambda (y)
                              (get-buffer-window y))
                            (usls--window-usls-buffers))))

(defun usls--window-buffers ()
  "Return buffer names in `usls--window-buffers-live'."
  (mapcar (lambda (x)
            (window-buffer x))
          (usls--window-buffers-live)))

(defun usls--window-single-buffer-or-prompt ()
  "Return buffer name if one, else prompt with completion."
  (let* ((buffers
          (delete-dups
           (mapcar (lambda (x)
                     (format "%s" x))
                   (usls--window-buffers))))
         (bufs (usls--completion-table 'buffer buffers))
         (buf (if (> (length buffers) 1)
                  (completing-read "Pick buffer: "
                                   bufs nil t)
                (if (listp buffers) (car buffers) buffers))))
    (unless (eq buf nil)
      (get-buffer-window buf))))

(defun usls--window-buffer-or-file ()
  "Return window with a USLS buffer or prompt for a file."
  (let ((files (usls--directory-files)))
    (or (usls--window-single-buffer-or-prompt)
        (completing-read "Visit file: " files nil t nil 'usls--file-history))))

(defun usls--append-region (buf region arg)
  "Routines to append active region.
All of BUF, REGION, ARG are intended to be passed by another
function, such as with `usls-append-region-buffer-or-file'."
  (let ((window (get-buffer-window buf))
        (mark (gensym)))
    (with-current-buffer buf
      (goto-char (if (not (eq arg nil)) (point-max) (window-point window)))
      (setq mark (point))
      (insert region)
      (goto-char mark))))

;;;###autoload
(defun usls-append-region-buffer-or-file (&optional arg)
  "Append active region to buffer or file.

To 'append' is to insert at point.  To insert at the end of text
instead, pass a \\[universal-argument] prefix argument ARG.

If there exist one or more windows whose buffers visit a file
found in `usls-directory', then they are used as targets for
appending the active region.  When multiple windows are
available, a minibuffer prompt with completion is provided to
select one among them.

When no such windows are live, the minibuffer prompt asks for a
file to visit.

The appended region is not preceded by a delimiter, as is the
case with `usls-new-note'."
  (interactive "P")
  (let* ((object (usls--window-buffer-or-file))
         (buf (when (windowp object) (window-buffer object)))
         (region (usls--file-region-append))
         (append (if arg t nil)))
    (if (bufferp buf)
        (usls--append-region buf region append)
      (usls--append-region (find-file (usls--file-name object)) region append)
      ;; Only add to history when we are dealing with a file
      (add-to-history 'usls--file-history object))))

;;;; Dired

;;;###autoload
(defun usls-dired (&optional arg)
  "Switch to `usls-directory' using `dired'.
With optional \\[universal-argument] prefix ARG prompt for a usls
subdirectory to switch to.  If none is available, the main
directory will be directly displayed instead."
  (interactive "P")
  (let ((path usls-directory)
        (subdirs (usls--directory-subdirs-no-git)))
    (unless (file-directory-p path)
      (user-error "`usls-directory' not found at %s" usls-directory))
    (if (and arg subdirs)
        (dired (usls--directory-subdirs-prompt))
      (dired path))))

;;; User-facing setup

(defvar usls-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Key map for use when USLS mode is active.")

(define-minor-mode usls-mode
  "Extras for working with `usls' notes.

\\{usls-mode-map}"
  :init-value nil
  :global nil
  :lighter " usls"
  :keymap usls-mode-map)

(defun usls-mode-activate ()
  "Activate mode when inside `usls-directory'."
  (when (or (string-match-p (expand-file-name usls-directory) default-directory)
            (string-match-p (abbreviate-file-name usls-directory) default-directory)
            (string-match-p usls-directory default-directory))
      (usls-mode 1)))

(add-hook 'find-file-hook #'usls-mode-activate)
(add-hook 'dired-mode-hook #'usls-mode-activate)

(defgroup usls-faces ()
  "Faces for USLS mode."
  :group 'faces)

(defface usls-header-data-date
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#2544bb")
    (((class color) (min-colors 88) (background dark))
     :foreground "#79a8ff")
    (t :inherit font-lock-string-face))
  "Face for header date entry.")

(defface usls-header-data-category
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#5317ac")
    (((class color) (min-colors 88) (background dark))
     :foreground "#b6a0ff")
    (t :inherit font-lock-keyword-face))
  "Face for header category entry.")

(defface usls-header-data-title
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#8f0075")
    (((class color) (min-colors 88) (background dark))
     :foreground "#f78fe7")
    (t :inherit font-lock-builtin-face))
  "Face for header title entry.")

(defface usls-header-data-secondary
  '((((class color) (min-colors 88) (background light))
     :foreground "#61284f")
    (((class color) (min-colors 88) (background dark))
     :foreground "#fbd6f4")
    (t :inherit (bold shadow)))
  "Face for secondary header information.")

(defface usls-header-data-key
  '((((class color) (min-colors 88) (background light))
     :foreground "#00538b")
    (((class color) (min-colors 88) (background dark))
     :foreground "#00d3d0")
    (t :inherit font-lock-variable-name-face))
  "Face for secondary header information.")

(defface usls-section-delimiter
  '((((class color) (min-colors 88) (background light))
     :background "#d7d7d7" :foreground "#404148")
    (((class color) (min-colors 88) (background dark))
     :background "#323232" :foreground "#bfc0c4")
    (t :inherit shadow))
  "Face for section delimiters.")

(defface usls-dired-field-date
  '((((class color) (min-colors 88) (background light))
     :foreground "#005077")
    (((class color) (min-colors 88) (background dark))
     :foreground "#90c4ed")
    (t :inherit font-lock-variable-name-face))
  "Face for file name date in `dired-mode' buffers.")

(defface usls-dired-field-delimiter
  '((t :inherit shadow))
  "Face for file name field delimiters in `dired-mode' buffers.")

(defface usls-dired-field-category
  '((((class color) (min-colors 88) (background light))
     :foreground "#5317ac")
    (((class color) (min-colors 88) (background dark))
     :foreground "#b6a0ff")
    (t :inherit font-lock-keyword-face))
  "Face for file name category in `dired-mode' buffers.")

(defface usls-dired-field-name
  '((((class color) (min-colors 88) (background light))
     :foreground "#000000")
    (((class color) (min-colors 88) (background dark))
     :foreground "#ffffff")
    (t :inherit default))
  "Face for file name title in `dired-mode' buffers.")

;; TODO: re-use regular expressions as is already done for
;; `usls-file-regexp'.
(defconst usls-font-lock-keywords
  `((,usls-file-regexp
     (0 'usls-dired-field-delimiter)
     (1 'usls-dired-field-date t)
     (2 'usls-dired-field-category t)
     (3 'usls-dired-field-name t)
     (4 'usls-dired-field-delimiter t))
    ("\\(title:\\) \\(.*\\)"
     (1 'usls-header-data-key)
     (2 'usls-header-data-title))
    ("\\(date:\\) \\(.*\\)"
     (1 'usls-header-data-key)
     (2 'usls-header-data-date))
    ("\\(category:\\) \\(.*\\)"
     (1 'usls-header-data-key)
     (2 'usls-header-data-category))
    ("\\(orig_\\(name\\|id\\):\\) \\(.*\\)"
     (1 'usls-header-data-key)
     (2 'usls-header-data-key)
     (3 'usls-header-data-secondary t))
    ("^\\(-\\{26\\}\\|[*\s]\\{5\\}\\)$"
     (1 'usls-section-delimiter))
    ("\\(\\^\\)\\([0-9_]\\{15\\}\\)"
     (1 'escape-glyph)
     (2 'font-lock-variable-name-face))
    (,usls--file-link-regexp
     (1 'escape-glyph)
     (2 'font-lock-constant-face t)
     (3 'font-lock-constant-face t)))
  "Rules to apply font-lock highlighting with `usls--fontify'.")

(defun usls--fontify ()
  "Font-lock setup for `usls-font-lock-keywords'."
  (font-lock-flush (point-min) (point-max))
  (if usls-mode
      (font-lock-add-keywords nil usls-font-lock-keywords t)
    (font-lock-remove-keywords nil usls-font-lock-keywords))
  (font-lock-flush (point-min) (point-max)))

(add-hook 'usls-mode-hook #'usls--fontify)

(provide 'usls)

;;; usls.el ends here
