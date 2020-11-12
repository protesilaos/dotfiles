;;; usls.el --- Unassuming Sidenotes of Little Significance -*- lexical-binding: t -*-

;; Copyright (C) 2020  Protesilaos Stavrou

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos.com/dotemacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
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
;; usls.el -- Unassuming Sidenotes of Little Significance
;; ------------------------------------------------------
;;
;; WARNING: This software is pre-alpha quality.  There will be bugs,
;; errors, cases where improvements could be made.  Please do not try it
;; with sensitive data that you have not safely backed up.  If you do use
;; it, I encourage you to send me feedback about anything you feel could be
;; improved or otherwise made different.
;;
;;
;; USLS or usls, which may be pronounced as a series of letters or just
;; "useless", is a set of utilities for fleshing out a note-taking workflow
;; that revolves around a strict file naming convention and relies
;; exclusively on core Emacs tools.
;;
;; usls.el is meant to be a simple tool for streamlining the process of
;; creating notes.  It does not provide utilities that already exist in the
;; Emacs milieu or standard Unix tools, such as dired and grep
;; respectively.  The focus is on the main points of interaction: (i)
;; creating notes, (ii) adding forward/backward references to other notes,
;; (iii) browsing such references for the current file, (iv) visiting the
;; 'usls-directory', (v) finding a file that belongs to said directory.
;;
;;
;; The file name convention
;; ------------------------
;;
;; All files created with usls have a file name that follows this pattern:
;;
;;     DATE--CATEGORY--TITLE.EXTENSION
;;
;; All fields are separated by two hyphens.
;;
;; The DATE field represents the date in year-month-day followed by an
;; underscore and the current time in hour-minute-second notation.  The
;; presentation is compact, with only the underscore separating the two
;; components.  Like this: 20201108_091625.  The DATE serves as the unique
;; identifier of each note.
;;
;; CATEGORY is one or more entries separated by a hyphen.  Items that need
;; to be more than one word long must be written with an underscore.  So
;; "emacs_library" is one category, while "emacs-library" are two.
;;
;; The TITLE is the title of the note that gets extracted and hyphenated.
;; An entry about "This is a test" produces a "this-is-a-test" TITLE.
;;
;; Some complete examples:
;;
;; 20201007_124941--economics--plentiful-and-predictable-liquidity.txt
;; 20201007_104945--emacs-git--git-patch-formatting.txt
;; 20201105_113805--monetary_policy--asset-bubbles-macroprudential-policy.txt
;;
;; EXTENSION is one of ".txt", ".md", ".org" and is subject to a
;; user-facing customisation option.
;;
;;
;; Main points of entry
;; --------------------
;;
;; The aforementioned are handled automatically by the 'usls-new-note'
;; command.  Invoking it brings up a minibuffer prompt for entering the
;; note's title.  Once that is done, it opens a second prompt, with
;; completion, for inputting the category.  The date is always derived
;; automatically.
;;
;; Completion for categories presents a list that combines two sources: (1)
;; a customisable list of "known categories", (2) a dynamic list of
;; inferred categories from existing file names.  The latter is possible
;; due to the assumption that the file name convention is fully respected.
;;
;; To create a new category, just enter text that does not match any of the
;; existing items.  To input multiple categories, separate them with a
;; comma or a space.  If your completion framework does not support such
;; actions, then it should be considered undesirable behaviour and reported
;; upstream.
;;
;; 'usls-new-note' accepts an optional prefix argument, with C-u.  Doing so
;; will start the command with a completion prompt for the subdirectory to
;; be used for the new note.  Subdirectories must already exist in the
;; filesystem, else an error message is displayed.
;;
;; A key feature of 'usls-new-note' is the ability to extract the current
;; region, if active, and place it below the area where the point will be
;; in.  This is useful for quickly capturing some text you wish to comment
;; on and keep it in context.
;;
;; The note's text and the captured region are demarcated by a horizontal
;; rule, denoted by three space-separated asterisks for plain text and
;; markdown (* * *), or five consecutive hyphens for org (-----), plus
;; empty lines before and after the separator.
;;
;;
;; References to other notes
;; -------------------------
;;
;; In the interest of simplicity, usls.el does not maintain a database and
;; does not try to be too smart about linking between notes in the
;; 'usls-directory'.  A "link" is, for our purposes, a plain text reference
;; to the unique identifier of a file (the DATE mentioned above).
;;
;; Inserting such text is made simple with the use of 'usls-id-insert'.  It
;; will produce a minibuffer completion prompt with a list of all notes
;; except the current one.  Selecting an item will place its ID at point,
;; preceded by an uptick.  Like this:
;;
;;     A reference here.^20201108_141930
;;
;; An endnote is also included, now with two successive upticks (^^) which
;; points to the full file name of the referenced entry.  If support for
;; subdirectories is enabled (via 'usls-subdir-support'), such endnotes
;; will include a complete filesystem path.  Otherwise they are assumed as
;; relative to the 'usls-directory'.
;;
;; In the background, the referenced file will get a backward reference as
;; an endnote, now denoted by two adjacent at-signs (@@) followed by the
;; file name of the item where the reference was created.
;;
;; USLS makes sure to remove duplicate backward references whenever a new
;; one is created, but it does not try to update them in case things
;; change.  This is where general-purpose tools come in handy, such as the
;; ability to edit a grep buffer with the wgrep package.  Combine that with
;; your completion framework's directory-wide search or something like the
;; rg.el library to edit references in bulk (e.g. when renaming a file
;; name).
;;
;; To visit the reference at point, one can rely on Emacs' ability to
;; identify a file name contextually (among others).  Type C-x C-f or M-x
;; find-file and follow it up with M-n.  You will get the file-at-point
;; (i.e. the referenced entry) as the selected item.  Or call the command
;; 'usls-follow-link' which uses minibuffer completion, with candidates
;; being the file references documented in the endnotes.
;;
;;
;; Accessing notes
;; ---------------
;;
;; Two commands allow you to quickly visit your notes with usls.el:
;;
;; 1. 'usls-dired' will produce a dired buffer with the contents of the
;;    'usls-directory'.
;;
;; 2. 'usls-find-file' uses minibuffer completion to run 'find-file' on the
;;    selected entry, with options in the list being all the files in the
;;    'usls-directory'.
;;
;;
;; Standard Emacs commands for extending usls.el
;; ---------------------------------------------
;;
;; As we do not have any intent to reproduce general purpose tools for
;; usls-specific cases, we encourage the usage of existing solutions within
;; the Emacs milieu.  Some ideas:
;;
;; + Use a completion framework, such as Icomplete (built-in), Ivy, Helm,
;;   Selectrum.  Packages such as the Orderless completion style can
;;   further improve your experience, depending on your choice and needs.
;;
;; + Learn how to run directory-wide searches and how to refactor entries
;;   in bulk.  A common workflow involves some grep command and the wgrep
;;   package.  Though you could also use 'ibuffer-do-query-replace',
;;   'dired-do-find-regexp-and-replace', 'multi-occur'.
;;
;; + Benefit from dired's numerous capabilities.
;;
;;   * For example, the key sequence '% m' (dired-mark-files-regexp) lets
;;     you mark files based on a regular expression or just a string.  Say
;;     you wish to only see notes about "Emacs".  Do '% m Emacs', then
;;     toggle the mark with 't' and complete the process with 'k'.  What
;;     you just did is to remove from view all entries that do no match the
;;     pattern you searched for.  Bring everything back to the standard
;;     view with 'g'.
;;
;;   * Another neat feature of dired is 'dired-goto-file' which is bound to
;;     'j' by default.  It lets you jump to the line of a given file using
;;     minibuffer completion.  So if your completion framework supports
;;     fuzzy search or out-of-order matching of regular expression groups,
;;     you can interactively find virtually any file with only a few key
;;     strokes.
;;
;; The principle is to learn how to use Emacs' existing capabilities or
;; extensions to your advantage---not just for usls but for your day-to-day
;; operations.
;;
;;
;; General principles of usls.el
;; -----------------------------
;;
;; This blog post from 2020-10-08 describes the core ideas of usls:
;; <https://protesilaos.com/codelog/2020-10-08-intro-usls-emacs-notes/>.
;; Some references are out-of-date, since the library is expanded to
;; support Org and Markdown file types, while it can be configured to
;; access subdirectories inside the 'usls-directory'.

;;; Code:

(require 'cl-lib)
(require 'crm)
(require 'ffap)
(require 'thingatpt)

;;; User-facing options

(defgroup usls ()
  "Simple tool for plain text notes."
  :group 'files
  :prefix "usls-")

(defcustom usls-directory "~/Documents/notes/"
  "Directory for storing personal notes."
  :group 'usls
  :type 'directory)

(defcustom usls-known-categories '(economics philosophy politics)
  "List of predefined categories for `usls-new-note'.

The implicit assumption is that a category is a single word.  If
you need a category to be multiple words long, use underscores to
separate them.  Do not use hyphens, as those are assumed to
demarcate distinct categories, per `usls--inferred-categories'.

Also see `usls-categories' for a dynamically generated list that
gets combined with this one in relevant prompts."
  :group 'usls
  :type 'list)

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

(defvar usls--link-history '()
  "Used internally by `usls-id-insert' to record links.")

(defvar usls--subdirectory-history '()
  "Used internally by `usls-new-note' to record subdirectories.")

;;; Basic utilities

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

;; REVIEW: any character class that captures those?  It seems to work
;; though...
(defun usls--slug-no-punct (str)
  "Convert STR to a file name slug."
  (replace-regexp-in-string "[][{}!@#$%^&*()_=+'\"?,.\|;:~`]*" "" str))

;; REVIEW: this looks inelegant.  We want to remove spaces or multiple
;; hyphens, as well as a final hyphen.
(defun usls--slug-hyphenate (str)
  "Replace spaces with hyphens in STR."
  (replace-regexp-in-string "-$" "" (replace-regexp-in-string "--+\\|\s+" "-" str)))

(defun usls--sluggify (str)
  "Make STR an appropriate file name slug."
  (downcase (usls--slug-hyphenate (usls--slug-no-punct str))))

;;;; Files in directory

(defun usls--directory-files ()
  "List directory files."
  (let ((path (usls--directory))
        (dotless directory-files-no-dot-files-regexp))
    (unless (file-directory-p path)
      (make-directory path t))
    (if usls-subdir-support
        (directory-files-recursively path ".*" nil t)
      (directory-files path nil dotless t))))

(defun usls--directory-subdirs ()
  "Return list of subdirectories in `usls-directory'."
  (cl-remove-if-not
   (lambda (x)
     (file-directory-p x))
   (directory-files-recursively usls-directory ".*" t t)))

(defun usls--directory-subdirs-prompt ()
  "Handle user input on choice of subdirectory."
  (let* ((subdirs
          (if (eq (usls--directory-subdirs) nil)
              (user-error "No subdirs in `%s'; create them manually"
                          (usls--directory))
            (usls--directory-subdirs)))
         (choice (completing-read "Subdirectory of new note: " subdirs
                                  nil t nil 'usls--subdirectory-history))
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
  (append (usls--inferred-categories) usls-known-categories))

(defun usls--categories-prompt ()
  "Prompt for one or more categories (comma/space separated)."
  (let* ((categories (usls-categories))
         (crm-separator "[, ]")
         (choice (completing-read-multiple "File category: " categories
                                           nil nil nil 'usls--category-history)))
    (if (= (length choice) 1)
        (car choice)
      choice)))

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
  (if (and (> (length categories) 1)
           (not (stringp categories)))
      (dolist (x categories)
        (add-to-history 'usls--category-history x))
    (add-to-history 'usls--category-history categories)))

;;; Templates

(defun usls--file-meta-header (title date categories filename id)
  "Front matter template based on `usls-file-type-extension'.

This helper function is meant to integrate with `usls-new-note'.
As such TITLE, DATE, CATEGORIES, FILENAME, ID are all retrieved
from there."
  (let ((cat (usls--categories-capitalize `,categories)))
    (pcase usls-file-type-extension
      ;; TODO: make those templates somewhat customisable.  We need to
      ;; determine what should be parametrised.
      (".md" `(concat "---" "\n"
                      "title: " ,title "\n"
                      "date: " ,date "\n"
                      "category: " ,cat "\n"
                      "orig_name: " ,filename "\n"
                      "orig_id: " ,id "\n"
                      "---" "\n\n"))
      (".org" `(concat "#+title: " ,title "\n"
                       "#+date: " ,date "\n"
                       "#+category: " ,cat "\n"
                       "#+orig_name: " ,filename "\n"
                       "#+orig_id: " ,id "\n\n"))
      (_ `(concat "title: " ,title "\n"
                  "date: " ,date "\n"
                  "category: " ,cat "\n"
                  "orig_name: " ,filename "\n"
                  "orig_id: " ,id "\n"
                  (make-string 24 ?-) "\n\n")))))

(defun usls--file-region-separator (region)
  "Separator for captured REGION in `usls-new-note'."
  (pcase usls-file-type-extension
    (".org" `(concat "\n\n" (make-string 5 ?-) "\n\n" ,region))
    (_ `(concat "\n\n* * *\n\n" ,region))))

;;; Interactive functions

;;;###autoload
(defun usls-new-note (&optional arg)
  "Create new note with the appropriate metadata and file name.
If the region is active, append it to the newly created file.

This command first prompts for a file title and then for a
category.  The latter supports completion.  To input multiple
categories, separate them with a space or a comma.

With prefix key (\\[universal-argument]) as optional ARG also
prompt for a subdirectory of `usls-directory' to place the new
note in."
  (interactive "P")
  (let* ((subdir (when arg (usls--directory-subdirs-prompt)))
         (title (read-string "File title: " nil 'usls--title-history))
         (categories (usls--categories-prompt))
         (slug (usls--sluggify title))
         (path (file-name-as-directory (or subdir usls-directory)))
         (id (format-time-string usls-id))
         (filename
          (format "%s%s--%s--%s%s"
                  path
                  id
                  (usls--categories-hyphenate categories)
                  slug
                  usls-file-type-extension))
         (date (format-time-string "%F"))
         (region (with-current-buffer (current-buffer)
                   (if (region-active-p)
                       (eval (usls--file-region-separator
                              (buffer-substring-no-properties
                               (region-beginning)
                               (region-end))))
                     ""))))
    (with-current-buffer (find-file filename)
      (insert (eval (usls--file-meta-header title date categories filename id)))
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
  (let* ((file (completing-read "Link to: "
                                (usls--directory-files-not-current)
                                nil t nil 'usls--link-history))
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
  "Visit link referenced in the note using completion."
  (interactive)
  (let ((links (usls--links)))
    (if links
        (find-file
         (completing-read "Follow link: " links nil t))
      (usls-find-file))))

(defun usls--file-name (file)
  "Return properly formatted name of FILE."
  (if usls-subdir-support
     (file-truename file)
    (file-truename (concat (usls--directory) file))))

;;;###autoload
(defun usls-find-file ()
  "Visit a file in `usls-directory' using completion."
  (interactive)
  (let* ((files (usls--directory-files))
         (file (completing-read "Visit file: " files nil t nil 'usls--link-history))
         (item (usls--file-name file)))
    (find-file item)
    (add-to-history 'usls--link-history item)))

;;;###autoload
(defun usls-dired ()
  "Switch to `usls-directory'."
  (interactive)
  (let ((path usls-directory))
    (if (file-directory-p path)
        (dired path)
      (error "`usls-directory' not found"))))

;;; User-facing setup

;; TODO: how to define a prefix key?
;;
;; NOTE: Users are expected to bind this to something more useful.  Did
;; not want to violate key binding conventions.
(global-set-key (kbd "C-c _ d") 'usls-dired)
(global-set-key (kbd "C-c _ f") 'usls-find-file)
(global-set-key (kbd "C-c _ n") 'usls-new-note)

(defvar usls-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c _ i") 'usls-id-insert)
    (define-key map (kbd "C-c _ l") 'usls-follow-link)
    map)
  "Key map for use when variable `usls-mode' is non-nil.")

(defvar usls-mode-hook nil
  "Hook called when variable `usls-mode' is non-nil.")

(define-minor-mode usls-mode
  "Extras for working with `usls' notes.

\\{usls-mode-map}"
  :init-value nil
  :global nil
  :lighter " usls"
  :keymap usls-mode-map
  (run-hooks 'usls-mode-hook))

(defun usls-mode-activate ()
  "Activate mode when inside `usls-directory'."
  (when (or (string-match-p (expand-file-name usls-directory) default-directory)
            (string-match-p usls-directory default-directory))
    (unless (or (string= usls-file-type-extension ".md")
                (string= usls-file-type-extension ".org"))
      (usls-mode 1))))

(add-hook 'find-file-hook #'usls-mode-activate)
(add-hook 'dired-mode-hook #'usls-mode-activate)

(defgroup usls-faces ()
  "Faces for `usls-mode'."
  :group 'faces)

(defface usls-header-data-date
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#0031a9")
    (((class color) (min-colors 88) (background dark))
     :foreground "#2fafff")
    (t :foreground "blue"))
  "Face for header date entry.")

(defface usls-header-data-category
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#721045")
    (((class color) (min-colors 88) (background dark))
     :foreground "#feacd0")
    (t :foreground "magenta"))
  "Face for header category entry.")

(defface usls-header-data-title
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#000000")
    (((class color) (min-colors 88) (background dark))
     :foreground "#ffffff")
    (t :foreground "blue"))
  "Face for header title entry.")

(defface usls-header-data-secondary
  '((((class color) (min-colors 88) (background light))
     :foreground "#093060")
    (((class color) (min-colors 88) (background dark))
     :foreground "#c6eaff")
    (t :inherit (bold shadow)))
  "Face for secondary header information.")

(defface usls-header-data-key
  '((((class color) (min-colors 88) (background light))
     :foreground "#505050")
    (((class color) (min-colors 88) (background dark))
     :foreground "#a8a8a8")
    (t :inherit shadow))
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
     :foreground "#2544bb")
    (((class color) (min-colors 88) (background dark))
     :foreground "#79a8ff")
    (t :inherit font-lock-string-face))
  "Face for file name date in `dired-mode' buffers.")

(defface usls-dired-field-delimiter
  '((t :inherit shadow))
  "Face for file name field delimiters in `dired-mode' buffers.")

(defface usls-dired-field-category
  '((((class color) (min-colors 88) (background light))
     :foreground "#8f0075")
    (((class color) (min-colors 88) (background dark))
     :foreground "#f78fe7")
    (t :inherit font-lock-builtin-face))
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
  `(("\\(title:\\) \\(.*\\)"
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
     (3 'usls-header-data-secondary))
    ("^\\(-\\{24\\}\\|[*\s]\\{5\\}\\)$"
     (1 'usls-section-delimiter))
    ("\\(\\^\\)\\([0-9_]\\{15\\}\\)"
     (1 'escape-glyph)
     (2 'font-lock-variable-name-face))
    (,usls--file-link-regexp
     (1 'escape-glyph)
     (2 'font-lock-constant-face)
     (3 'font-lock-constant-face))
    (,usls-file-regexp
     (1 'usls-dired-field-date)
     (2 'usls-dired-field-category)
     (3 'usls-dired-field-name)
     (4 'usls-dired-field-delimiter)))
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
