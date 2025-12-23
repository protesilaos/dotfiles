;;; prot-minibuffer.el --- Extensions for the minibuffer and completions -*- lexical-binding: t -*-

;; Copyright (C) 2025  Protesilaos Stavrou

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Protesilaos Stavrou <info@protesilaos.com>
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
;; Extensions for the minibuffer and completions, intended for my
;; Emacs setup: <https://protesilaos.com/emacs/dotemacs/>.

;;; Code:

(require 'prot-common)
(require 'prot-icons)

(defgroup prot-minibuffer nil
  "Extensions for the minibuffer and completions."
  :group 'minibuffer)

;;;; Completion category grouping, sorting, and affixating

;; Add some missing completion categories to let me configure the
;; relevant prompts via the `completion-category-overrides'.
(defun prot-minibuffer@read-from-kill-ring (&rest args)
  (let ((completion-extra-properties (list :category 'kill-ring)))
    (apply args)))

(defun prot-minibuffer@read-library-name (&rest args)
  (let ((completion-extra-properties (list :category 'library)))
    (apply args)))

(defun prot-minibuffer@emoji--read-emoji (&rest args)
  (let ((completion-extra-properties (list :category 'emoji)))
    (apply args)))

;;;###autoload
(define-minor-mode prot-minibuffer-missing-categories-mode
  "When enabled, add missing compleiton categories to relevant prompts."
  :global t
  (if prot-minibuffer-missing-categories-mode
      (dolist (original (list #'read-from-kill-ring #'read-library-name #'emoji--read-emoji))
        (when-let* ((my-function-name (format "prot-minibuffer@%s" original))
                    (my-function-symbol (intern-soft my-function-name)))
          (advice-add original :around my-function-symbol)))
    (dolist (original (list #'read-from-kill-ring #'read-library-name #'emoji--read-emoji))
      (when-let* ((my-function-name (format "prot-minibuffer@%s" original))
                  (my-function-symbol (intern-soft my-function-name)))
        (advice-remove original my-function-symbol)))))

(defun prot-minibuffer-file-sort (files)
  "Sort FILES to have directories first and then `completions-sort' sorting.
Omit the .. directory from FILES."
  (setq files (delete "../" files))
  (setq files (minibuffer-sort-alphabetically files))
  (let ((directory-p (lambda (file) (string-suffix-p "/" file))))
    (nconc (seq-filter directory-p files)
           (seq-remove directory-p files))))

(defun prot-minibuffer-file-affixate (files)
  "Return FILES with prefix and suffix."
  (mapcar
   (lambda (file)
     (list file (format "%s " (prot-icons-get-file-icon file)) ""))
   files))

(defun prot-minibuffer-file-group (file transform)
  "Return FILE group name unless TRANSFORM is non-nil."
  (cond
   (transform file)
   ((string-suffix-p "/" file) "Directory")
   ((string-match-p (prot-common--get-file-type-regexp 'archive) file) "Archive")
   ((string-match-p (prot-common--get-file-type-regexp 'text) file) "Text")
   ((string-match-p (prot-common--get-file-type-regexp 'image) file) "Image")
   ((string-match-p (prot-common--get-file-type-regexp 'audio) file) "Audio")
   ((string-match-p (prot-common--get-file-type-regexp 'video) file) "Video")
   ((string-match-p (prot-common--get-file-type-regexp 'document) file) "Document")
   ((string-match-p (prot-common--get-file-type-regexp 'program) file) "Program")
   (t "Other")))

(defun prot-minibuffer--set-default-sort (candidates)
  "Sort CANDIDATES according to `completions-sort' and return the sorted list."
  (setq candidates
        (pcase completions-sort
          ('nil candidates)
          ('alphabetical (minibuffer-sort-alphabetically candidates))
          ('historical (minibuffer-sort-by-history candidates))
          (_ (funcall completions-sort candidates)))))

(defun prot-minibuffer-symbol-sort (symbols)
  "Sort SYMBOLS so that public ones come first."
  (setq symbols (prot-minibuffer--set-default-sort symbols))
  (let ((private-p (lambda (symbol) (string-suffix-p "--" symbol))))
    (nconc (seq-remove private-p symbols)
           (seq-filter private-p symbols))))

(defun prot-minibuffer--propertize-suffix-with-space (string)
  "Propertize STRING with spacing before it."
  (format " %s%s"
          (if (eq completions-format 'one-column)
              (propertize " " 'display '(space :align-to 60))
            " ")
          (propertize string 'face 'completions-annotations)))

(defun prot-minibuffer-buffer-affixate (buffers)
  "Return BUFFERS with prefix and suffix."
  (mapcar
   (lambda (buffer)
     (let* ((buffer-object (get-buffer buffer))
            (mode (with-current-buffer buffer-object major-mode)))
       (list
        buffer
        (format "%s " (prot-icons-get-icon mode))
        (prot-minibuffer--propertize-suffix-with-space (format "%s" mode)))))
   buffers))

(defun prot-minibuffer-bookmark-affixate (bookmarks)
  "Return BOOKMARKS with prefix and suffix."
  (mapcar
   (lambda (bookmark)
     (let* ((data (bookmark-get-bookmark bookmark))
            (file (bookmark-prop-get data 'filename)))
       (list
        bookmark
        (format "%s " (prot-icons-get-file-icon file))
        (prot-minibuffer--propertize-suffix-with-space (format "%s" file)))))
   bookmarks))

(defun prot-minibuffer-library-sort (libraries)
  "Sort LIBRARIES, omitting autoloads and bytecode files."
  (setq libraries (seq-remove
                   (lambda (library)
                     (string-match-p "\\(-autoload\\|\\.elc\\)" library))
                   libraries))
  (prot-minibuffer--set-default-sort libraries))

;; NOTE 2025-12-19: Maybe there is a better way, but this is okay to start with.
(defun prot-minibuffer-library-annotate (library)
  "Return the group documentation of LIBRARY."
  (when-let* ((group-documentation (get (intern-soft library) 'group-documentation)))
    (prot-minibuffer--propertize-suffix-with-space group-documentation)))

(defun prot-minibuffer-command-annotate (command)
  "Annotate COMMAND with its key binding and shortened documentation string."
  (let ((symbol (intern-soft command)))
    (format "%s%s%s"
            (if-let* ((binding (where-is-internal symbol overriding-local-map t))
                      (description (key-description binding))
                      (key (when (and binding (not (stringp binding)))
                             (format " %s " description))))
                (propertize key 'face 'help-key-binding)
              "")
            (if (eq completions-format 'one-column)
                (propertize " " 'display '(space :align-to 60))
              " ")
            (if-let* ((doc (condition-case nil (documentation symbol) (error nil)))
                      (first-line (substring doc 0 (string-search "\n" doc))))
                (propertize first-line 'face 'completions-annotations)
              ""))))

;;;; Completions

(defun prot-minibuffer-completions-tweak-style ()
  "Tweak the style of the Completions buffer."
  (setq-local mode-line-format nil)
  (setq-local cursor-in-non-selected-windows nil)
  (when (and completions-header-format
             (not (string-blank-p completions-header-format)))
    (setq-local display-line-numbers-offset -1))
  (display-line-numbers-mode 1))

(defun prot-minibuffer-quit-completions ()
  "Always quit the Completions window."
  (when-let* ((window (get-buffer-window "*Completions*")))
    (quit-window nil window)))

(defun prot-minibuffer-choose-completion-no-exit ()
  "Call `choose-completion' without exiting the minibuffer.
Also see `prot-minibuffer-choose-completion-exit' and `prot-minibuffer-choose-completion-dwim'."
  (interactive)
  (choose-completion nil :no-exit :no-quit)
  (switch-to-minibuffer))

(defun prot-minibuffer-choose-completion-exit ()
  "Call `choose-completion' and exit the minibuffer.
Also see `prot-minibuffer-choose-completion-no-exit' and `prot-minibuffer-choose-completion-dwim'."
  (interactive)
  (choose-completion nil :no-exit)
  (exit-minibuffer))

(defun prot-minibuffer-crm-p ()
  "Return non-nil if `completing-read-multiple' is in use."
  (when-let* ((_ (featurep 'crm))
              (window (active-minibuffer-window))
              (buffer (window-buffer window)))
    (buffer-local-value 'crm-completion-table buffer)))

(defun prot-minibuffer-choose-completion-dwim ()
  "Call `choose-completion' that exits only on a unique match.
If the match is not unique, then complete up to the largest common
prefix or, anyhow, continue with the completion (e.g. in `find-file'
switch into the directory and then show the files therein).

Also see `prot-minibuffer-choose-completion-no-exit' and `prot-minibuffer-choose-completion-exit'."
  (interactive)
  (if (prot-minibuffer-crm-p)
      (prot-minibuffer-choose-completion-no-exit)
    (choose-completion nil :no-exit :no-quit)
    (switch-to-minibuffer)
    (minibuffer-completion-help)
    (unless (get-buffer-window "*Completions*")
      (exit-minibuffer))))

(define-advice minibuffer-completion-help (:around (&rest args) prot)
  "Make `minibuffer-completion-help' display *Completions* in a side window.
Make the window be at slot 0, such that the *Help* buffer produced by
`prot-minibuffer-completions-describe-at-point' is to its right."
  (let ((display-buffer-overriding-action
         `((display-buffer-reuse-mode-window display-buffer-in-side-window)
           (mode . completion-list-mode)
           (side . bottom)
           (slot . 0))))
    (apply args)))

(defun prot-minibuffer-completions-describe-at-point (symbol)
  "Describe SYMBOL at point inside the *Completions* buffer.
Place the *Help* buffer in a side window, situated to the right of the
*Completions* buffer.  Make the window have the `prot-minibuffer-help'
property, such that it can be found by `prot-minibuffer-completions-close-help'."
  (interactive (list (intern-soft (thing-at-point 'symbol))))
  (unless (derived-mode-p 'completion-list-mode)
    (user-error "Can only do this from the *Completions* buffer"))
  (when symbol
    (let ((help-window-select nil)
          (display-buffer-overriding-action
           `((display-buffer-reuse-mode-window display-buffer-in-side-window)
             (mode . help-mode)
             (side . bottom)
             (slot . 1)
             (window-height . fit-window-to-buffer)
             (window-parameters . ((prot-minibuffer-help . t))))))
      (describe-symbol symbol))))

(defun prot-minibuffer-completions-close-help ()
  "Close the window that has a `prot-minibuffer-help' parameter."
  (when-let* ((help (seq-find
                     (lambda (window)
                       (window-parameter window 'prot-minibuffer-help))
                     (window-list))))
    (delete-window help)))

;;;###autoload
(define-minor-mode prot-minibuffer-completions-mode
  "Tweak the interface of the minibuffer and the *Completions*."
  :global t
  (if prot-minibuffer-completions-mode
      (progn
        (setq completion-show-help nil)
        (setq completion-show-inline-help nil)
        (setq completions-detailed t)
        (setq completions-format 'one-column)
        (setq completions-header-format "")
        (setq completions-highlight-face 'button)
        (setq completions-max-height 12)
        (setq completions-sort 'historical)
        (setq completion-auto-help t)
        (setq completion-auto-select t)
        (setq completion-eager-display 'auto)
        (setq completion-eager-update 'auto)
        (add-hook 'completion-list-mode-hook #'prot-minibuffer-completions-tweak-style)
        (add-hook 'minibuffer-exit-hook #'prot-minibuffer-quit-completions)
        (add-hook 'minibuffer-exit-hook #'prot-minibuffer-completions-close-help))
    (setq completion-show-help t)
    (setq completion-show-inline-help nil)
    (setq completions-detailed nil)
    (setq completions-format 'horizontal)
    (setq completions-header-format (propertize "%s possible completions:\n" 'face 'shadow))
    (setq completions-highlight-face 'completions-highlight)
    (setq completions-max-height nil)
    (setq completions-sort 'alphabetical)
    (setq completion-auto-help t)
    (setq completion-auto-select nil)
    (setq completion-eager-display 'auto)
    (setq completion-eager-update 'auto)
    (remove-hook 'completion-list-mode-hook #'prot-minibuffer-completions-tweak-style)
    (remove-hook 'minibuffer-exit-hook #'prot-minibuffer-quit-completions)
    (remove-hook 'minibuffer-exit-hook #'prot-minibuffer-completions-close-help)))

(provide 'prot-minibuffer)
;;; prot-minibuffer.el ends here
