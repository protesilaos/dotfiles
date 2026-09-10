;;; prot-vc.el --- Extensions for VC -*- lexical-binding: t -*-

;; Copyright (C) 2026  Protesilaos

;; Author: Protesilaos <info@protesilaos.com>
;; URL: https://protesilaos.com/emacs/dotemacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "31.1"))

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
;; This covers my VC-related extensions, for use in my Emacs setup:
;; https://protesilaos.com/emacs/dotemacs.

;;; Code:

;;;; Window and buffer behaviour

(define-advice vc-push (:around (&rest args) prot-vc)
  (let ((current-window (selected-window)))
    (apply args)
    (select-window current-window)))

(define-advice vc-pull (:around (&rest args) prot-vc)
  (let ((current-window (selected-window)))
    (apply args)
    (select-window current-window)))

;;;; Diff

;;;###autoload
(defun prot-vc-diff-dwim ()
  "Show diff of buffer against file or against VC history."
  (interactive)
  (if-let* ((buffer (current-buffer))
            (_ (buffer-modified-p buffer)))
      (diff-buffer-with-file buffer)
    (call-interactively #'vc-diff)))

;;;; Git grep

(defvar prot-vc-git-grep-history nil
  "Minibuffer history for `prot-vc-git-grep'.")

(defun prot-vc--get-root ()
  "Return VC root."
  (or (vc-root-dir)
      (locate-dominating-file "." ".git")))

;;;###autoload
(defun prot-vc-git-grep (directory regexp)
  "Use `vc-git-grep' with REGEXP in the current root Git DIRECTORY."
  (interactive
   (if-let* ((directory (prot-vc--get-root)))
     (list
      directory
      (read-regexp
       (format "vc-git-grep for REGEXP in `%s': " (propertize directory 'face 'warning))
       nil 'prot-vc-git-grep-history))
     (user-error "No Git directory found")))
  (vc-git-grep regexp "*" directory))

;;;; VC Clone

(defvar prot-vc-clone-types
  '((personal . "~/Git/Projects/")
    (emacs-community . "~/Git/emacs-community/")
    (general . "~/Git/general/")
    (build . "~/Builds/"))
  "Alist of (SYMBOL . PATH) for cloning Git repositories.
SYMBOL is an arbitrary symbol describing the type of Git repository,
which PATH is its corresponding filesystem location.")

(defvar prot-vc-clone-type-prompt-history nil
  "Minibuffer history for `prot-vc-clone-type-prompt'.")

(defun prot-vc-clone--get-path (type)
  "Return path corresponding to TYPE."
  (and-let* ((type (intern type))
             (path (alist-get type prot-vc-clone-types)))))

(defun prot-vc-clone--format-name (url)
  "Format name of VC project based on URL."
  (let* ((strings (split-string url "[/]" :omit-nulls))
         (length (length strings))
         (butlast (nth (- length 2) strings))
         (last (nth (- length 1) strings))
         (last-two (list butlast last)))
    (string-join last-two "-")))

(defun prot-vc-clone-type-annotate (type)
  "Annotate TYPE among `prot-vc-clone-types' with its path."
  (when-let* ((path (prot-vc-clone--get-path type)))
    (format " -- %s" path)))

(defvar prot-vc-clone-completion-metadata
  '((annotation-function . prot-vc-clone-type-annotate))
  "Completion metadata for `prot-vc-clone-type-prompt'.")

(defun prot-vc-clone-type-prompt ()
  "Prompt for type among `prot-vc-clone-types' and return its path."
  (let ((symbols (mapcar #'car prot-vc-clone-types))
        (default (car prot-vc-clone-type-prompt-history)))
    (completing-read
     (format-prompt "Select VC project type" default)
     (completion-table-with-metadata symbols prot-vc-clone-completion-metadata)
     nil t nil 'prot-vc-clone-type-prompt-history default)))

;;;###autoload
(defun prot-vc-clone (remote type)
  "Prompt for REMOTE of TYPE and clone it outright."
  (interactive
   (list
    (read-string "Remote: " nil 'vc--remotes-history)
    (prot-vc-clone-type-prompt)))
  (if-let* ((path (prot-vc-clone--get-path type))
            (name (prot-vc-clone--format-name remote))
            (directory (expand-file-name name path)))
      (vc-clone remote 'Git directory nil t)
    (error "The type `%s' does not have a path in `prot-vc-clone-types'" type)))

(provide 'prot-vc)
;;; prot-vc.el ends here
