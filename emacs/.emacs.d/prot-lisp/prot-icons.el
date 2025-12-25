;;; prot-icons.el --- Get characters, icons, and symbols for things -*- lexical-binding: t -*-

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
;; Extensions to get icons and symbols for files, buffers, or related.
;; Intended for my Emacs setup: <https://protesilaos.com/emacs/dotemacs/>.

;;; Code:

(require 'prot-common)

(defgroup prot-icons nil
  "Get characters, icons, and symbols for things."
  :group 'convenience)

(defface prot-icons-icon
  '((t :inherit (bold fixed-pitch)))
  "Basic attributes for an icon."
  :group 'prot-icons)

(defface prot-icons-red
  '((default :inherit prot-icons-icon)
    (((class color) (min-colors 88) (background light))
     :foreground "#aa3232")
    (((class color) (min-colors 88) (background dark))
     :foreground "#f06464")
    (t :foreground "red"))
  "Face for icons."
  :group 'prot-icons)

(defface prot-icons-green
  '((default :inherit prot-icons-icon)
    (((class color) (min-colors 88) (background light))
     :foreground "#107010")
    (((class color) (min-colors 88) (background dark))
     :foreground "#33bb33")
    (t :foreground "green"))
  "Face for icons."
  :group 'prot-icons)

(defface prot-icons-yellow
  '((default :inherit prot-icons-icon)
    (((class color) (min-colors 88) (background light))
     :foreground "#605000")
    (((class color) (min-colors 88) (background dark))
     :foreground "#e0a055")
    (t :foreground "yellow"))
  "Face for icons."
  :group 'prot-icons)

(defface prot-icons-blue
  '((default :inherit prot-icons-icon)
    (((class color) (min-colors 88) (background light))
     :foreground "#223399")
    (((class color) (min-colors 88) (background dark))
     :foreground "#5599ff")
    (t :foreground "blue"))
  "Face for icons."
  :group 'prot-icons)

(defface prot-icons-magenta
  '((default :inherit prot-icons-icon)
    (((class color) (min-colors 88) (background light))
     :foreground "#8f2270")
    (((class color) (min-colors 88) (background dark))
     :foreground "#ee70aa")
    (t :foreground "magenta"))
  "Face for icons."
  :group 'prot-icons)

(defface prot-icons-cyan
  '((default :inherit prot-icons-icon)
    (((class color) (min-colors 88) (background light))
     :foreground "#226067")
    (((class color) (min-colors 88) (background dark))
     :foreground "#77b0c0")
    (t :foreground "cyan"))
  "Face for icons."
  :group 'prot-icons)

(defface prot-icons-gray
  '((default :inherit prot-icons-icon)
    (((class color) (min-colors 88) (background light))
     :foreground "gray30")
    (((class color) (min-colors 88) (background dark))
     :foreground "gray70")
    (t :foreground "gray"))
  "Face for icons."
  :group 'prot-icons)

(defface prot-icons-faint
  '((default :inherit prot-icons-icon)
    (((class color) (min-colors 88) (background light))
     :foreground "gray70")
    (((class color) (min-colors 88) (background dark))
     :foreground "gray30")
    (t :foreground "gray"))
  "Face for icons."
  :group 'prot-icons)

(defvar prot-icons
  '((dired-mode "|+" prot-icons-gray)
    (archive-mode "|@" prot-icons-gray)
    (diff-mode ">Δ" prot-icons-gray) ; διαφορά
    (prog-mode ">Π" prot-icons-magenta) ; πρόγραμμα
    (conf-mode ">Π" prot-icons-faint) ; πρόγραμμα
    (text-mode ">Α" prot-icons-green) ; αλφάβητο
    (comint-mode ">_" prot-icons-gray)
    (document ">Σ" prot-icons-red) ; σύγγραμμα
    (audio ">Η" prot-icons-cyan) ; ήχος
    (image ">Ε" prot-icons-yellow) ; εικόνα
    (video ">Κ" prot-icons-blue) ; κίνηση (κινηματογράφος)
    (t ">." prot-icons-faint))
  "Major modes or concepts and their corresponding icons.
Each element is a cons cell of the form (THING STRING FACE), where THING
is a symbol STRING is one or more characters that represent THING, and
FACE is the face to use for it, where applicable.")

(defun prot-icons--get (thing)
  "Return `prot-icons' representation of THING."
  (unless (symbolp thing)
    (error "The thing `%s' is not a symbol" thing))
  (when (string-suffix-p "-mode" (symbol-name thing))
    (while-let ((parent (get thing 'derived-mode-parent)))
      (setq thing parent)))
  (or (alist-get thing prot-icons)
      (alist-get t prot-icons)))

(defun prot-icons-get-icon (thing &optional face)
  "Return propertized icon THING."
  (pcase-let ((`(,icon ,inherent-face) (prot-icons--get thing)))
    (format "%2s" (propertize icon 'font-lock-face (or face inherent-face)))))

(defun prot-icons-get-file-icon (file)
  "Return FILE icon and face."
  (cond
   ((null file)
    (prot-icons-get-icon nil))
   ((string-suffix-p "/" file)
    (prot-icons-get-icon 'dired-mode))
   ((string-match-p (prot-common--get-file-type-regexp 'archive) file)
    (prot-icons-get-icon 'archive-mode))
   ((string-match-p (prot-common--get-file-type-regexp 'text) file)
    (prot-icons-get-icon 'text-mode))
   ((string-match-p (prot-common--get-file-type-regexp 'image) file)
    (prot-icons-get-icon 'image))
   ((string-match-p (prot-common--get-file-type-regexp 'audio) file)
    (prot-icons-get-icon 'audio))
   ((string-match-p (prot-common--get-file-type-regexp 'video) file)
    (prot-icons-get-icon 'video))
   ((string-match-p (prot-common--get-file-type-regexp 'document) file)
    (prot-icons-get-icon 'document))
   ((string-match-p (prot-common--get-file-type-regexp 'diff) file)
    (prot-icons-get-icon 'diff-mode))
   ((string-match-p (prot-common--get-file-type-regexp 'program) file)
    (prot-icons-get-icon 'prog-mode))
   ((string-match-p (prot-common--get-file-type-regexp 'program-data) file)
    (prot-icons-get-icon 'conf-mode))
   (t (prot-icons-get-icon nil))))

;;;; Icons for Dired

;; Adapted from `nerd-icons-dired'

(defun prot-icons-dired--add-overlay (pos string)
  "Add overlay to display STRING at POS."
  (let ((overlay (make-overlay (1- pos) pos)))
    (overlay-put overlay 'prot-icons-overlay t)
    (overlay-put overlay 'evaporate t)
    (overlay-put overlay 'before-string (propertize string 'display string))
    (overlay-put overlay 'after-string (propertize " " 'display '(space :align-to 1)))))

(defun prot-icons-dired--remove-all-overlays ()
  "Remove all `prot-icons' overlays."
  (dolist (buffer (buffer-list))
    (when (and (derived-mode-p 'dired-mode) prot-icons-dired-mode)
      (save-restriction
        (widen)
        (remove-overlays nil nil 'prot-icons-overlay t)))))

(defun prot-icons-dired--annotate ()
  "Add icons to all files in the visible region of the buffer."
  (save-excursion
    (goto-char (point-min))
    (while (and (dired-next-line 1) (not (eobp)))
      (when-let* ((file (dired-get-filename nil :no-error))
                  (icon (if (file-directory-p file)
                            (prot-icons-get-file-icon (concat file "/"))
                          (prot-icons-get-file-icon file))))
        (prot-icons-dired--add-overlay (dired-move-to-filename) icon)))))

(defun prot-icons-dired--refresh ()
  "Update the display of icons of files in a Dired buffer."
  (prot-icons-dired--remove-all-overlays)
  (save-restriction
    (widen)
    (prot-icons-dired--annotate)))

(defun prot-icons-dired--setup ()
  "Set up Dired to display icons."
  (setq-local tab-width 1)
  (prot-icons-dired--refresh))

;;;###autoload
(define-minor-mode prot-icons-dired-mode
  "Display icons for Dired entries."
  :global t
  (if prot-icons-dired-mode
      (progn
        (add-hook 'dired-mode-hook #'prot-icons-dired--setup)
        (add-hook 'dired-after-readin-hook 'prot-icons-dired--annotate)
        (advice-add 'wdired-abort-changes :after #'prot-icons-dired--refresh))
    (prot-icons-dired--remove-all-overlays)
    (remove-hook 'dired-mode-hook #'prot-icons-dired--setup)
    (remove-hook 'dired-after-readin-hook 'prot-icons-dired--annotate)
    (advice-remove 'wdired-abort-changes #'prot-icons-dired--refresh)))

;;;; Icons for Xref

;; Adapted from `nerd-icons-xref'

(defun prot-icons-xref--add-overlay (position string)
  "Add overlay at POSITION to display STRING."
  (let ((overlay (make-overlay position (+ position 1))))
    (overlay-put overlay 'prot-icons-overlay t)
    (overlay-put overlay 'evaporate t)
    (overlay-put overlay 'before-string (propertize string 'display string))
    (overlay-put overlay 'after-string (propertize " " 'display '(space :align-to 1)))))

(defun prot-icons-xref--add-icons ()
  "Add icons to Xref headings."
  (save-excursion
    (goto-char (point-min))
    (let ((prop))
      (while (setq prop (text-property-search-forward 'xref-group))
        (when-let* ((start (prop-match-beginning prop))
                    (end (prop-match-end prop))
                    (file (string-chop-newline (buffer-substring-no-properties start end)))
                    (icon (prot-icons-get-file-icon file)))
          (prot-icons-xref--add-overlay start icon))))))

;;;###autoload
(define-minor-mode prot-icons-xref-mode
  "Display icons for Xref headings."
  :global t
  (if prot-icons-xref-mode
      (add-hook 'xref-after-update-hook #'prot-icons-xref--add-icons)
    (remove-hook 'xref-after-update-hook #'prot-icons-xref--add-icons)))

;;;; Icons for Buffer menu

(defun prot-icons-buffer-menu--add-overlay (position string)
  "Add overlay at POSITION to display STRING."
  (let ((overlay (make-overlay position (+ position 1))))
    (overlay-put overlay 'prot-icons-overlay t)
    (overlay-put overlay 'evaporate t)
    (overlay-put overlay 'before-string (propertize string 'display string))))

(defun prot-icons-buffer-menu--add-icons (&rest _)
  "Add icons to `Buffer-menu-mode' entries."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'Buffer-menu-mode)
        (save-excursion
          (goto-char (point-min))
          (while-let ((match (text-property-search-forward 'tabulated-list-id))
                      (buffer (prop-match-value match))
                      (mode (with-current-buffer buffer major-mode))
                      (icon (prot-icons-get-icon mode)))
            (prot-icons-buffer-menu--add-overlay (line-beginning-position) (concat icon " "))))))))

;;;###autoload
(define-minor-mode prot-icons-buffer-menu-mode
  "Display icons for `Buffer-menu-mode' entries."
  :global t
  (if prot-icons-buffer-menu-mode
      (advice-add #'list-buffers--refresh :after #'prot-icons-buffer-menu--add-icons)
    (advice-remove #'list-buffers--refresh #'prot-icons-buffer-menu--add-icons)))

(provide 'prot-icons)
;;; prot-icons.el ends here
