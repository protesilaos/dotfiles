;;; prot-modeline.el --- Code for my custom mode line -*- lexical-binding: t -*-

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
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

(defface prot-modeline-intense
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :background "#0000aa" :foreground "#ffffff")
    (((class color) (min-colors 88) (background dark))
     :background "#77aaff" :foreground "#000000")
    (t :inverse-video t))
  "Face for intense mode line constructs.")

(setq mode-line-defining-kbd-macro
      (propertize " KMacro " 'face 'prot-modeline-intense))

(defvar-local prot-modeline-input-method
    '(:eval
      (when current-input-method-title
        (propertize (format " %s" current-input-method-title)
                    'mouse-face 'mode-line-highlight)))
  "Mode line construct to report the multilingual environment.")

(defvar-local prot-modeline-buffer-status
    '(:eval
      (when (file-remote-p default-directory)
        (propertize "@" 'mouse-face 'mode-line-highlight)))
  "Mode line construct for showing remote file name.")

(defun prot-modeline-buffer-identification-face ()
  "Return appropriate face or face list for `prot-modeline-buffer-identification'."
  (let ((file (buffer-file-name)))
    (cond
     ((and (mode-line-window-selected-p)
           file
           (buffer-modified-p))
      '(italic mode-line-buffer-id))
     ((and file (buffer-modified-p))
      'italic)
     ((mode-line-window-selected-p)
      'mode-line-buffer-id))))

(defun prot-modeline--buffer-name ()
  "Return `buffer-name', truncating it if necessary.
The name is truncated if the width of the window is smaller than
`split-width-threshold'."
  (let ((name (buffer-name)))
    (if (< (window-width) split-width-threshold)
        (concat (substring name 0 9) "...")
      name)))

(defun prot-modeline-buffer-name ()
  "Return buffer name, with read-only indicator if relevant."
  (let ((name (prot-modeline--buffer-name)))
    (if buffer-read-only
        (format "%s %s" (char-to-string #xE0A2) name)
      name)))

(defvar-local prot-modeline-buffer-identification
    '(:eval
      (propertize (prot-modeline-buffer-name)
                  'face (prot-modeline-buffer-identification-face)
                  'mouse-face 'mode-line-highlight))
  "Mode line construct for identifying the buffer being displayed.
Propertize the current buffer with the `mode-line-buffer-id'
face.  Let other buffers have no face.")

(defun prot-modeline-major-mode-symbol ()
  "Return appropriate propertized mode line indicator for the major mode."
  (let ((indicator (cond
                    ((derived-mode-p 'text-mode) "§")
                    ((derived-mode-p 'prog-mode) "λ")
                    ((derived-mode-p 'comint-mode) ">_")
                    (t "◦"))))
    (propertize indicator 'face 'shadow)))

(defvar-local prot-modeline-major-mode
    (list
     (propertize "%[" 'face 'error)
     '(:eval
       (concat
        (prot-modeline-major-mode-symbol)
        " "
        (propertize
         (capitalize
          (string-replace
           "-mode"
           ""
           (symbol-name major-mode)))
         'mouse-face 'mode-line-highlight)))
     '(:eval
       (when mode-line-process
         (concat " " mode-line-process)))
     (propertize "%]" 'face 'error))
  "Mode line construct for displaying major modes.")

(defvar-local prot-modeline-align-right
    '(:eval (propertize
             " " 'display
             `((space :align-to
                      (- (+ right right-fringe right-margin)
                         ,(string-width
                           (format-mode-line mode-line-misc-info)))))))
  "Mode line construct to align following elements to the right.
Read Info node `(elisp) Pixel Specification'.")

(defvar-local prot-modeline-kbd-macro
    '(:eval
      (when (and defining-kbd-macro (mode-line-window-selected-p))
        mode-line-defining-kbd-macro))
  "Mode line construct displaying `mode-line-defining-kbd-macro'.
Specific to the current window's mode line.")

(defvar-local prot-modeline-flymake
    '(:eval
      (when (and (bound-and-true-p flymake-mode)
                 (mode-line-window-selected-p))
        flymake-mode-line-format))
  "Mode line construct displaying `flymake-mode-line-format'.
Specific to the current window's mode line.")

(defvar-local prot-modeline-misc-info
    '(:eval
      (when (mode-line-window-selected-p)
        mode-line-misc-info))
  "Mode line construct displaying `mode-line-misc-info'.
Specific to the current window's mode line.")

(defun prot-modeline-diffstat (file)
  "Return shortened Git diff numstat for FILE."
  (when-let* ((output (shell-command-to-string (format "git diff --numstat %s" file)))
              (stats (split-string output "[\s\t]" :omit-nulls "[\s\f\t\n\r\v]+"))
              (added (nth 0 stats))
              (deleted (nth 1 stats)))
    (cond
     ((and (equal added "0") (equal deleted "0"))
      "")
     ((and (not (equal added "0")) (equal deleted "0"))
      (propertize (format "+%s" added) 'face 'shadow))
     ((and (equal added "0") (not (equal deleted "0")))
      (propertize (format "-%s" deleted) 'face 'shadow))
     (t
      (propertize (format "+%s -%s" added deleted) 'face 'shadow)))))

(defvar-local prot-modeline-vc-branch
    '(:eval
      (when-let* (((mode-line-window-selected-p))
                  (file (buffer-file-name))
                  ((vc-git-registered file))
                  (branches (vc-git-branches))
                  (branch (car branches))
                  (state (vc-state file 'Git))
                  ;; (face (pcase state
                  ;;         ('added 'vc-locally-added-state)
                  ;;         ('edited 'vc-edited-state)
                  ;;         ('removed 'vc-removed-state)
                  ;;         ('missing 'vc-missing-state)
                  ;;         ('conflict 'vc-conflict-state)
                  ;;         ('locked 'vc-locked-state)
                  ;;         (_ 'vc-up-to-date-state)))
                  )
        (concat
         (propertize (char-to-string #xE0A0) 'face 'shadow)
         " "
         (propertize (capitalize branch)
                     ;; 'face face
                     'mouse-face 'highlight
                     'help-echo (vc-git-working-revision file))
         " "
         (prot-modeline-diffstat file))))
  "Mode line construct to return propertized VC branch.")

;; NOTE 2023-04-28: The `risky-local-variable' is critical, as those
;; variables will not work without it.
(dolist (construct '( prot-modeline-major-mode prot-modeline-align-right
                      prot-modeline-kbd-macro prot-modeline-flymake
                      prot-modeline-vc-branch prot-modeline-misc-info
                      prot-modeline-buffer-identification prot-modeline-buffer-status
                      prot-modeline-input-method))
  (put construct 'risky-local-variable t))

(provide 'prot-modeline)
;;; prot-modeline.el ends here
