;;; prot-outline.el --- Extend outline.el for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2020-2021  Protesilaos Stavrou

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos.com/dotemacs
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
;; Extensions to the built-in `outline.el' library for my Emacs
;; configuration: <https://protesilaos.com/dotemacs/>.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

;; TODO 2021-04-15: This file needs to be reviewed in light of changes
;; in Emacs28.

(require 'outline)
(require 'imenu)
(require 'prot-common)

;;; Commands for outline visibility

;;;###autoload
(defun prot-outline-hide-all ()
  "Hide all `outline-mode' subtrees."
  (interactive)
  (outline-map-region 'outline-hide-subtree (point-min) (point-max)))

;;;###autoload
(defun prot-outline-down-heading ()
  "Move to the next `outline-mode' subtree."
  (interactive)
  ;; Hacky, but it kinda works.
  (outline-up-heading 1 t)
  (outline-forward-same-level 1))

;;;###autoload
(defun prot-outline-cycle-dwim ()
  "Convenience wrapper around common `outline-mode' commands.
When constructs are hidden, show everything.  While on a
headline, or an invisible part of the overlay, cycle the item's
visibility.  Else call `indent-for-tab-command'."
  (interactive)
  (cond
   ((eq (outline--cycle-state) (or 'hide-all 'headings-only))
    (outline-show-all))
   ((or (outline-on-heading-p) (outline-invisible-p))
    (outline-cycle))
   (t
    (indent-for-tab-command))))

;;; Minor mode setup

(autoload 'org-src-mode "org-src")
(defvar outline-minor-faces--font-lock-keywords)

;;;###autoload
(defun prot-outline-refontify-buffer ()
  "Re-enable the current buffer's major mode.
Add this to `prot-outline-minor-mode-exit-hook'."
  (let ((minor-modes (prot-common-minor-modes-active)))
    (when (featurep 'outline-minor-faces)
      (font-lock-remove-keywords nil outline-minor-faces--font-lock-keywords))
    (when (or (derived-mode-p 'text-mode)
              (derived-mode-p 'prog-mode)
              (derived-mode-p 'diary-mode))
      (funcall major-mode)
      ;; REVIEW: Are there any other minor modes we need to account for?
      ;; If so, create a defvar and check it here.
      (when (member 'org-src-mode minor-modes)
        (org-src-mode)))))

(defvar prot-outline-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-n") 'outline-next-visible-heading)
    (define-key map (kbd "C-c C-p") 'outline-previous-visible-heading)
    (define-key map (kbd "C-c C-f") 'outline-forward-same-level)
    (define-key map (kbd "C-c C-b") 'outline-backward-same-level)
    ;; (define-key map (kbd "C-c C-a") 'outline-show-all)
    (define-key map (kbd "C-c C-q") 'prot-outline-hide-all)
    (define-key map (kbd "C-c C-u") 'outline-up-heading)
    (define-key map (kbd "C-c C-d") 'prot-outline-down-heading)
    (define-key map (kbd "C-c C-v") 'outline-move-subtree-down)
    (define-key map (kbd "C-c M-v") 'outline-move-subtree-up)
    (define-key map (kbd "<tab>") 'prot-outline-cycle-dwim)
    map)
  "Custom keymap for working with Outlines.")

(defvar prot-outline-minor-mode-enter-hook nil
  "Hook used if variable `prot-outline-minor-mode' is non-nil.")

(defvar prot-outline-minor-mode-exit-hook nil
  "Hook called when variable `prot-outline-minor-mode' is nil.")

;;;###autoload
(define-minor-mode prot-outline-minor-mode
  "Toggle Outline minor mode and extras.

\\{prot-outline-minor-mode-map}"
  :init-value nil
  :lighter " =~"
  :keymap prot-outline-minor-mode-map
  :global nil
  (if prot-outline-minor-mode
      (progn
        (outline-minor-mode 1)
        (run-hooks 'prot-outline-minor-mode-enter-hook))
    (outline-minor-mode -1)
    (run-hooks 'prot-outline-minor-mode-exit-hook)))

(add-hook 'prot-outline-minor-mode-exit-hook #'prot-outline-refontify-buffer)

;; TODO: which other modes could prove problematic?
(defvar prot-outline-major-modes-blocklist '(org-mode outline-mode markdown-mode))

;;;###autoload
(defun prot-outline-minor-mode-safe ()
  "Test to set variable `prot-outline-minor-mode' to non-nil."
  (interactive)
  (let ((blocklist prot-outline-major-modes-blocklist)
        (mode major-mode))
    (when (derived-mode-p (car (member mode blocklist)))
      (error "Don't use `prot-outline-minor-mode' with `%s'" mode))
    (if (eq prot-outline-minor-mode nil)
        (prot-outline-minor-mode 1)
      (prot-outline-minor-mode -1))))

;;; Imenu bindings

(defun prot-outline-imenu-heading ()
  "Move to the previous `outline-mode' heading.
This is because `imenu' produces its index by moving backward
from the bottom."
  (outline-previous-heading))

;; FIXME: breaks `flymake-mode' (because it returns a string?)
(defun prot-outline-imenu-title ()
  "Return current line and text of the `outline-mode' heading.
To be used by `imenu-extract-index-name-function'."
  (format "%d %s"
          (line-number-at-pos nil t)
          ;; NOTE: I actually prefer the output of `buffer-substring'
          ;; over `buffer-substring-no-properties'.  It is not related
          ;; to the above "fixme", though it might cause problems in
          ;; some cases (none that I know of).
          (buffer-substring (line-beginning-position)
                            (line-end-position))))

(defun prot-outline-imenu-setup ()
  "`imenu' bindings for the local `outline-mode' buffer.
To be used in tandem with `prot-outline-minor-mode-enter-hook'."
  (setq-local imenu-prev-index-position-function
              'prot-outline-imenu-heading)
  (setq-local imenu-extract-index-name-function
              'prot-outline-imenu-title))

(defun prot-outline-imenu-restore ()
  "Restore `imenu' list when variable `outline-minor-mode' is nil.
The new index should be the same as the one you would get in a
standard invocation of `imenu'.

To be used in `prot-outline-minor-mode-exit-hook'."
  (dolist (var '(imenu-prev-index-position-function
                 imenu-extract-index-name-function))
    (kill-local-variable var))
  (ignore-errors
    (save-excursion
      (imenu-default-create-index-function))
    (message "Refreshed `imenu' index")))

(add-hook 'prot-outline-minor-mode-enter-hook #'prot-outline-imenu-setup)
(add-hook 'prot-outline-minor-mode-exit-hook #'prot-outline-imenu-restore)

(provide 'prot-outline)
;;; prot-outline.el ends here
