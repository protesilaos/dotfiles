;;; prot-pulse.el --- Extend pulse.el for my dotemacs -*- lexical-binding: t -*-

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
;; DEPRECATED 2022-03-11 Use my pulsar.el instead (part of this
;; repository).
;;
;;
;; Extensions to the built-in `pulse.el' library for my Emacs
;; configuration: <https://protesilaos.com/emacs/dotemacs/>.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

(require 'pulse)

(defgroup prot-pulse ()
  "Extensions for `pulse.el'."
  :group 'editing)

(defcustom prot-pulse-pulse-command-list
  '(recenter-top-bottom reposition-window)
  "Commands that should automatically `prot-pulse-pulse-line'.
You must restart function `prot-pulse-advice-commands-mode' for
changes to take effect."
  :type 'list
  :group 'prot-pulse)

(defcustom prot-pulse-line-face 'prot-pulse-line
  "Face to use for the pulse line."
  :type '(choice (face :tag "Subtle style" 'prot-pulse-line)
                 (face :tag "Intense style" 'prot-pulse-line-intense)
                 (face :tag "Other face"))
  :group 'prot-pulse)

(defcustom prot-pulse-delay 0.05
  "Duration in seconds of pulse delay."
  :type 'list
  :group 'prot-pulse)

(defface prot-pulse-line
  '((default :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#8eecf4")
    (((class color) (min-colors 88) (background dark))
     :background "#004065")
    (t :inverse-video t))
  "Default face for `prot-pulse-pulse-line'."
  :group 'prot-pulse)

(defface prot-pulse-line-intense
  '((default :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#f5df23")
    (((class color) (min-colors 88) (background dark))
     :background "#874900")
    (t :inverse-video t))
  "Intense variant face for `prot-pulse-pulse-line'."
  :group 'prot-pulse)

;;;###autoload
(defun prot-pulse-pulse-line (&optional face)
  "Temporarily highlight the current line with optional FACE."
  (interactive)
  (let ((start (if (or (eobp)
                       (eq (line-number-at-pos)
                           (line-number-at-pos (point-max))))
                   (line-beginning-position 0)
                 (line-beginning-position)))
        (end (line-beginning-position 2))
        (pulse-delay prot-pulse-delay)
        (face (or face prot-pulse-line-face)))
    (pulse-momentary-highlight-region start end face)))

(defun prot-pulse--recentre (&optional position)
  "Recentre the window with optional POSITION.
POSITION is an argument passed to `recenter'."
  (let ((pulse-delay prot-pulse-delay))
    (recenter position)
    ;; ;; NOTE 2022-02-06: we do not need this if we add `recenter' to
    ;; ;; the `prot-pulse-pulse-command-list'.  See my prot-emacs.org
    ;; ;; for the relevant configuration.
    ;; (prot-pulse-pulse-line)
    (prot-pulse-show-entry)))

;;;###autoload
(defun prot-pulse-recentre-top ()
  "Reposition at the top and pulse line.
Add this to a hook, such as `imenu-after-jump-hook'."
  (prot-pulse--recentre 0))

;;;###autoload
(defun prot-pulse-recentre-centre ()
  "Recentre and pulse line.
Add this to a hook, such as `imenu-after-jump-hook'."
  (prot-pulse--recentre))

(autoload 'org-at-heading-p "org")
(autoload 'org-show-entry "org")
(autoload 'org-reveal "org")
(autoload 'outline-show-entry "outline")

;;;###autoload
(defun prot-pulse-show-entry ()
  "Reveal index at point in outline views.
To be used with a hook such as `imenu-after-jump-hook'."
  (cond
   ((and (eq major-mode 'org-mode)
         (org-at-heading-p))
    (org-show-entry)
    (org-reveal t))
   ((or (bound-and-true-p prot-outline-minor-mode)
        (bound-and-true-p outline-minor-mode))
    (outline-show-entry))))

(defvar prot-pulse-after-command-hook nil
  "Hook that runs after select commands.
To be used with `advice-add' after those functions declared in
`prot-pulse-pulse-command-list'.")

(defun prot-pulse-after-command (&rest _)
  "Run `prot-pulse-after-command-hook'."
  (run-hooks 'prot-pulse-after-command-hook))

;;;###autoload
(define-minor-mode prot-pulse-advice-commands-mode
  "Set up for `prot-pulse-pulse-command-list'."
  :init-value nil
  :global t
  (if prot-pulse-advice-commands-mode
      (progn
        (dolist (fn prot-pulse-pulse-command-list)
          (advice-add fn :after #'prot-pulse-after-command))
        (add-hook 'prot-pulse-after-command-hook #'prot-pulse-pulse-line))
    (dolist (fn prot-pulse-pulse-command-list)
      (advice-remove fn #'prot-pulse-after-command))
    (remove-hook 'prot-pulse-after-command-hook #'prot-pulse-pulse-line)))

(provide 'prot-pulse)
;;; prot-pulse.el ends here
