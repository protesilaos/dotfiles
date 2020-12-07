;;; prot-logos.el --- Extensions for my dotemacs to help read, write, present -*- lexical-binding: t -*-

;; Copyright (C) 2020  Protesilaos Stavrou

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
;; Extensions to help me read, write, present.  For use in my Emacs
;; setup: https://protesilaos.com/dotemacs.

;;; Code:

(when (featurep 'org-tree-slide)
  (require 'org-tree-slide))
(when (featurep 'org-indent)
  (require 'org-indent))
(when (featurep 'olivetti)
  (require 'olivetti))
(require 'face-remap)
(require 'org)

(defgroup prot-logos ()
  "Setup for reading and presenting text-heavy buffers."
  :group 'files)

(defvar prot-logos--focus-mode-hook nil
  "Hook that runs from function `prot-logos-focus-mode'.")

;;;###autoload
(define-minor-mode prot-logos-focus-mode
  "Buffer-local wrapper mode for presentations.
Other tools should hook into `prot-logos--focus-mode-hook' to
introduce their effects.  Otherwise this minor mode has no effect
on its own."
  :init-value nil
  :global nil
  :lighter " -Λ-"			; greek lambda majuscule
  (run-hooks 'prot-logos--focus-mode-hook))

(declare-function buffer-face-mode "face-remap")
(declare-function variable-pitch-mode "face-remap")

(defun prot-logos--variable-pitch-toggle ()
  "Make text use `variable-pitch' face, except for programming."
  (unless (derived-mode-p 'prog-mode)
    (if (or (bound-and-true-p buffer-face-mode)
	        (not (bound-and-true-p prot-logos-focus-mode)))
	    (variable-pitch-mode -1)
      (variable-pitch-mode 1))))

(add-hook 'prot-logos--focus-mode-hook #'prot-logos--variable-pitch-toggle)

(declare-function olivetti-mode "olivetti")

(defun prot-logos--olivetti-toggle ()
  "Toggle the variable `olivetti-mode', if available."
  (if (or (bound-and-true-p olivetti-mode)
          (not (bound-and-true-p prot-logos-focus-mode)))
	  (olivetti-mode -1)
	(olivetti-mode 1)))

(add-hook 'prot-logos--focus-mode-hook #'prot-logos--olivetti-toggle)

(defun prot-logos--fringe-toggle ()
  "Toggle fringe width."
  (if (or (= (car (window-fringes)) 0)
          (not (bound-and-true-p prot-logos-focus-mode)))
      (set-window-fringes (selected-window) nil)
    (set-window-fringes (selected-window) 0 0)))

(add-hook 'prot-logos--focus-mode-hook #'prot-logos--fringe-toggle)

(defcustom prot-logos-org-presentation nil
  "Whether Org files should always switch to presentation view.
This concerns cases where variable `prot-logos-focus-mode' is set
to non-nil and determines whether headings should be converted
into pseudo slides and indentation be adjusted accordingly."
  :type 'boolean
  :group 'prot-logos)

(declare-function org-tree-slide-mode "org-tree-slide")

(defun prot-logos--org-tree-slide-mode ()
  "Toggle variable `org-tree-slide-mode' if loaded and needed."
  (let* ((buf (window-buffer (get-mru-window)))
         (mode (with-current-buffer buf major-mode)))
    (when (and prot-logos-org-presentation
               (eq mode 'org-mode))
	  (if (or (bound-and-true-p org-tree-slide-mode)
		      (not (bound-and-true-p prot-logos-focus-mode)))
	      (org-tree-slide-mode -1)
	    (org-tree-slide-mode 1)))))

(add-hook 'prot-logos--focus-mode-hook #'prot-logos--org-tree-slide-mode)

(declare-function org-indent-mode "org")

(defun prot-logos--org-indent-mode ()
  "Toggle variable `org-tree-slide-mode' if loaded and needed."
  (let* ((buf (window-buffer (get-mru-window)))
         (mode (with-current-buffer buf major-mode)))
    (when (and prot-logos-org-presentation
               (eq mode 'org-mode))
	  (if (or (bound-and-true-p org-indent-mode)
		      (not (bound-and-true-p prot-logos-focus-mode)))
	      (org-indent-mode -1)
	    (org-indent-mode 1)))))

(add-hook 'prot-logos--focus-mode-hook #'prot-logos--org-indent-mode)

(provide 'prot-logos)
;;; prot-logos.el ends here
