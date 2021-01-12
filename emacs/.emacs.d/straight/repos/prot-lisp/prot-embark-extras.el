;;; prot-embark-extras.el --- Complementary extensions to embark.el for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2021  Protesilaos Stavrou

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos.com/dotemacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4"))

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
;; Complementary extensions to `embark.el' for my Emacs configuration:;
; <https://protesilaos.com/dotemacs/>.

;;; Code:

(when (featurep 'embark)
  (require 'embark))
(require 'prot-consult)
(require 'prot-embark)
(require 'prot-recentf)

(defgroup prot-embark-extras ()
  "Custom cross-package extensions for `embark'."
  :group 'editing)

(declare-function prot-consult-fd "prot-consult")
(declare-function prot-consult-rg "prot-consult")

(defvar prot-embark-extras-become-general-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "f") 'prot-consult-fd)
    (define-key map (kbd "g") 'prot-consult-rg)
    map)
  "General custom cross-package `embark-become' keymap.")

(defvar prot-embark-extras-become-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "l") 'consult-line)
    (define-key map (kbd "i") 'consult-imenu)
    (define-key map (kbd "s") 'consult-outline) ; as my default is 'M-s s'
    map)
  "Line-specific custom cross-package `embark-become' keymap.")

(defvar embark-become-file+buffer-map)
(declare-function prot-recentf-recent-files "prot-recentf")

(defvar prot-embark-extras-become-file+buffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map embark-become-file+buffer-map)
    (define-key map (kbd "r") 'prot-recentf-recent-files)
    (define-key map (kbd "B") 'project-switch-to-buffer)
    (define-key map (kbd "F") 'project-find-file)
    map)
  "File+buffer custom cross-package `embark-become' keymap.")

(defvar embark-become-keymaps)

;;;###autoload
(define-minor-mode prot-embark-extras-keymaps
  "Add or remove keymaps from Embark.
This is based on the value of `prot-embark-extras-add-keymaps'
and is meant to keep things clean in case I ever wish to disable
those so-called 'extras'."
  :init-value nil
  :global t
  (let ((maps (list 'prot-embark-extras-become-general-map
                    'prot-embark-extras-become-line-map
                    'prot-embark-extras-become-file+buffer-map)))
    (if prot-embark-extras-keymaps
        (dolist (map maps)
          (cl-pushnew map embark-become-keymaps))
      (setq embark-become-keymaps
            (dolist (map maps)
              (delete map embark-become-keymaps))))))

;;;; Keycast integration

;; Got this from Embark's wiki.  Renamed it to placate the compiler:
;; <https://github.com/oantolin/embark/wiki/Additional-Configuration>.

(defvar keycast--this-command-keys)
(defvar keycast--this-command)

(defun prot-embark-extras--store-action-key+cmd (cmd)
  "Configure keycast variables for keys and CMD.
To be used as filter-return advice to `embark-keymap-prompter'."
  (setq keycast--this-command-keys (this-single-command-keys)
        keycast--this-command cmd))

(advice-add 'embark-keymap-prompter :filter-return #'prot-embark-extras--store-action-key+cmd)

(defun prot-embark-extras--force-keycast-update (&rest _)
  "Update keycast's mode line.
To be passed as advice before `embark-act' and others."
  (force-mode-line-update t))

;; NOTE: This has a generic name because my plan is to add more packages
;; to it.
;;;###autoload
(define-minor-mode prot-embark-extras-setup-packages
  "Set up advice to integrate Embark with various commands."
  :init-value nil
  :global t
  (if prot-embark-extras-setup-packages
      (dolist (cmd '(embark-act embark-act-noexit embark-become))
        (advice-add cmd :before #'prot-embark-extras--force-keycast-update))
    (dolist (cmd '(embark-act embark-act-noexit embark-become))
      (advice-remove cmd #'prot-embark-extras--force-keycast-update))))

(provide 'prot-embark-extras)
;;; prot-embark-extras.el ends here
