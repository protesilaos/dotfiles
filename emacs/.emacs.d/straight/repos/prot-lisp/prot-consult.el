;;; prot-consult.el --- Tweak consult.el for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2020  Protesilaos Stavrou

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
;; Tweaks for `consult.el' intended for my Emacs configuration:
;; <https://protesilaos.com/dotemacs/>.

;;; Code:

(require 'consult)
(require 'prot-imenu)

(defgroup prot-consult ()
  "Tweaks for consult.el."
  :group 'minibuffer)

(defcustom prot-consult-add-advice-set-hooks nil
  "Whether to `advice-add' certain commands."
  :group 'prot-consult
  :type 'boolean)

(defvar prot-consult-jump-recentre-hook nil
  "Hook that runs after select Consult commands.
To be used with `advice-add'.")

(defvar prot-consult-jump-top-hook nil
  "Hook that runs after select Consult commands.
To be used with `advice-add'.")

(defun prot-consult-after-jump-recentre (&rest _)
  "Run `prot-consult-jump-recentre-hook'."
  (run-hooks 'prot-consult-jump-recentre-hook))

(defun prot-consult-after-jump-top (&rest _)
  "Run `prot-consult-jump-top-hook'."
  (run-hooks 'prot-consult-jump-top-hook))

;;;###autoload
(defun prot-consult-set-up-hooks ()
  "Set up hooks for select Consult commands."
  (if prot-consult-add-advice-set-hooks
      (progn
        (dolist (fn '(consult-line consult-mark))
          (advice-add fn :after #'prot-consult-after-jump-recentre))
        (advice-add #'consult-outline :after #'prot-consult-after-jump-top)
        (add-hook 'prot-consult-jump-recentre-hook #'prot-imenu-recentre-pulse-centre)
        (add-hook 'prot-consult-jump-top-hook #'prot-imenu-recentre-pulse-top)
        (add-hook 'prot-consult-jump-top-hook #'prot-imenu-show-entry))
    (dolist (fn '(consult-line consult-mark))
      (advice-remove fn #'prot-consult-after-jump-recentre))
    (advice-remove #'consult-outline #'prot-consult-after-jump-top)
    (remove-hook 'prot-consult-jump-recentre-hook #'prot-imenu-recentre-pulse-centre)
    (remove-hook 'prot-consult-jump-top-hook #'prot-imenu-recentre-pulse-top)
    (remove-hook 'prot-consult-jump-top-hook #'prot-imenu-show-entry)))

(provide 'prot-consult)
;;; prot-consult.el ends here
