;;; prot-fill.el --- Minor fill-mode tweaks for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2021  Protesilaos Stavrou

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
;; This covers my fill-mode extensions, for use in my Emacs setup:
;; https://protesilaos.com/dotemacs.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

(defgroup prot-fill ()
  "Tweak for filling paragraphs."
  :group 'fill)

(defcustom prot-fill-default-column 72
  "Default width for `fill-column'."
  :type 'integer
  :group 'prot-fill)

(defcustom prot-fill-prog-mode-column 80
  "`prog-mode' width for `fill-column'.
Also see `prot-fill-default-column'."
  :type 'integer
  :group 'prot-fill)

(defun prot-fill--fill-prog ()
  "Set local value of `fill-column' for programming modes.
Meant to be called via `prog-mode-hook'."
  (setq-local fill-column prot-fill-prog-mode-column))

;;;###autoload
(define-minor-mode prot-fill-fill-mode
  "Set up fill-mode and relevant variable."
  :init-value nil
  :global t
  (if prot-fill-fill-mode
      (progn
        (setq-default fill-column prot-fill-default-column)
        (add-hook 'prog-mode-hook #'prot-fill--fill-prog)
        (add-hook 'text-mode-hook #'turn-on-auto-fill))
    (setq-default fill-column 70)
    (remove-hook 'prog-mode-hook #'prot-fill--fill-prog)
    (remove-hook 'text-mode-hook #'turn-on-auto-fill)))

(provide 'prot-fill)
;;; prot-fill.el ends here
