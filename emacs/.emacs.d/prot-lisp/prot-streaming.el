;;; prot-streaming.el --- Tweaks for when I do videos or share my screen -*- lexical-binding: t -*-

;; Copyright (C) 2026  Protesilaos

;; Author: Protesilaos <info@protesilaos.com>
;; URL: https://protesilaos.com/emacs/dotemacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))

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
;; This covers my streaming-related extensions, for use in my Emacs
;; setup: https://protesilaos.com/emacs/dotemacs.

;;; Code:

(require 'display-line-numbers)
(require 'hl-line)

(defcustom prot-streaming-mode-modes
  '(text-mode prog-mode dired-mode comint-mode)
  "Major modes where `prot-streaming-mode' takes effect.")

(defun prot-streaming-mode--display-line-numbers (arg)
  "Call `display-line-numbers-mode' with ARG.
Do it for all buffers whose major mode is one among `prot-streaming-mode-modes'."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p prot-streaming-mode-modes)
        (display-line-numbers-mode arg)))))

(defun prot-streaming--get-mode-hooks ()
  "Return hooks belonging to `prot-streaming-mode-modes'."
  (mapcar
   (lambda (symbol)
     (intern (format "%s-hook" symbol)))
   prot-streaming-mode-modes))

(defun prot-streaming-mode--affect-hooks (add-or-remove)
  "Apply ADD-OR-REMOVE to `prot-streaming-mode-hooks'.
ADD-OR-REMOVE is the keyword `:add' or `:remove', which correspond to
`add-hook' and `remove-hook'."
  (let ((fn (pcase add-or-remove
              (:add #'add-hook)
              (:remove #'remove-hook)
              (_ (error "ADD-OR-REMOVE must be either `:add' or `:remove'")))))
    (dolist (hook (prot-streaming--get-mode-hooks))
      (funcall fn hook #'display-line-numbers-mode))))

;;;###autoload
(define-minor-mode prot-streaming-mode
  "Enable extras for streaming."
  :global t
  :init-value nil
  (if prot-streaming-mode
      (progn
        (prot-streaming-mode--display-line-numbers 1)
        (prot-streaming-mode--affect-hooks :add)
        (global-hl-line-mode 1))
    (prot-streaming-mode--display-line-numbers -1)
    (prot-streaming-mode--affect-hooks :remove)
    (global-hl-line-mode -1)))

(provide 'prot-streaming)
;;; prot-streaming.el ends here
