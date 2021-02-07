;;; prot-cursor.el --- Extensions for the cursor -*- lexical-binding: t -*-

;; Copyright (C) 2021  Protesilaos Stavrou

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
;; Extensions for the cursor, intended for my Emacs setup:
;; <https://protesilaos.com/dotemacs/>.

;;; Code:

;;;; General utilities

(defgroup prot-cursor ()
  "Tweaks for cursor appearance."
  :group 'cursor)

(defvar prot-minibuffer-mini-cursors)

;;;###autoload
(define-minor-mode prot-cursor-presentation-mode ()
  :init-value nil
  :global t
  (if prot-cursor-presentation-mode
      (progn
        (setq-default prot-minibuffer-mini-cursors nil) ; from `prot-minibuffer.el'
        (setq-default cursor-type 'box)
        (setq-default cursor-in-non-selected-windows 'hollow)
        (setq-default blink-cursor-blinks 10)
        (setq-default blink-cursor-interval 0.5)
        (setq-default blink-cursor-delay 0.2)
        (blink-cursor-mode 1))
    (setq-default prot-minibuffer-mini-cursors t)
    (setq-default cursor-type '(hbar . 3))
    (setq-default cursor-in-non-selected-windows 'hollow)
    (setq-default blink-cursor-blinks 50)
    (setq-default blink-cursor-interval 0.2)
    (setq-default blink-cursor-delay 0.2)
    (blink-cursor-mode 1)))

(provide 'prot-cursor)
;;; prot-cursor.el ends here
