;;; prot-ediff.el --- Ediff extensions for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2025  Protesilaos Stavrou

;; Author: Protesilaos Stavrou <info@protesilaos.com>
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
;; Extensions for Ediff, intended for use in my Emacs setup:
;; https://protesilaos.com/emacs/dotemacs.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:
(require 'ediff)

(defun prot-ediff-store-layout ()
  "Store current frame window configuration as a frame parameter.
Add this function to the `ediff-before-setup-hook'.

Also see `prot-ediff-restore-layout'."
  (let ((frame (selected-frame)))
    (set-frame-parameter
     frame
     'prot-ediff-last-layout
     (current-window-configuration frame))))

(defun prot-ediff-restore-layout ()
  "Restore the frame's window configuration.
Add this function to the `ediff-quit-hook'.

Also see `prot-ediff-store-layout'."
  (if-let* ((layout (frame-parameter (selected-frame) 'prot-ediff-last-layout)))
      (set-window-configuration layout)
    ;; We do not signal a `user-error' here because that would prevent
    ;; `ediff-quit' from closing the Ediff session.
    (message "No Ediff window configuration for the current frame")))

(defun prot-ediff-visible-buffers-2 ()
  "Run ediff on the buffers displayed in the current frame's two windows."
  (interactive)
  (if-let* ((windows (window-list))
            (_ (= (length windows) 2))
            (buffers (mapcar #'window-buffer windows)))
      (pcase-let ((`(,first ,second) buffers))
        (ediff-buffers first second))
    (user-error "Can only operate on two windows")))

(defun prot-ediff-visible-buffers-3 ()
  "Run ediff on the buffers displayed in the current frame's three windows."
  (interactive)
  (if-let* ((windows (window-list))
            (_ (= (length windows) 3))
            (buffers (mapcar #'window-buffer windows)))
      (pcase-let ((`(,first ,second ,third) buffers))
        (ediff-buffers3 first second third))
    (user-error "Can only operate on three windows")))

(provide 'prot-ediff)
;;; prot-ediff.el ends here
