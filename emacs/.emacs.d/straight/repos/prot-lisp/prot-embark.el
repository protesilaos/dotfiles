;;; prot-embark.el --- Extensions to embark.el for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2020-2021  Protesilaos Stavrou

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
;; Extensions to `embark.el' for my Emacs configuration:
;; <https://protesilaos.com/dotemacs/>.

;;; Code:

(require 'cl-lib)
(when (featurep 'embark)
  (require 'embark))

(defgroup prot-embark ()
  "Extensions for `embark'."
  :group 'editing)

;; Thanks to Omar Antolín Camarena for providing this!
(defun prot-embark--live-occur-fit-window (&rest _)
  "Fit Embark's live occur window to its buffer.
To be added to `embark-occur-post-revert-hook'."
  (when (string-match-p "Live" (buffer-name))
    (fit-window-to-buffer (get-buffer-window)
                          (floor (frame-height) 2) 1)))

(defun prot-embark--live-buffer-p ()
  "Determine presence of a linked live occur buffer."
  (let* ((buf-link embark-occur-linked-buffer)
         (buf-name (buffer-name buf-link)))
    (when buf-name
      (string-match-p "Embark Live Occur" buf-name))))

;;;###autoload
(defun prot-embark-live-occur-toggle ()
  "Toggle `embark-live-occur', call `prot-embark-live-occur-hook'."
  (interactive)
  (if (prot-embark--live-buffer-p)
      (kill-buffer embark-occur-linked-buffer)
    (embark-live-occur))
  (run-hooks 'prot-embark-live-occur-hook))

;;;; Icomplete integration

;; DEPRECATED: I just use Embark for my completion UI, but am keeping
;; this around in case I ever revisit Icomplete.
(defcustom prot-embark-live-occur-disable-icomplete nil
  "Whether `prot-embark-live-occur-toggle' should disable Icomplete."
  :group 'prot-embark
  :type 'boolean)

(declare-function icomplete-mode "icomplete")

(defvar prot-embark-live-occur-hook nil
  "Hook that runs after `prot-embark-live-occur-toggle'.")

(defun prot-embark--icomplete-toggle ()
  "Toggle Icomplete for `prot-embark-live-occur-toggle'."
  (let ((icomplete-default (symbol-value icomplete-mode)))
    (when prot-embark-live-occur-disable-icomplete
      (if (and icomplete-default (prot-embark--live-buffer-p))
          (icomplete-mode -1)
        (icomplete-mode 1)))))

;;;###autoload
(define-minor-mode prot-embark-icomplete-hooks-mode
  "Set up hooks for `prot-embark--icomplete-toggle'."
  :init-value nil
  :global t
  (if (and prot-embark-icomplete-hooks-mode
           prot-embark-live-occur-disable-icomplete)
      (progn
        (add-hook 'prot-embark-live-occur-hook #'prot-embark--icomplete-toggle)
        (add-hook 'minibuffer-exit-hook #'prot-embark--icomplete-toggle))
    (remove-hook 'prot-embark-live-occur-hook #'prot-embark--icomplete-toggle)
    (remove-hook 'minibuffer-exit-hook #'prot-embark--icomplete-toggle)))

(provide 'prot-embark)
;;; prot-embark.el ends here
