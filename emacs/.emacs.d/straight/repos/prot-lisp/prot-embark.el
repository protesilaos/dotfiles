;;; prot-embark.el --- Extensions to embark.el for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2020-2021  Protesilaos Stavrou

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos.com/dotemacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))

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

(defcustom prot-embark-collect-window-regexp
  "\\*Embark Collect \\(Live\\|Completions\\).*"
  "Regexp to match window names with Embark collections."
  :group 'prot-embark
  :type 'string)

;; Thanks to Omar Antolín Camarena for providing a variant of this!
;; (mistakes are always my own).
(defun prot-embark--collect-fit-window (&rest _)
  "Fit Embark's live occur window to its buffer.
To be added to `embark-occur-post-revert-hook'."
  (when (string-match-p prot-embark-collect-window-regexp (buffer-name))
    (fit-window-to-buffer (get-buffer-window)
                          (floor (frame-height) 2) 1)))

(defvar embark-collect-linked-buffer)

(defun prot-embark--live-buffer-p ()
  "Determine presence of a linked live occur buffer."
  (let* ((buf-link embark-collect-linked-buffer)
         (buf-name (buffer-name buf-link)))
    (when buf-name
      (string-match-p prot-embark-collect-window-regexp buf-name))))

(declare-function embark-collect-completions "embark")

;;;###autoload
(defun prot-embark-completions-toggle ()
  "Toggle `embark-collect-completions'."
  (interactive)
  (if (prot-embark--live-buffer-p)
      (kill-buffer embark-collect-linked-buffer)
    (embark-collect-completions)))

;; ;; FIXME: we need a more robust approach than `this-command'.
;;
;; (declare-function embark-collect-completions-after-delay "embark")
;; (declare-function embark-collect-completions-after-input "embark")
;;
;; ;; TODO: consider pros and cons of a blocklist.
;; (defcustom prot-embark-completions-passlist nil
;;   "Command list that can display Embark Collect automatically.
;; Those are run with `embark-collect-completions-after-delay',
;; otherwise we revert to `embark-collect-completions-after-input'.
;;
;; This list is intended for `prot-embark-collect-completions'."
;;   :type 'list
;;   :group 'prot-embark)
;;
;; ;;;###autoload
;; (defun prot-embark-collect-completions ()
;;   "Control how to display Embark Collection Completions.
;;
;; If `this-command' belongs to `prot-embark-completions-passlist',
;; the completions' buffer is automatically displayed following a
;; delay.  Otherwise it only shows up after some minibuffer input.
;;
;; Use this function with `minibuffer-setup-hook'."
;;   (if (member this-command prot-embark-completions-passlist)
;;       (embark-collect-completions-after-delay)
;;     (embark-collect-completions-after-input)))

(provide 'prot-embark)
;;; prot-embark.el ends here
