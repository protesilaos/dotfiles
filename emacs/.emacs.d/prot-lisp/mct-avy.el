;;; mct-avy.el --- MCT integration with Avy -*- lexical-binding: t -*-

;; Copyright (C) 2022  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://gitlab.com/protesilaos/mct
;; Version: 0.5.0
;; Package-Requires: ((emacs "27.1") (mct "0.5"))

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
;; MCT extension which provides the means to select a completion
;; candidate with the `avy' package's mode of interaction.
;;
;; Inspired by Omar Antolín Camarena's `avy-embark-collect.el'.  The
;; `mct-avy-choose' function borrows code or ideas from that library:
;; <https://github.com/oantolin/embark/>.

;;; Code:

;;;; General utilities

(require 'mct)

(declare-function avy-process "avy" (candidates &optional overlay-fn cleanup-fn))

(defun mct-avy-choose (&optional fn)
  "Use Avy to go to completion candidate and optionally run FN."
  (cond
   ((mct--get-completion-window)
    (mct-switch-to-completions-top)
    (goto-char
     (avy-process
      (save-excursion
        (let (completions)
          (goto-char (mct--last-completion-point))
          (while (not (bobp))
            (push (point) completions)
            (previous-completion 1))
          completions))))
    (when fn (funcall fn)))
   (t (user-error "No Completions' buffer available"))))

;;;###autoload
(defun mct-avy-choose-completion-exit ()
  "Use Avy to run `mct-choose-completion-exit' on candidate."
  (interactive)
  (mct-avy-choose #'mct-choose-completion-exit))

;;;###autoload
(defun mct-avy-choose-completion-dwim ()
  "Use Avy to run `mct-choose-completion-dwim' on candidate."
  (interactive)
  (mct-avy-choose #'mct-choose-completion-dwim))

;;;###autoload
(defun mct-avy-choose-completion-jump ()
  "Use Avy to jump to the selected candidate."
  (interactive)
  (mct-avy-choose))

;;;###autoload
(defun mct-avy-region-choose-completion ()
  "Use Avy to run `choose-completion' on candidate.
Intended for use with the MCT region mode for in-buffer
completion where the minibuffer is not active."
  (interactive)
  (mct-avy-choose #'choose-completion))

(declare-function embark-act "embark" (&optional arg))

;;;###autoload
(defun mct-avy-embark-act ()
  "Use Avy to run `embark-act' on candidate.
Requires the `embark' package."
  (interactive)
  (if (require 'embark nil t)
      (mct-avy-choose #'embark-act)
    (user-error "The `embark' package has not been loaded")))

(provide 'mct-avy)
;;; mct-avy.el ends here
