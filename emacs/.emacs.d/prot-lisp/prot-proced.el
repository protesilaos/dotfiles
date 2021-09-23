;;; prot-proced.el --- Extras for proced -*- lexical-binding: t -*-

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
;; Extras for `proced', intended for use in my Emacs setup:
;; <https://protesilaos.com/dotemacs/>.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

(defgroup prot-proced ()
  "Proced extras for my dotemacs."
  :group 'proced)

;;;; Extend `proced' faces

(defface prot-proced-user '((t :inherit shadow))
  "Face for user indicator in `proced'.")

(defface prot-proced-pid
  '((((class color) (min-colors 88) (background light))
     :foreground "#5317ac")
    (((class color) (min-colors 88) (background dark))
     :foreground "#b6a0ff"))
  "Face for PID indicator in `proced'.")

(defface prot-proced-cpu
  '((((class color) (min-colors 88) (background light))
     :foreground "#8f0075")
    (((class color) (min-colors 88) (background dark))
     :foreground "#f78fe7"))
  "Face for memory indicator in `proced'.")

(defface prot-proced-mem
  '((((class color) (min-colors 88) (background light))
     :foreground "#0031a9")
    (((class color) (min-colors 88) (background dark))
     :foreground "#2fafff"))
  "Face for CPU indicator in `proced'.")

(defface prot-proced-time-start
  '((((class color) (min-colors 88) (background light))
     :foreground "#30517f")
    (((class color) (min-colors 88) (background dark))
     :foreground "#a0bfdf"))
  "Face for start time indicator in `proced'.")

(defface prot-proced-time-duration
  '((((class color) (min-colors 88) (background light))
     :foreground "#00538b")
    (((class color) (min-colors 88) (background dark))
     :foreground "#00cdc8"))
  "Face for time indicator in `proced'.")

(defface prot-proced-process nil
  "Face for process indicator in `proced'.")

(defconst prot-proced-keywords
  `((,(concat "^\s+\\(.*?\\)\s+\\(.*?\\)\s+\\(.*?\\)\s+\\(.*?\\)\s+"
             "\\(.*?\\)\s+\\(.*?\\)\s+\\(.*\\)")
     (1 'prot-proced-user)
     (2 'prot-proced-pid)
     (3 'prot-proced-cpu)
     (4 'prot-proced-mem)
     (5 'prot-proced-time-start)
     (6 'prot-proced-time-duration)
     (7 'prot-proced-process)))
  "Extra font-lock patterns for the `proced' menu.")

;;;###autoload
(define-minor-mode prot-proced-extra-keywords
  "Apply extra font-lock rules to diff buffers."
  :init-value nil
  :global t
  (if prot-proced-extra-keywords
      (progn
        (font-lock-flush (point-min) (point-max))
        (font-lock-add-keywords nil prot-proced-keywords nil)
        (add-hook 'proced-mode-hook #'prot-proced-extra-keywords))
    (font-lock-remove-keywords nil prot-proced-keywords)
    (remove-hook 'proced-mode-hook #'prot-proced-extra-keywords)
    (font-lock-flush (point-min) (point-max))))

(provide 'prot-proced)
;;; prot-proced.el ends here
