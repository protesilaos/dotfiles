;;; prot-bookmark.el --- Bookmark extras for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022  Protesilaos Stavrou

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos.com/emacs/dotemacs
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
;; Bookmark extras for my Emacs: <https://protesilaos.com/emacs/dotemacs/>.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

(require 'prot-common)

(defgroup prot-bookmark ()
  "Bookmark extras for my dotemacs."
  :group 'matching)

;;;; Extend Bookmark menu font-lock

(defface prot-bookmark-url
  '((((class color) (min-colors 88) (background light))
     :foreground "#0000c0")
    (((class color) (min-colors 88) (background dark))
     :foreground "#00bcff")
    (t :foreground "blue"))
  "Face for URL bookmarks.")

(defface prot-bookmark-pdf
  '((((class color) (min-colors 88) (background light))
     :foreground "#7f1010")
    (((class color) (min-colors 88) (background dark))
     :foreground "#ffa0a0")
    (t :foreground "red"))
  "Face for PDF bookmarks.")

(defface prot-bookmark-directory
  '((((class color) (min-colors 88) (background light))
     :foreground "#0f3d8c")
    (((class color) (min-colors 88) (background dark))
     :foreground "#a0acef")
    (t :foreground "cyan"))
  "Face for directory bookmarks.")

;; TODO 2021-09-08: We should be able to filter out bookmarks from the
;; likes of Info and VC-Dir which set a file path even though they are
;; not really intended to be visited as files.
(defconst prot-bookmark-keywords
  `((,(concat "\\(.*\\)" " " prot-common-url-regexp)
     (1 '(bold prot-bookmark-url) t)
     (2 'prot-bookmark-url t))
    ("\\(.*\\)\\( [~/].*\\.pdf\\)"
     (1 '(bold prot-bookmark-pdf) t)
     (2 'prot-bookmark-pdf t))
    ("\\(.*\\)\\( [~/].*/$\\)"
     (1 '(bold prot-bookmark-directory) t)
     (2 'prot-bookmark-directory t))
    ("\\(.*org.*last-stored.*\\)"
     (1 'shadow t)))
  "Extra font-lock patterns for the Bookmark menu.")

;;;###autoload
(define-minor-mode prot-bookmark-extra-keywords
  "Apply extra font-lock rules to bookmark list buffers."
  :init-value nil
  :global t
  (if prot-bookmark-extra-keywords
      (progn
        (font-lock-flush (point-min) (point-max))
        (font-lock-add-keywords nil prot-bookmark-keywords nil)
        (add-hook 'bookmark-bmenu-mode-hook #'prot-bookmark-extra-keywords))
    (font-lock-remove-keywords nil prot-bookmark-keywords)
    (remove-hook 'bookmark-bmenu-mode-hook #'prot-bookmark-extra-keywords)
    (font-lock-flush (point-min) (point-max))))

(provide 'prot-bookmark)
;;; prot-bookmark.el ends here
