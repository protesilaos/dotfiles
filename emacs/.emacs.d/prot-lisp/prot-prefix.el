;;; prot-prefix.el --- Prefix keymap for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2023  Protesilaos Stavrou

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos.com/emacs/dotemacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

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
;; Prefix keymap for my custom keymaps.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

(defvar-keymap prot-prefix-file-map
  :doc "Custom keymap with file-related commands.
Add this to `prot-prefix-map'."
  :name "File"
  "f" #'find-file
  "F" #'find-file-other-window
  "l" #'find-library
  "m" #'man
  "r" #'recentf-open
  "s" #'save-buffer
  "w" #'write-file
  "d" #'dired
  "D" #'dired-other-window)

(defvar-keymap prot-prefix-buffer-map
  :doc "Custom keymap with buffer-related commands.
Add this to `prot-prefix-map'."
  :name "Buffer"
  "b" #'switch-to-buffer
  "B" #'switch-to-buffer-other-window
  "c" #'clone-indirect-buffer
  "C" #'clone-indirect-buffer-other-window
  "k" #'kill-current-buffer
  "r" #'revert-buffer
  "R" #'rename-buffer
  "s" #'save-buffer
  "n" #'next-buffer
  "p" #'previous-buffer
  "m" #'buffer-menu
  "q" #'bury-buffer)

(defvar-keymap prot-prefix-search-map
  :doc "Custom keymap with various search (and replace) commands.
Add this to `prot-prefix-map'."
  :name "Search"
  "s" #'isearch-forward
  "S" #'isearch-forward-regexp
  "r" #'isearch-backward
  "R" #'isearch-backward-regexp
  "q" #'query-replace
  "Q" #'query-replace-regexp)

(defvar-keymap prot-prefix-kill-map
  :doc "Custom keymap with various kill/close commands.
Add this to `prot-prefix-map'."
  :name "Kill"
  "b" #'kill-this-buffer
  "w" #'delete-window
  "f" #'delete-frame
  "e" #'save-buffers-kill-emacs)

(defvar-keymap prot-prefix-window-map
  :doc "Custom keymap with various window commands.
Add this to `prot-prefix-map'."
  :name "Window"
  "s" #'split-window-below
  "v" #'split-window-right
  "k" #'delete-window
  "b" #'balance-windows
  "<down>" #'enlarge-window
  "<right>" #'enlarge-window-horizontally
  "<up>" #'shrink-window
  "<left>" #'shrink-window-horizontally)

(defvar-keymap prot-prefix-repeat-map
  :doc "Global prefix map for repeatable keybindings."
  :name "Repeat"
  :repeat t
  "n" #'next-buffer
  "p" #'previous-buffer
  "<down>" #'enlarge-window
  "<right>" #'enlarge-window-horizontally
  "<up>" #'shrink-window
  "<left>" #'shrink-window-horizontally)

(defvar-keymap prot-prefix-toggle-map
  :doc "Global prefix map for various toggles."
  :name "Toggle"
  "d" #'toggle-debug-on-error)

(defvar-keymap prot-prefix-map
  :doc "Global prefix map for my custom keymaps.
This map should be bound to a global prefix."
  :name "Prot Prefix"
  "b" prot-prefix-buffer-map
  "f" prot-prefix-file-map
  "k" prot-prefix-kill-map
  "r" prot-prefix-repeat-map
  "s" prot-prefix-search-map
  "t" prot-prefix-toggle-map
  "w" prot-prefix-window-map
  "<down>" #'enlarge-window
  "<right>" #'enlarge-window-horizontally
  "<up>" #'shrink-window
  "<left>" #'shrink-window-horizontally)

(provide 'prot-prefix)
;;; prot-prefix.el ends here
