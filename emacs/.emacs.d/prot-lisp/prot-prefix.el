;;; prot-prefix.el --- Prefix keymap for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2023  Protesilaos Stavrou

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos.com/emacs/dotemacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))

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

(require 'transient)

(transient-define-prefix prot-prefix-file nil
  "Transient with file commands."
  [["File or directory"
    ("f" "find-file" find-file)
    ("F" "find-file-other-window" find-file-other-window)]
   ["Directory only"
    ("d" "dired" dired)
    ("D" "dired-other-window" dired-other-window)]
   ["Documentation"
    ("l" "find-library" find-library)
    ("m" "man" man)]])

(transient-define-prefix prot-prefix-buffer nil
  "Transient with buffer commands."
  [["Switch"
    ("b" "switch buffer" switch-to-buffer)
    ("B" "switch buf other window" switch-to-buffer-other-window)
    ("n" "next-buffer" next-buffer)
    ("p" "previous-buffer" previous-buffer)
    ("m" "buffer-menu" buffer-menu)
    ("q" "bury-buffer" bury-buffer)]
   ["Persist"
    ("c" "clone buffer" clone-indirect-buffer)
    ("C" "clone buf other window" clone-indirect-buffer-other-window)
    ("r" "rename-buffer" rename-buffer)
    ("R" "rename-uniquely" rename-uniquely)
    ("s" "save-buffer" save-buffer)
    ("w" "write-file" write-file)]
   ["Destroy"
    ("k" "kill-current-buffer" kill-current-buffer)
    ("K" "kill-buffer-and-window" kill-buffer-and-window)
    ("r" "revert-buffer" revert-buffer)]])

(transient-define-prefix prot-prefix-search nil
  "Transient with search commands."
  [["Search"
    ("s" "isearch-forward" isearch-forward)
    ("S" "isearch-forward-regexp" isearch-forward-regexp)
    ("r" "isearch-backward" isearch-backward)
    ("R" "isearch-backward-regexp" isearch-backward-regexp)
    ("o" "occur" occur)]
   ["Edit"
    ("f" "flush-lines" flush-lines)
    ("k" "keep-lines" keep-lines)
    ("q" "query-replace" query-replace)
    ("Q" "query-replace-regexp" query-replace-regexp)]])

(transient-define-prefix prot-prefix-window nil
  "Transient with window commands."
  [["Manage"
    ("b" "balance-windows" balance-windows)
    ("f" "fit-window-to-buffer" fit-window-to-buffer)
    ("t" "tear-off-window" tear-off-window)]
   ["Popup"
    ("c" "calc" calc)
    ("f" "list-faces-display" list-faces-display)
    ("r" "re-builder" re-builder)
    ("w" "world-clock" world-clock)]])

;; This is independent of the transient, though still useful.
(defvar-keymap prot-prefix-repeat-map
  :doc "Global prefix map for repeatable keybindings (per `repeat-mode')."
  :name "Repeat"
  :repeat t
  "n" #'next-buffer
  "p" #'previous-buffer
  "<down>" #'enlarge-window
  "<right>" #'enlarge-window-horizontally
  "<up>" #'shrink-window
  "<left>" #'shrink-window-horizontally)

(transient-define-prefix prot-prefix-toggle nil
  "Transient with minor mode toggles."
  [["Interface"
    ("c" "context-menu-mode" context-menu-mode)
    ("m" "menu-bar-mode" menu-bar-mode)
    ("s" "scroll-bar-mode" scroll-bar-mode)
    ("C-t" "tool-bar-mode" tool-bar-mode)]
   ["Tools"
    ("d" "toggle-debug-on-error" toggle-debug-on-error)
    ("f" "follow-mode" follow-mode)
    ("l" "visual-line-mode" visual-line-mode)
    ("v" "variable-pitch-mode" variable-pitch-mode)
    ("t" "toggle-truncate-lines" toggle-truncate-lines)
    ("C-s" "window-toggle-side-windows" window-toggle-side-windows)]])

(transient-define-prefix prot-prefix nil
  "Transient with common commands.
Commands that bring up transients have ... in their description."
  [["Common"
    ("b" "Buffer..." prot-prefix-buffer)
    ("f" "File..." prot-prefix-file)
    ("s" "Search..." prot-prefix-search)
    ("w" "Window..." prot-prefix-window)
    ("t" "Toggle..." prot-prefix-toggle)]
   ["Resize"
    ("   <up>" "Shrink vertically" shrink-window)
    (" <down>" "Enlarge vertically" enlarge-window)
    (" <left>" "Shrink horizontally" shrink-window-horizontally)
    ("<right>" "Enlarge horizontally" enlarge-window-horizontally)]
   ["Misc"
    ("e" "Emoji transient..." emoji-insert)
    ("E" "Emoji search" emoji-search)
    ("C-e" "Emoji buffer" emoji-list)
    ("RET" "Insert unicode" insert-char)
    ("\\" "toggle-input-method" toggle-input-method)]])

(provide 'prot-prefix)
;;; prot-prefix.el ends here
