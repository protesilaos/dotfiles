;;; prot-prefix.el --- Prefix keymap for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2023-2025  Protesilaos Stavrou

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

;;; Code:

(declare-function prot-simple-kill-buffer-current "prot-simple" (&optional arg))
(declare-function prot-simple-rename-file-and-buffer "prot-simple" (name))
(declare-function prot-simple-buffers-major-mode "prot-simple")
(declare-function prot-simple-buffers-vc-root "prot-simple")
(declare-function beframe-buffer-menu "beframe" (&optional frame &key sort))

(defvar-keymap prot-prefix-buffer-map
  :doc "Prefix keymap for buffers."
  :name "Buffer"
  :prefix 'prot-prefix-buffer
  "m" #'beframe-buffer-menu
  "b" #'switch-to-buffer
  "B" #'prot-simple-buffers-major-mode
  "c" #'clone-indirect-buffer-other-window
  "f" #'fit-window-to-buffer
  "k" #'prot-simple-kill-buffer-current
  "g" #'revert-buffer-quick
  "r" #'prot-simple-rename-file-and-buffer
  "n" #'next-buffer
  "p" #'previous-buffer
  "v" #'prot-simple-buffers-vc-root)

(defvar-keymap prot-prefix-file-map
  :doc "Prefix keymaps for files."
  :name "File"
  :prefix 'prot-prefix-file
  "f" #'find-file
  "F" #'find-file-other-window
  "b" #'bookmark-jump
  "d" #'dired
  "l" #'find-library
  "m" #'man)

(defvar-keymap prot-prefix-insert-map
  :doc "Prefix keymap for character insertion."
  :name "Insert"
  :prefix 'prot-prefix-insert
  "i" #'insert-char
  "e" #'emoji-search
  "q" #'quoted-insert
  "s" #'emoji-search
  "l" #'emoji-list)

(declare-function logos-focus-mode "logos")
(declare-function keycast-mode-line-mode "keycast")
(declare-function rainbow-mode "rainbow")
(declare-function spacious-padding-mode "spacious-padding")

(defvar-keymap prot-prefix-mode-map
  :doc "Prefix keymap for minor mode toggles."
  :name "Toggle"
  :prefix 'prot-prefix-mode
  "f" #'flymake-mode
  "h" #'hl-line-mode
  "k" #'keycast-mode-line-mode
  "l" #'logos-focus-mode
  "m" #'menu-bar-mode
  "n" #'display-line-numbers-mode
  "t" #'toggle-truncate-lines
  "s" #'spacious-padding-mode
  "r" #'rainbow-mode
  "v" #'variable-pitch-mode)

(defvar-keymap prot-prefix-window-map
  :doc "Prefix keymap for windows."
  :name "Window"
  :prefix 'prot-prefix-window
  "u" #'winner-undo
  "r" #'winner-redo
  "b" #'balance-windows-area
  "d" #'toggle-window-dedicated
  "0" #'delete-window
  "1" #'delete-other-windows
  "!" #'delete-other-windows-vertically
  "2" #'split-window-below
  "@" #'split-root-window-below
  "3" #'split-window-right
  "#" #'split-root-window-right
  "o" #'other-window
  "^" #'tear-off-window
  "h" #'windmove-left
  "j" #'windmove-down
  "k" #'windmove-up
  "l" #'windmove-right
  "H" #'windmove-swap-states-left
  "J" #'windmove-swap-states-down
  "K" #'windmove-swap-states-up
  "L" #'windmove-swap-states-right)

(declare-function consult-find "consult" (&optional dir initial))
(declare-function consult-ripgrep "consult" (&optional dir initial))
(declare-function prot-search-grep "prot-search" (regexp &optional recursive))
(declare-function prot-search-grep-todo-keywords "prot-search" (&optional arg))
(declare-function prot-search-occur-browse-url "prot-search")
(declare-function prot-search-occur-outline "prot-search" (&optional arg))
(declare-function prot-simple-flush-and-diff "prot-simple" (regexp beg end))

(defvar-keymap prot-prefix-search-map
  :doc "Prefix keymap for search (and replace) commands."
  :name "Search"
  :prefix 'prot-prefix-search
  "f" #'consult-find
  "d" #'prot-simple-flush-and-diff
  "g" #'prot-search-grep
  "o" #'prot-search-occur-outline
  "r" #'consult-ripgrep
  "t" #'prot-search-grep-todo-keywords
  "u" #'prot-search-occur-browse-url)

(declare-function prot-simple-transpose-chars "prot-simple")
(declare-function prot-simple-transpose-lines "prot-simple" (arg))
(declare-function prot-simple-transpose-paragraphs "prot-simple" (arg))
(declare-function prot-simple-transpose-sentences "prot-simple" (arg))
(declare-function prot-simple-transpose-words "prot-simple" (arg))
(declare-function prot-simple-transpose-sexps "prot-simple" (arg))

(defvar-keymap prot-prefix-transpose-map
  :doc "Prefix keymap for object transposition."
  :name "Transpose"
  :prefix 'prot-prefix-transpose
  "c" #'prot-simple-transpose-chars
  "l" #'prot-simple-transpose-lines
  "p" #'prot-simple-transpose-paragraphs
  "s" #'prot-simple-transpose-sentences
  "w" #'prot-simple-transpose-words
  "x" #'prot-simple-transpose-sexps)

(defvar-keymap prot-prefix-expression-map
  :doc "Prefix keymap for s-expression motions."
  :name "S-EXP"
  :prefix 'prot-prefix-expression
  "a" #'beginning-of-defun
  "e" #'end-of-defun
  "f" #'forward-sexp
  "b" #'backward-sexp
  "n" #'forward-list
  "p" #'backward-list
  "d" #'up-list ; confusing name for what looks "out and down" to me
  "t" #'transpose-sexps
  "u" #'backward-up-list ; the actual "up"
  "k" #'kill-sexp
  "DEL" #'backward-kill-sexp)

(declare-function winner-undo "winner")
(declare-function winner-redo "winner")
(declare-function magit-status "magit" (&optional directory cache))
(declare-function prot-simple-other-windor-or-frame "prot-simple")

;; NOTE 2024-02-17: Some cons cells here have a symbol as a `cdr' and
;; some do not.  The former are those which define a prefix command
;; (per `define-prefix-command').  This is a symbol that references
;; the keymaps, thus making our binding an indirection: if we update
;; the key map, we automatically get the new key bindings.  Whereas
;; when we bind a key to the value of a variable, we have to update
;; the key map and then the binding for changes to propagate.
(defvar-keymap prot-prefix-map
  :doc "Prefix keymap with multiple subkeymaps."
  :name "Prot Prefix"
  :prefix 'prot-prefix
  "0" #'delete-window
  "1" #'delete-other-windows
  "!" #'delete-other-windows-vertically
  "^" #'tear-off-window
  "2" #'split-window-below
  "@" #'split-root-window-below
  "3" #'split-window-right
  "#" #'split-root-window-right
  "o" #'other-window
  "O" #'prot-simple-other-windor-or-frame
  "Q" #'save-buffers-kill-emacs
  "b" (cons "Buffer" 'prot-prefix-buffer)
  "c" #'world-clock
  "f" (cons "File" 'prot-prefix-file)
  "g" #'magit-status
  "h" (cons "Help" help-map)
  "i" (cons "Insert" 'prot-prefix-insert)
  "j" #'dired-jump
  "m" (cons "Minor modes" 'prot-prefix-mode)
  "n" (cons "Narrow" narrow-map)
  "p" (cons "Project" project-prefix-map)
  "r" (cons "Rect/Registers" ctl-x-r-map)
  "s" (cons "Search" 'prot-prefix-search)
  "t" (cons "Transpose" 'prot-prefix-transpose)
  "u" #'universal-argument
  "v" (cons "Version Control" 'vc-prefix-map)
  "w" (cons "Window" 'prot-prefix-window)
  "x" (cons "S-EXP" 'prot-prefix-expression))

;; ;; NOTE 2024-02-17: This is not needed anymore, because I bind a cons
;; ;; cell to the key.  The `car' of it is the description, which
;; ;; `which-key-mode' understands.
;;
;; (with-eval-after-load 'which-key
;;   (which-key-add-keymap-based-replacements prot-prefix-map
;;     "b" `("Buffer" . ,prot-prefix-buffer-map)
;;     "f" `("File" . ,prot-prefix-file-map)
;;     "h" `("Help" . ,help-map)
;;     "i" `("Insert" . ,prot-prefix-insert-map)
;;     "m" `("Mode" . ,prot-prefix-mode-map)
;;     "n" `("Narrow" . ,narrow-map)
;;     "p" `("Project" . ,project-prefix-map)
;;     "r" `("C-x r" . ,ctl-x-r-map)
;;     "s" `("Search" . ,prot-prefix-search-map)
;;     "t" `("Transpose" . ,prot-prefix-transpose-map)
;;     "v" `("C-x v" . ,vc-prefix-map)
;;     "w" `("Window" . ,prot-prefix-window-map)
;;     "x" `("S-EXP" . ,prot-prefix-expression-map)))

;; What follows is an older experiment with transient.  I like its
;; visuals, though find it hard to extend.  Keymaps are easier for me,
;; as I can add commands to one of the subkeymaps and they are readily
;; available without evaluating anything else.  Probably transient can
;; do this, though it is not obvious to me as to how.

;; (require 'transient)
;;
;; (transient-define-prefix prot-prefix-file nil
;;   "Transient with file commands."
;;   [["File or directory"
;;     ("f" "find-file" find-file)
;;     ("F" "find-file-other-window" find-file-other-window)]
;;    ["Directory only"
;;     ("d" "dired" dired)
;;     ("D" "dired-other-window" dired-other-window)]
;;    ["Documentation"
;;     ("l" "find-library" find-library)
;;     ("m" "man" man)]])
;;
;; (transient-define-prefix prot-prefix-buffer nil
;;   "Transient with buffer commands."
;;   [["Switch"
;;     ("b" "switch buffer" switch-to-buffer)
;;     ("B" "switch buf other window" switch-to-buffer-other-window)
;;     ("n" "next-buffer" next-buffer)
;;     ("p" "previous-buffer" previous-buffer)
;;     ("m" "buffer-menu" buffer-menu)
;;     ("q" "bury-buffer" bury-buffer)]
;;    ["Persist"
;;     ("c" "clone buffer" clone-indirect-buffer)
;;     ("C" "clone buf other window" clone-indirect-buffer-other-window)
;;     ("r" "rename-buffer" rename-buffer)
;;     ("R" "rename-uniquely" rename-uniquely)
;;     ("s" "save-buffer" save-buffer)
;;     ("w" "write-file" write-file)]
;;    ["Destroy"
;;     ("k" "kill-current-buffer" kill-current-buffer)
;;     ("K" "kill-buffer-and-window" kill-buffer-and-window)
;;     ("r" "revert-buffer" revert-buffer)]])
;;
;; (transient-define-prefix prot-prefix-search nil
;;   "Transient with search commands."
;;   [["Search"
;;     ("s" "isearch-forward" isearch-forward)
;;     ("S" "isearch-forward-regexp" isearch-forward-regexp)
;;     ("r" "isearch-backward" isearch-backward)
;;     ("R" "isearch-backward-regexp" isearch-backward-regexp)
;;     ("o" "occur" occur)]
;;    ["Edit"
;;     ("f" "flush-lines" flush-lines)
;;     ("k" "keep-lines" keep-lines)
;;     ("q" "query-replace" query-replace)
;;     ("Q" "query-replace-regexp" query-replace-regexp)]])
;;
;; (transient-define-prefix prot-prefix-window nil
;;   "Transient with window commands."
;;   [["Manage"
;;     ("b" "balance-windows" balance-windows)
;;     ("f" "fit-window-to-buffer" fit-window-to-buffer)
;;     ("t" "tear-off-window" tear-off-window)]
;;    ["Popup"
;;     ("c" "calc" calc)
;;     ("f" "list-faces-display" list-faces-display)
;;     ("r" "re-builder" re-builder)
;;     ("w" "world-clock" world-clock)]])
;;
;; ;; This is independent of the transient, though still useful.
;; (defvar-keymap prot-prefix-repeat-map
;;   :doc "Global prefix map for repeatable keybindings (per `repeat-mode')."
;;   :name "Repeat"
;;   :repeat t
;;   "n" #'next-buffer
;;   "p" #'previous-buffer
;;   "<down>" #'enlarge-window
;;   "<right>" #'enlarge-window-horizontally
;;   "<up>" #'shrink-window
;;   "<left>" #'shrink-window-horizontally)
;;
;; (transient-define-prefix prot-prefix-toggle nil
;;   "Transient with minor mode toggles."
;;   [["Interface"
;;     ("c" "context-menu-mode" context-menu-mode)
;;     ("m" "menu-bar-mode" menu-bar-mode)
;;     ("s" "scroll-bar-mode" scroll-bar-mode)
;;     ("C-t" "tool-bar-mode" tool-bar-mode)]
;;    ["Tools"
;;     ("d" "toggle-debug-on-error" toggle-debug-on-error)
;;     ("f" "follow-mode" follow-mode)
;;     ("l" "visual-line-mode" visual-line-mode)
;;     ("v" "variable-pitch-mode" variable-pitch-mode)
;;     ("t" "toggle-truncate-lines" toggle-truncate-lines)
;;     ("C-s" "window-toggle-side-windows" window-toggle-side-windows)]])
;;
;; (transient-define-prefix prot-prefix nil
;;   "Transient with common commands.
;; Commands that bring up transients have ... in their description."
;;   [["Common"
;;     ("b" "Buffer..." prot-prefix-buffer)
;;     ("f" "File..." prot-prefix-file)
;;     ("s" "Search..." prot-prefix-search)
;;     ("w" "Window..." prot-prefix-window)
;;     ("t" "Toggle..." prot-prefix-toggle)]
;;    ["Resize"
;;     ("   <up>" "Shrink vertically" shrink-window)
;;     (" <down>" "Enlarge vertically" enlarge-window)
;;     (" <left>" "Shrink horizontally" shrink-window-horizontally)
;;     ("<right>" "Enlarge horizontally" enlarge-window-horizontally)]
;;    ["Misc"
;;     ("e" "Emoji transient..." emoji-insert)
;;     ("E" "Emoji search" emoji-search)
;;     ("C-e" "Emoji buffer" emoji-list)
;;     ("RET" "Insert unicode" insert-char)
;;     ("\\" "toggle-input-method" toggle-input-method)]])

(provide 'prot-prefix)
;;; prot-prefix.el ends here
