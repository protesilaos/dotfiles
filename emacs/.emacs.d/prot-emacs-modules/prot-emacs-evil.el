(prot-emacs-configure
  (:delay 1)

;;;; General settings

  (setq evil-want-C-u-delete nil)
  (setq evil-want-C-w-delete nil)
  (setq evil-want-C-h-delete nil)
  (setq evil-want-C-w-in-emacs-state nil)
  (setq evil-want-abbrev-expand-on-insert-exit nil)
  (setq evil-disable-insert-state-bindings t)

  (setq evil-want-change-word-to-end t)
  (setq evil-want-C-i-jump t)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-d-scroll t)
  (setq evil-want-C-g-bindings nil)
  (setq evil-want-Y-yank-to-eol t) ; consistent with D
  (setq evil-want-empty-ex-last-command t)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-minibuffer nil)

  (setq evil-toggle-key "<f12>") ; I seldom need this, so putting it somewhere far
  (setq evil-default-state 'normal) ; check `evil-set-initial-state'
  (setq evil-echo-state nil)
  (setq evil-mode-line-format nil) ; see `prot-modeline.el' and its implementation in `prot-emacs-modeline.el'
  (setq evil-want-fine-undo t)
  (setq evil-esc-delay 0.01)
  (setq evil-intercept-esc 'always)
  (setq evil-highlight-closing-paren-at-point-states '(not emacs insert replace))
  (setq evil-kill-on-visual-paste nil) ; Does not work, see `prot-evil-visual-paste-no-kill'
  (setq evil-auto-indent t)

  (setq evil-undo-system 'undo-redo) ; Emacs 28
  (setq evil-visual-update-x-selection-p t)

  (setq evil-repeat-move-cursor t)
  (setq evil-repeat-find-to-skip-next t)
  (setq evil-kbd-macro-suppress-motion-error nil) ; never suppress errors in kmacros, Emacs-style

  ;; ;; I disable K anyway
  ;; (setq evil-lookup-func #'ignore)

  ;; I only use the standard commands for window splitting...
  (setq evil-auto-balance-windows window-combination-resize)
  (setq evil-split-window-below nil)
  (setq evil-vsplit-window-right nil)

  (setq evil-mouse-word 'evil-word)
  (setq evil-bigword "^ \t\r\n")

  ;; evil-complete-all-buffer
  ;; evil-buffer-regexps
  ;; evil-intercept-maps
  ;; evil-motions
  ;; evil-visual-newline-commands
  ;; evil-text-object-change-visual-type
  ;; evil-command-window-height
  ;; evil-display-shell-error-in-message

;;;; Evil search setup

  ;; NOTE 2023-11-06: These settings do not give me the same isearch
  ;; experience as core Emacs.  I only want that, but to use it with
  ;; Vim keys.

  (setq evil-search-module 'isearch)
  (setq evil-symbol-word-search t)
  (setq evil-flash-delay 0.5)
  (setq evil-ex-hl-update-delay lazy-highlight-initial-delay)
  (setq evil-regexp-search nil)
  (setq evil-search-wrap t)
  (setq evil-search-wrap-ring-bell t) ; TODO does this quit keyboard macros, specifically with C-u 0?
  (setq evil-magic nil) ; not applicable for `evil-search-module' isearch module
  (setq evil-ex-search-vim-style-regexp nil) ; not for isearch

  ;; NOTE 2023-11-06: I have not tested all of the following and am
  ;; not sure which ones are relevant for my use-case.

;;;; Ex commands

  ;; NOTE 2023-11-06: I have not used any of these...
  (setq evil-ex-interactive-search-highlight 'selected-window)
  (setq evil-ex-search-persistent-highlight t)
  (setq evil-ex-search-case 'smart)
  (setq evil-ex-substitute-case nil) ; use `evil-ex-search-case'
  (setq evil-ex-search-incremental t)
  (setq evil-ex-search-highlight-all t)
  (setq evil-ex-substitute-highlight-all t)
  (setq evil-ex-substitute-interactive-replace t)
  (setq evil-ex-substitute-global nil)
  ;; evil-ex-complete-emacs-commands 'in-turn
  ;; evil-ex-visual-char-range nil

;;;; Evil cursors

  ;; TODO 2023-11-06: integrate with `cursory'
  (setq evil-default-cursor t)

;;;; How lines are treated

  (setq evil-respect-visual-line-mode nil)

  (setq evil-shift-width tab-width)
  (setq evil-start-of-line t)
  (setq evil-backspace-join-lines t)
  (setq evil-cross-lines t)
  (setq evil-move-cursor-back nil) ; my "insert" mode is "emacs", so not relevant
  (setq evil-move-beyond-eol nil)
  (setq evil-show-paren-range 0)
  (setq evil-track-eol t)
  (setq evil-v$-excludes-newline nil)

;;;; Compatibility with other modes

  ;; I load the `evil' feature here because some functions/macros of
  ;; it are needed from this point on.  Evil is one of those few
  ;; packages that requires certain variables to be evaluated before
  ;; it is `require'd.  This is why I place most of the `setq' calls
  ;; above.
  (prot-emacs-package evil (:install t))

  ;; The `prot-evil' library defines a new state for better
  ;; compatibility with most (all?) major modes.  It also introduces a
  ;; new "erase" operator.  These are configured from this point on.
  (prot-emacs-package prot-evil)

  (setq evil-overriding-maps nil)
  (setq evil-motion-state-modes nil)
  (setq evil-insert-state-modes nil)
  (setq evil-emacs-state-modes
        '(comint-mode
          rcirc-mode
          eshell-mode
          inferior-emacs-lisp-mode
          reb-mode
          shell-mode
          term-mode
          wdired-mode
          log-edit-mode
          git-commit-mode))
  (setq evil-prot-basic-state-modes
        '(completion-list-mode
          Buffer-menu-mode
          Custom-mode
          edebug-mode
          Info-mode
          help-mode
          diff-mode
          ediff-mode
          log-view-mode
          org-agenda-mode
          dired-mode
          magit-status-mode
          magit-diff-mode
          magit-log-mode
          notmuch-hello-mode
          notmuch-search-mode
          notmuch-show-mode
          notmuch-tree-mode
          special-mode
          tabulated-list-mode
          world-clock-mode))

  (with-eval-after-load 'info
    (evil-define-key 'prot-basic Info-mode-map (kbd "L") #'Info-history-back))

  (with-eval-after-load 'help-mode
    (evil-define-key 'prot-basic help-mode-map (kbd "L") #'help-go-back))

  (with-eval-after-load 'dired
    (evil-define-key 'prot-basic dired-mode-map (kbd "J") #'dired-goto-file)
    (evil-define-key 'prot-basic dired-mode-map (kbd "K") #'dired-do-kill-lines))

  (with-eval-after-load 'notmuch
    (evil-define-key 'prot-basic notmuch-hello-mode-map (kbd "J") #'notmuch-jump-search)
    (evil-define-key 'prot-basic notmuch-show-mode-map (kbd "J") #'notmuch-jump-search)
    (evil-define-key 'prot-basic notmuch-search-mode-map (kbd "J") #'notmuch-jump-search)
    (evil-define-key 'prot-basic notmuch-tree-mode-map (kbd "J") #'notmuch-jump-search)
    (evil-define-key 'prot-basic notmuch-show-mode-map (kbd "K") #'notmuch-tag-jump)
    (evil-define-key 'prot-basic notmuch-search-mode-map (kbd "K") #'notmuch-tag-jump)
    (evil-define-key 'prot-basic notmuch-tree-mode-map (kbd "K") #'notmuch-tag-jump))

  ;; See TODO above about the visual state.
  (with-eval-after-load 'magit
    (evil-define-key '(visual prot-basic) magit-status-mode-map (kbd "K") #'magit-discard)
    (evil-define-key 'prot-basic magit-status-mode-map (kbd "l") #'magit-log)
    (evil-define-key 'visual magit-status-mode-map (kbd "s") #'magit-stage)
    (evil-define-key 'visual magit-status-mode-map (kbd "u") #'magit-unstage))

  (with-eval-after-load 'embark
    (defun prot-evil-embark-act-or-repeat-pop ()
      "Call `evil-repeat-pop' if relevant, else `prot/embark-act-no-quit'."
      (interactive)
      (call-interactively
       (if (memq last-command '(evil-repeat evil-repeat-pop evil-repeat-pop-next))
           #'evil-repeat-pop
         #'prot/embark-act-no-quit)))

    (evil-define-key '(normal visual motion) global-map (kbd "C-.") #'prot-evil-embark-act-or-repeat-pop))

  (with-eval-after-load 'org
    (evil-define-key '(normal visual motion) org-mode-map
      (kbd "<tab>") #'org-cycle
      (kbd "<return>") #'org-ctrl-c-ctrl-c))

;;;; Make Emacs the Insert state

  (defalias 'evil-insert-state 'evil-emacs-state)
  (evil-define-key 'emacs global-map (kbd "<escape>") #'prot-evil-normal-or-basic-state)
  (setq evil-emacs-state-cursor evil-insert-state-cursor)

;;;; Custom Evil keys

  ;; The `prot-evil' library defines an "erase" operator.

  (evil-define-key '(normal visual motion) global-map
    (kbd "U") #'evil-redo
    (kbd "C-r") #'isearch-backward
    (kbd "K") #'prot-evil-erase
    (kbd "g d") #'xref-find-definitions
    (kbd "g D") #'xref-go-back
    (kbd "<") #'prot-evil-shift-left
    (kbd ">") #'prot-evil-shift-right)

;;;; Set up `devil-mode' to reduce modifier key usage

  (prot-emacs-package devil (:install t))

  (evil-define-key '(normal visual motion prot-basic) global-map (kbd ",") #'devil)

  ;; This one affects the emacs/insert state.
  (global-devil-mode 1)

;;;; Set up my prefix keymap

  (evil-define-key '(emacs insert) global-map (kbd "SPC") #'prot-evil-prefix-or-self-insert)
  (evil-define-key '(normal motion prot-basic) global-map (kbd "SPC") prot-prefix-map)

;;;; Activate `evil-mode'
  (evil-mode 1))

(provide 'prot-emacs-evil)
