(prot-emacs-configure
  (setq tab-bar-new-button-show nil)
  (setq tab-bar-close-button-show nil)
  (setq tab-bar-show 1)

  (advice-add #'window--delete :after #'prot-common-clear-minibuffer-message))

;;; General window and buffer configurations
(prot-emacs-configure
;;;; `uniquify' (unique names for buffers)
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-strip-common-suffix t)
  (setq uniquify-after-kill-buffer-p t))

;;;; Line highlight
(prot-emacs-configure
  (setq hl-line-sticky-flag nil)
  (setq hl-line-overlay-priority -50)) ; emacs28

;;;; Negative space highlight
(prot-emacs-configure
  (setq whitespace-style
        '(face
          tabs
          spaces
          newline
          tab-mark
          space-mark
          newline-mark
          trailing
          missing-newline-at-eof
          space-after-tab::tab
          space-after-tab::space
          space-before-tab::tab
          space-before-tab::space))

  (prot-emacs-keybind global-map
    "<f6>" #'whitespace-mode
    "C-c z" #'delete-trailing-whitespace))

;;; Line numbers on the side of the window
(prot-emacs-configure
  (define-key global-map (kbd "<f7>") #'display-line-numbers-mode)
  (setq display-line-numbers-type t)
  ;; Those two variables were introduced in Emacs 27.1
  (setq display-line-numbers-major-tick 0)
  (setq display-line-numbers-minor-tick 0)
  ;; Use absolute numbers in narrowed buffers
  (setq-default display-line-numbers-widen t))

;;;; `window', `display-buffer-alist', and related
(prot-emacs-configure
  (require 'prot-window)
  ;; NOTE 2023-03-17: Remember that I am using development versions of
  ;; Emacs.  Some of my `display-buffer-alist' contents are for Emacs
  ;; 29+.
  (setq display-buffer-alist
        `(;; no window
          ("\\`\\*Async Shell Command\\*\\'"
           (display-buffer-no-window))
          ("\\`\\*\\(Warnings\\|Compile-Log\\|Org Links\\)\\*\\'"
           (display-buffer-no-window)
           (allow-no-window . t))
          ;; bottom side window
          ("\\*\\(Org \\(Select\\|Note\\)\\|Agenda Commands\\)\\*" ; the `org-capture' key selection and `org-add-log-note'
           (display-buffer-in-side-window)
           (dedicated . t)
           (side . bottom)
           (slot . 0)
           (window-parameters . ((mode-line-format . none))))
          ;; bottom buffer (NOT side window)
          ((or . ((derived-mode . flymake-diagnostics-buffer-mode)
                  (derived-mode . flymake-project-diagnostics-mode)
                  (derived-mode . messages-buffer-mode)
                  (derived-mode . backtrace-mode)))
           (display-buffer-reuse-mode-window display-buffer-at-bottom)
           (mode . ( flymake-diagnostics-buffer-mode flymake-project-diagnostics-mode
                     messages-buffer-mode backtrace-mode))
           (inhibit-switch-frame . t)
           (window-height . 0.3)
           (dedicated . t)
           (preserve-size . (t . t))
           (body-function . select-window))
          ("\\*Embark Actions\\*"
           (display-buffer-below-selected)
           (window-height . fit-window-to-buffer)
           (window-parameters . ((no-other-window . t)
                                 (mode-line-format . none))))
          ("\\*\\(Output\\|Register Preview\\).*"
           (display-buffer-reuse-mode-window display-buffer-at-bottom)
           (inhibit-switch-frame . t))
          ;; below current window
          ("*prot-elisp-macroexpand*"
           (display-buffer-below-selected)
           (window-height . 0.3)
           (dedicated . t)
           (preserve-size . (t . t))
           (body-function . select-window))
          ("\\(\\*Capture\\*\\|CAPTURE-.*\\)"
           (display-buffer-reuse-mode-window display-buffer-below-selected)
           (inhibit-switch-frame . t))
          ((derived-mode . reb-mode) ; M-x re-builder
           (display-buffer-reuse-mode-window display-buffer-below-selected)
           (inhibit-switch-frame . t)
           (window-height . 4) ; note this is literal lines, not relative
           (dedicated . t)
           (preserve-size . (t . t)))
          ((or . ((derived-mode . occur-mode)
                  (derived-mode . grep-mode)
                  (derived-mode . Buffer-menu-mode)
                  (derived-mode . log-view-mode)
                  (derived-mode . help-mode) ; See the hooks for `visual-line-mode'
                  "\\*\\(|Buffer List\\|Occur\\|vc-change-log\\|eldoc.*\\).*"
                  "\\*\\vc-\\(incoming\\|outgoing\\|git : \\).*"))
           (prot-window-display-buffer-below-or-pop)
           (body-function . prot-window-select-fit-size))
          (prot-window-shell-or-term-p
           (display-buffer-reuse-mode-window display-buffer-at-bottom)
           (mode . (shell-mode eshell-mode comint-mode))
           (inhibit-switch-frame . t)
           (body-function . prot-window-select-fit-size))
          ("\\*\\(Calendar\\|Bookmark Annotation\\|ert\\).*"
           (display-buffer-reuse-mode-window display-buffer-below-selected)
           (mode . (calendar-mode bookmark-edit-annotation-mode ert-results-mode))
           (inhibit-switch-frame . t)
           (dedicated . t)
           (window-height . fit-window-to-buffer))
          ;; NOTE 2022-09-10: The following is for `ispell-word', though
          ;; it only works because I override `ispell-display-buffer'
          ;; with `prot-spell-ispell-display-buffer' and change the
          ;; value of `ispell-choices-buffer'.
          ("\\*ispell-top-choices\\*.*"
           (display-buffer-below-selected)
           (window-height . fit-window-to-buffer))
          ;; same window

          ;; NOTE 2023-02-17: `man' does not fully obey the
          ;; `display-buffer-alist'.  It works for new frames and for
          ;; `display-buffer-below-selected', but otherwise is
          ;; unpredictable.  See `Man-notify-method'.
          ((or . ((derived-mode . Man-mode)
                  (derived-mode . woman-mode)
                  "\\*\\(Man\\|woman\\).*"))
           (display-buffer-same-window))
          ;; other `tab-bar-mode' tab
          ((derived-mode . magit-status-mode)
           (display-buffer-reuse-mode-window display-buffer-in-tab)
           (mode . magit-status-mode)
           (tab-name . (lambda (buffer _alist) (buffer-name buffer)))
           (reusable-frames . :just-the-selected-frame)
           (inhibit-switch-frame . t)))))

(prot-emacs-configure
  (setq split-window-preferred-direction 'horizontal) ; Emacs 31
  (setq window-combination-resize t)
  (setq even-window-sizes 'height-only)
  (setq window-sides-vertical nil)
  (setq switch-to-buffer-in-dedicated-window 'pop)
  (setq split-height-threshold 85)
  (setq split-width-threshold 125)
  (setq window-min-height 3)
  (setq window-min-width 30))

(prot-emacs-configure
  (prot-emacs-hook (epa-info-mode-hook help-mode-hook custom-mode-hook) visual-line-mode))

(prot-emacs-configure
  (prot-emacs-hook (world-clock-mode-hook calendar-mode-hook) prot-common-truncate-lines-silently))

(prot-emacs-configure
  (prot-emacs-keybind global-map
    ;; NOTE 2022-09-17: Also see `prot-simple-swap-window-buffers'.
    "C-x <down>" #'next-buffer
    "C-x <up>" #'previous-buffer
    "C-x C-n" #'next-buffer     ; override `set-goal-column'
    "C-x C-p" #'previous-buffer ; override `mark-page'
    "C-x !" #'delete-other-windows-vertically
    "C-x _" #'balance-windows      ; underscore
    "C-x -" #'fit-window-to-buffer ; hyphen
    "C-x +" #'balance-windows-area
    "C-x }" #'enlarge-window
    "C-x {" #'shrink-window
    "C-x >" #'enlarge-window-horizontally ; override `scroll-right'
    "C-x <" #'shrink-window-horizontally) ; override `scroll-left'
  (prot-emacs-keybind resize-window-repeat-map
    ">" #'enlarge-window-horizontally
    "<" #'shrink-window-horizontally))

;;; Frame-isolated buffers
;; Another package of mine.  Read the manual:
;; <https://protesilaos.com/emacs/beframe>.
(prot-emacs-configure
  (prot-emacs-install beframe)
  (require 'beframe)

  (setq beframe-functions-in-frames '(project-prompt-project-dir))

  (prot-emacs-keybind global-map
    ;; Override the `set-fill-column' that I have no use for.
    "C-x f" #'other-frame-prefix
    ;; Bind Beframe commands to a prefix key. Notice the -map as I am
    ;; binding keymap here, not a command.
    "C-c b" #'beframe-prefix-map
    ;; Replace the generic `buffer-menu'.  With a prefix argument, this
    ;; commands prompts for a frame.  Call the `buffer-menu' via M-x if
    ;; you absolutely need the global list of buffers.
    "C-x C-b" #'beframe-buffer-menu
    ;; Not specific to Beframe, but since it renames frames (by means
    ;; of `beframe-mode') it is appropriate to have this here:
    "C-x B" #'select-frame-by-name)

  (beframe-mode 1)

  ;; Integration with the `consult-buffer' command.  It will show only
  ;; buffers from the current frame.  To view all buffers, first input
  ;; a space at the empty minibuffer prompt.  This enables the "hidden
  ;; buffers" view.
  (with-eval-after-load 'consult
    (defun consult-beframe-buffer-list (&optional frame)
      "Return the list of buffers from `beframe-buffer-names' sorted by visibility.
With optional argument FRAME, return the list of buffers of FRAME.

For use in `consult-buffer-list'."
      (beframe-buffer-list frame :sort #'beframe-buffer-sort-visibility))

    (setq consult-buffer-list #'consult-beframe-buffer-list)))

;;; Frame history (undelete-frame-mode)
(prot-emacs-configure
  (define-key global-map (kbd "C-x u") #'undelete-frame) ; I use only C-/ for `undo'
  (undelete-frame-mode 1))

;;; Window history (winner-mode)
(prot-emacs-configure
  (prot-emacs-keybind global-map
    "C-x <right>" #'winner-redo
    "C-x <left>" #'winner-undo)

  (winner-mode 1))

;;; Directional window motions (windmove)
(prot-emacs-configure
  ;; Those override some commands that are already available with
  ;; C-M-u, C-M-f, C-M-b.
  (prot-emacs-keybind global-map
    "C-M-<up>" #'windmove-up
    "C-M-<right>" #'windmove-right
    "C-M-<down>" #'windmove-down
    "C-M-<left>" #'windmove-left
    "C-M-S-<up>" #'windmove-swap-states-up
    "C-M-S-<right>" #'windmove-swap-states-right ; conflicts with `org-increase-number-at-point'
    "C-M-S-<down>" #'windmove-swap-states-down
    "C-M-S-<left>" #'windmove-swap-states-left)

  (setq windmove-create-window nil)) ; Emacs 27.1

(provide 'prot-emacs-window)
