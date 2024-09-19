(with-eval-after-load 'org-capture
  (add-hook 'org-capture-after-finalize-hook #'prot-window-delete-popup-frame))

(with-eval-after-load 'tmr
  (add-hook 'tmr-timer-created-functions #'prot-window-delete-popup-frame))

;;; General window and buffer configurations
(use-package uniquify
  :ensure nil
  :config
;;;; `uniquify' (unique names for buffers)
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-strip-common-suffix t)
  (setq uniquify-after-kill-buffer-p t))

;;;; Line highlight
(use-package hl-line
  :ensure nil
  :commands (hl-line-mode)
  :config
  (setq hl-line-sticky-flag nil)
  (setq hl-line-overlay-priority -50)) ; emacs28

;;;; Negative space highlight
(use-package whitespace
  :ensure nil
  :bind
  (("<f6>" . whitespace-mode)
   ("C-c z" . delete-trailing-whitespace))
  :config
  ;; NOTE 2023-08-14: This is experimental.  I am not sure I like it.
  (setq whitespace-style
        '(face
          tabs
          spaces
          tab-mark
          space-mark
          trailing
          missing-newline-at-eof
          space-after-tab::tab
          space-after-tab::space
          space-before-tab::tab
          space-before-tab::space)))

;;; Line numbers on the side of the window
(use-package display-line-numbers
  :ensure nil
  :bind
  ("<f7>" . display-line-numbers-mode)
  :config
  (setq-default display-line-numbers-type t)
  ;; Those two variables were introduced in Emacs 27.1
  (setq display-line-numbers-major-tick 0)
  (setq display-line-numbers-minor-tick 0)
  ;; Use absolute numbers in narrowed buffers
  (setq-default display-line-numbers-widen t))

  ;;;; `window', `display-buffer-alist', and related
  (use-package prot-window
    :ensure nil
    :demand t
    :config
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
            ("\\*Org \\(Select\\|Note\\)\\*" ; the `org-capture' key selection and `org-add-log-note'
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
             (window-height . 0.3)
             (dedicated . t)
             (preserve-size . (t . t)))
            ("\\*Embark Actions\\*"
             (display-buffer-reuse-mode-window display-buffer-below-selected)
             (window-height . fit-window-to-buffer)
             (window-parameters . ((no-other-window . t)
                                   (mode-line-format . none))))
            ("\\*\\(Output\\|Register Preview\\).*"
             (display-buffer-reuse-mode-window display-buffer-at-bottom))
            ;; below current window
            ("\\(\\*Capture\\*\\|CAPTURE-.*\\)"
             (display-buffer-reuse-mode-window display-buffer-below-selected))
            ("\\*\\vc-\\(incoming\\|outgoing\\|git : \\).*"
             (display-buffer-reuse-mode-window display-buffer-below-selected)
             (window-height . 0.1)
             (dedicated . t)
             (preserve-size . (t . t)))
            ((derived-mode . reb-mode) ; M-x re-builder
             (display-buffer-reuse-mode-window display-buffer-below-selected)
             (window-height . 4) ; note this is literal lines, not relative
             (dedicated . t)
             (preserve-size . (t . t)))
            ((or . ((derived-mode . occur-mode)
                    (derived-mode . grep-mode)
                    (derived-mode . Buffer-menu-mode)
                    (derived-mode . log-view-mode)
                    (derived-mode . help-mode) ; See the hooks for `visual-line-mode'
                    "\\*\\(|Buffer List\\|Occur\\|vc-change-log\\|eldoc.*\\).*"
                    prot-window-shell-or-term-p
                    ;; ,world-clock-buffer-name
                    ))
             (prot-window-display-buffer-below-or-pop)
             (body-function . prot-window-select-fit-size))
            ("\\*\\(Calendar\\|Bookmark Annotation\\|ert\\).*"
             (display-buffer-reuse-mode-window display-buffer-below-selected)
             (dedicated . t)
             (window-height . fit-window-to-buffer))
            ;; NOTE 2022-09-10: The following is for `ispell-word', though
            ;; it only works because I override `ispell-display-buffer'
            ;; with `prot-spell-ispell-display-buffer' and change the
            ;; value of `ispell-choices-buffer'.
            ("\\*ispell-top-choices\\*.*"
             (display-buffer-reuse-mode-window display-buffer-below-selected)
             (window-height . fit-window-to-buffer))
            ;; same window

            ;; NOTE 2023-02-17: `man' does not fully obey the
            ;; `display-buffer-alist'.  It works for new frames and for
            ;; `display-buffer-below-selected', but otherwise is
            ;; unpredictable.  See `Man-notify-method'.
            ((or . ((derived-mode . Man-mode)
                    (derived-mode . woman-mode)
                    "\\*\\(Man\\|woman\\).*"))
             (display-buffer-same-window)))))

(use-package prot-window
  :ensure nil
  :demand t
  :config
  (setq window-combination-resize t)
  (setq even-window-sizes 'height-only)
  (setq window-sides-vertical nil)
  (setq switch-to-buffer-in-dedicated-window 'pop)
  (setq split-height-threshold 80)
  (setq split-width-threshold 125)
  (setq window-min-height 3)
  (setq window-min-width 30))

(use-package prot-window
  :ensure nil
  :demand t
  :hook
  ((epa-info-mode help-mode custom-mode) . visual-line-mode))

(use-package prot-window
  :ensure nil
  :demand t
  :hook
  ((world-clock-mode calendar-mode) . prot-common-truncate-lines-silently))

(use-package prot-window
  :ensure nil
  :demand t
  :bind
  ( :map global-map
    ;; NOTE 2022-09-17: Also see `prot-simple-swap-window-buffers'.
    ("C-x <down>" . next-buffer)
    ("C-x <up>" . previous-buffer)
    ("C-x C-n" . next-buffer)     ; override `set-goal-column'
    ("C-x C-p" . previous-buffer) ; override `mark-page'
    ("C-x !" . delete-other-windows-vertically)
    ("C-x _" . balance-windows)      ; underscore
    ("C-x -" . fit-window-to-buffer) ; hyphen
    ("C-x +" . balance-windows-area)
    ("C-x }" . enlarge-window)
    ("C-x {" . shrink-window)
    ("C-x >" . enlarge-window-horizontally) ; override `scroll-right'
    ("C-x <" . shrink-window-horizontally) ; override `scroll-left'
    :map resize-window-repeat-map
    (">" . enlarge-window-horizontally)
    ("<" . shrink-window-horizontally)))

;;; Frame-isolated buffers
;; Another package of mine.  Read the manual:
;; <https://protesilaos.com/emacs/beframe>.
(use-package beframe
  :ensure t
  :hook (after-init . beframe-mode)
  :config
  (setq beframe-functions-in-frames '(project-prompt-project-dir))

  ;; I use this instead of :bind because I am binding a keymap and the
  ;; way `use-package' does it is by wrapping a lambda around it that
  ;; then breaks `describe-key' for those keys.
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
    "C-x B" #'select-frame-by-name))

;;; Frame history (undelete-frame-mode)
(use-package frame
  :ensure nil
  :bind ("C-x u" . undelete-frame) ; I use only C-/ for `undo'
  :hook (after-init . undelete-frame-mode))

;;; Window history (winner-mode)
(use-package winner
  :ensure nil
  :hook (after-init . winner-mode)
  :bind
  (("C-x <right>" . winner-redo)
   ("C-x <left>" . winner-undo)))

;;; Directional window motions (windmove)
(use-package windmove
  :ensure nil
  :bind
  ;; Those override some commands that are already available with
  ;; C-M-u, C-M-f, C-M-b.
  (("C-M-<up>" . windmove-up)
   ("C-M-<right>" . windmove-right)
   ("C-M-<down>" . windmove-down)
   ("C-M-<left>" . windmove-left)
   ("C-M-S-<up>" . windmove-swap-states-up)
   ("C-M-S-<right>" . windmove-swap-states-right) ; conflicts with `org-increase-number-at-point'
   ("C-M-S-<down>" . windmove-swap-states-down)
   ("C-M-S-<left>" . windmove-swap-states-left))
  :config
  (setq windmove-create-window nil)) ; Emacs 27.1

;;; Header line context of symbol/heading (breadcrumb.el)
(use-package breadcrumb
  :ensure t
  :functions (prot/breadcrumb-local-mode)
  :hook ((text-mode prog-mode) . prot/breadcrumb-local-mode)
  :config
  (setq breadcrumb-project-max-length 0.5)
  (setq breadcrumb-project-crumb-separator "/")
  (setq breadcrumb-imenu-max-length 1.0)
  (setq breadcrumb-imenu-crumb-separator " > ")

  (defun prot/breadcrumb-local-mode ()
    "Enable `breadcrumb-local-mode' if the buffer is visiting a file."
    (when buffer-file-name
      (breadcrumb-local-mode 1))))

(provide 'prot-emacs-window)
