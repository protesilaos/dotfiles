;;; Essential configurations
(use-package emacs
  :ensure nil
  :demand t
  :config
;;;; General settings and common custom functions (prot-simple.el)
  (setq blink-matching-paren nil)
  (setq delete-pair-blink-delay 0.1) ; Emacs28 -- see `prot-simple-delete-pair-dwim'
  (setq delete-pair-push-mark t) ; Emacs 31
  (setq help-window-select t)
  (setq next-error-recenter '(4)) ; center of the window
  (setq find-library-include-other-files nil) ; Emacs 29
  (setq remote-file-name-inhibit-delete-by-moving-to-trash t) ; Emacs 30
  (setq remote-file-name-inhibit-auto-save t)                 ; Emacs 30
  (setq tramp-connection-timeout (* 60 10)) ; seconds
  (setq save-interprogram-paste-before-kill t)
  (setq mode-require-final-newline 'visit-save)
  (setq-default truncate-partial-width-windows nil)
  (setq eval-expression-print-length nil)
  (setq kill-do-not-save-duplicates t)
  (setq duplicate-line-final-position -1 ; both are Emacs 29
        duplicate-region-final-position -1)
  (setq scroll-error-top-bottom t)
  (setq echo-keystrokes-help nil) ; Emacs 30
  (setq epa-keys-select-method 'minibuffer) ; Emacs 30
  (setq trusted-content '("~/Git/Projects/")) ; Emacs 30

  ;; Keys I unbind here are either to avoid accidents or to bind them
  ;; elsewhere later in the configuration.
  :bind
  ( :map global-map
    ("<f2>" . toggle-input-method)  ; F2 overrides that two-column gimmick.  Sorry, but no!
    ("<insert>" . nil)
    ("<menu>" . nil)
    ("C-x C-d" . nil) ; never use it
    ("C-x C-v" . nil) ; never use it
    ("C-z" . nil) ; I have a window manager, thanks!
    ("C-x C-z" . nil) ; same idea as above
    ("C-x C-c" . nil) ; avoid accidentally exiting Emacs
    ("C-x C-c C-c" . save-buffers-kill-emacs) ; more cumbersome, less error-prone
    ("C-x C-r" . restart-emacs) ; override `find-file-read-only'
    ("C-h h" . nil) ; Never show that "hello" file
    ("M-`" . nil)
    ("M-o" . delete-blank-lines) ; alias for C-x C-o
    ("M-SPC" . cycle-spacing)
    ("M-z" . zap-up-to-char) ; NOT `zap-to-char'
    ("M-c" . capitalize-dwim)
    ("M-l" . downcase-dwim) ; "lower" case
    ("M-u" . upcase-dwim)
    ("M-=" . count-words)
    ("C-x O" . next-multiframe-window)
    ("C-h K" . describe-keymap) ; overrides `Info-goto-emacs-key-command-node'
    ("C-h u" . apropos-user-option)
    ("C-h F" . apropos-function) ; lower case is `describe-function'
    ("C-h V" . apropos-variable) ; lower case is `describe-variable'
    ("C-h L" . apropos-library) ; lower case is `view-lossage'
    ("C-h c" . describe-char) ; overrides `describe-key-briefly'

    :map prog-mode-map
    ("C-M-d" . up-list) ; confusing name for what looks like "down" to me
    ("<C-M-backspace>" . backward-kill-sexp)

    ;; Keymap for buffers (Emacs28)
    :map ctl-x-x-map
    ("f" . follow-mode)  ; override `font-lock-update'
    ("r" . rename-uniquely)
    ("l" . visual-line-mode)))

(use-package prot-common
  :ensure nil
  :functions (prot-common-truncate-lines-silently)
  :hook ((text-mode prog-mode dired-mode prot/fundamental-mode hexl-mode comint-mode) . prot-common-truncate-lines-silently)
  :init
  (defvar prot/fundamental-mode-hook nil
    "Normal hook for `fundamental-mode' (which is missing by default).")

  (defun prot/fundamental-mode-run-hook (&rest args)
    "Apply ARGS and then run `prot/fundamental-mode-hook'."
    (apply args)
    (run-hooks 'prot/fundamental-mode-hook))

  (advice-add #'fundamental-mode :around #'prot/fundamental-mode-run-hook)
  :config
  ;; NEVER tell me which key can call a command that I specifically
  ;; invoked with M-x: I have a good reason to use it that way.
  (advice-add #'execute-extended-command--describe-binding-msg :override #'prot-common-ignore))

(use-package prot-simple
  :ensure nil
  :demand t
  :config
  (setq prot-simple-date-specifier "%F")
  (setq prot-simple-time-specifier "%R %z")

  (advice-add #'save-buffers-kill-emacs :before #'prot-simple-display-unsaved-buffers-on-exit)

  ;; All `prot-simple-override-mode' does is activate a key map.
  ;; Below I add keys to that map.  Because the mode is enabled
  ;; globally, those keys take precedence over the ones specified by
  ;; any given major mode.  In principle, this means that my keys will
  ;; always work (though technically they can be overriden by another
  ;; minor mode, depending on which one is evaluated last).
  (prot-simple-override-mode 1)

  (with-eval-after-load 'pulsar
    (add-hook 'prot-simple-file-to-register-jump-hook #'pulsar-recenter-center)
    (add-hook 'prot-simple-file-to-register-jump-hook #'pulsar-reveal-entry))
  :bind
  ( :map prot-simple-override-mode-map
    ("C-a" . prot-simple-duplicate-line-or-region) ; "again" mnemonic, overrides `move-beginning-of-line'
    ("C-d" . prot-simple-delete-line) ; overrides `delete-char'

    ("C-v" . prot-simple-multi-line-below) ; overrides `scroll-up-command'
    ("<next>" . prot-simple-multi-line-below) ; overrides `scroll-up-command'
    ("M-v" . prot-simple-multi-line-above) ; overrides `scroll-down-command'
    ("<prior>" . prot-simple-multi-line-above) ; overrides `scroll-down-command'

    :map global-map
    ("ESC" . prot-simple-keyboard-quit-dwim)
    ("<escape>" . prot-simple-keyboard-quit-dwim)
    ("C-g" . prot-simple-keyboard-quit-dwim)
    ("C-M-SPC" . prot-simple-mark-sexp)   ; will be overriden by `expreg' if tree-sitter is available
    ("C-," . prot-simple-mark-sexp)   ; I also have `isearch-forward-symbol-at-point' on C-.
    ;; Commands for lines
    ("C-S-d" . prot-simple-delete-line-backward)
    ("M-k" . prot-simple-kill-line-backward)
    ("M-j" . delete-indentation)
    ("C-w" . prot-simple-kill-region)
    ("M-w" . prot-simple-kill-ring-save)

    ("C-S-w" . prot-simple-copy-line)
    ("C-S-y" . prot-simple-yank-replace-line-or-region)
    ("<C-return>" . prot-simple-new-line-below)
    ("<C-S-return>" . prot-simple-new-line-above)
    ("C-x x a" . prot-simple-auto-fill-visual-line-mode) ; auto-fill/visual-line toggle
    ;; Commands for text insertion or manipulation
    ("C-=" . prot-simple-insert-date)
    ("C-<" . prot-simple-escape-url-dwim)
    ;; "C->" prot-simple-insert-line-prefix-dwim
    ("M-Z" . prot-simple-zap-to-char-backward)
    ;; Commands for object transposition
    ("C-S-p" . prot-simple-move-above-dwim)
    ("C-S-n" . prot-simple-move-below-dwim)
    ("C-t" . prot-simple-transpose-chars)
    ("C-x C-t" . prot-simple-transpose-lines)
    ("C-S-t" . prot-simple-transpose-paragraphs)
    ("C-x M-t" . prot-simple-transpose-sentences)
    ("C-M-t" . prot-simple-transpose-sexps)
    ("M-t" . prot-simple-transpose-words)
    ;; Commands for paragraphs
    ("M-Q" . prot-simple-unfill-region-or-paragraph)
    ;; Commands for windows and pages
    ("C-x o" . prot-simple-other-window)
    ("C-x n k" . prot-simple-delete-page-delimiters)
    ("M-r" . rotate-windows) ; Emacs 31 override `move-to-window-line-top-bottom'
    ("M-S-r" . rotate-windows-back) ; Emacs 31
    ;; Commands for buffers
    ("<C-f2>" . prot-simple-rename-file-and-buffer)
    ("C-x k" . prot-simple-kill-buffer-current)
    ("C-x K" . kill-buffer) ; leaving this here to contrast with the above
    ("M-s b" . prot-simple-buffers-major-mode)
    ("M-s v" . prot-simple-buffers-vc-root)
    ;; Commands for files
    ("C-x r ." . prot-simple-file-to-register)))

;;;; Scratch buffers per major mode (prot-scratch.el)
(use-package prot-scratch
  :ensure nil
  :bind ("C-c s" . prot-scratch-buffer)
  :config
  (setq prot-scratch-default-mode 'text-mode))

;;;; Insert character pairs (prot-pair.el)
(use-package prot-pair
  :ensure nil
  :bind
  (("C-'" . prot-pair-insert)
   ("M-'" . prot-pair-insert)
   ("M-\\" . prot-pair-delete)))

;;;; Comments (prot-comment.el)
(use-package prot-comment
  :ensure nil
  :init
  (setq comment-empty-lines t)
  (setq comment-fill-column nil)
  (setq comment-multi-line t)
  (setq comment-style 'multi-line)
  (setq-default comment-column 0)

  (setq prot-comment-comment-keywords '("TODO" "NOTE" "XXX" "REVIEW" "FIXME"))
  (setq prot-comment-timestamp-format-concise "%F")
  (setq prot-comment-timestamp-format-verbose "%F %T %z")
  :bind
  (("C-;" . prot-comment)
   ("C-x C-;" . prot-comment-timestamp-keyword)))

;;;; Prefix keymap (prot-prefix.el)
(use-package prot-prefix
  :ensure nil
  :bind-keymap
  (("<insert>" . prot-prefix)
   ("C-z" . prot-prefix)))

(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :config
  (setq recentf-max-saved-items 100)
  (setq recentf-max-menu-items 25) ; I don't use the `menu-bar-mode', but this is good to know
  (setq recentf-save-file-modes nil)
  (setq recentf-keep nil)
  (setq recentf-auto-cleanup nil)
  (setq recentf-initialize-file-name-history nil)
  (setq recentf-filename-handlers nil)
  (setq recentf-show-file-shortcuts-flag nil))

;;;; Mouse and mouse wheel behaviour
(use-package mouse
  :ensure nil
  :hook (after-init . mouse-wheel-mode)
  :config
  ;; Some of these variables are defined in places other than
  ;; mouse.el, but this is fine.
  (setq mouse-autoselect-window t) ; complements the auto-selection of my tiling window manager

  ;; In Emacs 27+, use Control + mouse wheel to scale text.
  (setq mouse-wheel-scroll-amount
        '(1
          ((shift) . 5)
          ((meta) . 0.5)
          ((control) . text-scale))
        mouse-drag-copy-region nil
        make-pointer-invisible t
        mouse-wheel-progressive-speed t
        mouse-wheel-follow-mouse t)

  ;; Scrolling behaviour
  (setq-default scroll-preserve-screen-position t
                scroll-conservatively 1 ; affects `scroll-step'
                scroll-margin 0
                next-screen-context-lines 0))

;;;; Repeatable key chords (repeat-mode)
(use-package repeat
  :ensure nil
  :hook (after-init . repeat-mode)
  :config
  (setq repeat-on-final-keystroke t
        repeat-exit-timeout 5
        repeat-exit-key "<escape>"
        repeat-keep-prefix nil
        repeat-check-key t
        repeat-echo-function 'ignore
        ;; Technically, this is not in repeal.el, though it is the
        ;; same idea.
        set-mark-command-repeat-pop t))

;;;; Built-in bookmarking framework (bookmark.el)
(use-package bookmark
  :ensure nil
  :commands (bookmark-set bookmark-jump bookmark-bmenu-list)
  :hook (bookmark-bmenu-mode . hl-line-mode)
  :config
  (setq bookmark-use-annotations nil)
  (setq bookmark-automatically-show-annotations nil)
  (setq bookmark-fringe-mark nil) ; Emacs 29 to hide bookmark fringe icon
  ;; Write changes to the bookmark file as soon as 1 modification is
  ;; made (addition or deletion).  Otherwise Emacs will only save the
  ;; bookmarks when it closes, which may never happen properly
  ;; (e.g. power failure).
  (setq bookmark-save-flag 1))

;;;; Registers (register.el)
(use-package register
  :ensure nil
  :defer t ; its commands are autoloaded, so this will be loaded then
  :config
  (setq register-preview-delay 0.8
        register-preview-function #'register-preview-default)

  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'register-alist)))

;;;; Auto revert mode
(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode)
  :config
  (setq auto-revert-verbose t))

;;;; Delete selection
(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

;;;; Tooltips (tooltip-mode)
(use-package tooltip
  :ensure nil
  :hook (after-init . tooltip-mode)
  :config
  (setq tooltip-delay 0.5
        tooltip-short-delay 0.5
        x-gtk-use-system-tooltips t
        tooltip-frame-parameters
        '((name . "tooltip")
          (internal-border-width . 10)
          (border-width . 0)
          (no-special-glyphs . t))))

;;;; Display current time
(use-package time
  :ensure nil
  :hook (after-init . display-time-mode)
  :config
  (setq display-time-format " %a %e %b, %H:%M ")
  ;;;; Covered by `display-time-format'
  ;; (setq display-time-24hr-format t)
  ;; (setq display-time-day-and-date t)
  (setq display-time-interval 60)
  (setq display-time-default-load-average nil)
  ;; NOTE 2022-09-21: For all those, I have implemented my own solution
  ;; that also shows the number of new items, although it depends on
  ;; notmuch: the `notmuch-indicator' package.
  (setq display-time-mail-directory nil)
  (setq display-time-mail-function nil)
  (setq display-time-use-mail-icon nil)
  (setq display-time-mail-string nil)
  (setq display-time-mail-face nil)

  ;; I don't need the load average and the mail indicator, so let this
  ;; be simple:
  (setq display-time-string-forms
        '((propertize
           (format-time-string display-time-format now)
           'face 'display-time-date-and-time
           'help-echo (format-time-string "%a %b %e, %Y" now))
          " ")))

;;;; World clock (M-x world-clock)
(use-package time
  :ensure nil
  :commands (world-clock)
  :config
  (setq display-time-world-list t)
  (setq zoneinfo-style-world-list ; M-x shell RET timedatectl list-timezones
        '(("America/Los_Angeles" "Los Angeles")
          ("America/Vancouver" "Vancouver")
          ("America/Chicago" "Chicago")
          ("America/Toronto" "Toronto")
          ("America/New_York" "New York")
          ("UTC" "UTC")
          ("Europe/Lisbon" "Lisbon")
          ("Europe/Brussels" "Brussels")
          ("Europe/Athens" "Athens")
          ("Asia/Riyadh" "Riyadh")
          ("Asia/Tbilisi" "Tbilisi")
          ("Asia/Singapore" "Singapore")
          ("Asia/Shanghai" "Shanghai")
          ("Asia/Seoul" "Seoul")
          ("Asia/Tokyo" "Tokyo")
          ("Australia/Brisbane" "Brisbane")
          ("Australia/Sydney" "Sydney")
          ("Pacific/Auckland" "Auckland")))

  ;; All of the following variables are for Emacs 28
  (setq world-clock-list t)
  (setq world-clock-time-format "%z %R	%a %d %b (%Z)")
  (setq world-clock-buffer-name "*world-clock*") ; Placement handled by `display-buffer-alist'
  (setq world-clock-timer-enable t)
  (setq world-clock-timer-second 60))

;;;; `man' (manpages)
(use-package man
  :ensure nil
  :commands (man)
  :config
  (setq Man-notify-method 'pushy)) ; does not obey `display-buffer-alist'

;;;; `proced' (process monitor, similar to `top')
(use-package proced
  :ensure nil
  :commands (proced)
  :config
  (setq proced-auto-update-flag 'visible) ; Emacs 30 supports more the `visible' value
  (setq proced-enable-color-flag t) ; Emacs 29
  (setq proced-auto-update-interval 5)
  (setq proced-descend t)
  (setq proced-filter 'user))

;;;; Emacs server (allow emacsclient to connect to running session)
(use-package server
  :ensure nil
  :defer 1
  :config
  (setq server-client-instructions nil)
  (unless (server-running-p)
    (server-start)))

;;; Substitute
;; Another package of mine... Video demo:
;; <https://protesilaos.com/codelog/2023-01-16-emacs-substitute-package-demo/>.
(use-package substitute
  :ensure t
  :defer 1
  ;; Produce a message after the substitution that reports on what
  ;; happened.  It is a single line, like "Substituted `TARGET' with
  ;; `SUBSTITUTE' N times across the buffer.
  :hook (substitute-post-replace . substitute-report-operation)
  :commands
  (substitute-target-below-point ; Forward motion like isearch (C-s)
   substitute-target-above-point ; Backward motion like isearch (C-r)
   substitute-target-in-defun    ; inside of the current definition
   substitute-target-in-buffer)  ; throughout the buffer
  :config
  ;; Set this to non-nil to highlight all occurrences of the current
  ;; target.
  (setopt substitute-highlight t)

  ;; Set this to t if you want to always treat the letter casing
  ;; literally.  Otherwise each command accepts a `C-u' prefix
  ;; argument to do this on-demand.
  (setq substitute-fixed-letter-case nil)

  ;; C-c s is occupied by `prot-scratch-buffer'.
  (define-key global-map (kbd "C-c r") #'substitute-prefix-map))

(use-package goto-chg
  :ensure t
  :bind
  (("C-(" . goto-last-change)
   ("C-)" . goto-last-change-reverse)))

;;; Mark syntactic constructs efficiently if tree-sitter is available (expreg)
(when (and (treesit-available-p) prot-emacs-treesitter-extras)
  (use-package expreg
    :ensure t
    :functions (prot/expreg-expand prot/expreg-expand-dwim)
    ;; There is also an `expreg-contract' command, though I have no use for it.
    :bind ("C-M-SPC" . prot/expreg-expand-dwim) ; overrides `mark-sexp'
    :config
    (defun prot/expreg-expand (n)
      "Expand to N syntactic units, defaulting to 1 if none is provided interactively."
      (interactive "p")
      (dotimes (_ n)
        (expreg-expand)))

    (defun prot/expreg-expand-dwim ()
      "Do-What-I-Mean `expreg-expand' to start with symbol or word.
If over a real symbol, mark that directly, else start with a
word.  Fall back to regular `expreg-expand'."
      (interactive)
      (let ((symbol (bounds-of-thing-at-point 'symbol)))
        (cond
         ((equal (bounds-of-thing-at-point 'word) symbol)
          (prot/expreg-expand 1))
         (symbol (prot/expreg-expand 2))
         (t (expreg-expand)))))))

;;; TMR May Ring (tmr is used to set timers)
;; Read the manual: <https://protesilaos.com/emacs/tmr>.
(use-package tmr
  :ensure t
  :bind
  (("C-c t t" . tmr)
   ("C-c t T" . tmr-with-description)
   ("C-c t l" . tmr-tabulated-view) ; "list timers" mnemonic
   ("C-c t c" . tmr-clone)
   ("C-c t k" . tmr-cancel)
   ("C-c t s" . tmr-reschedule)
   ("C-c t e" . tmr-edit-description)
   ("C-c t r" . tmr-remove)
   ("C-c t R" . tmr-remove-finished))
  :config
  (setq tmr-sound-file "/usr/share/sounds/freedesktop/stereo/alarm-clock-elapsed.oga"
        tmr-notification-urgency 'normal
        tmr-description-list 'tmr-description-history))

;;; Pass interface (password-store)
(use-package password-store
  :ensure t
  ;; Mnemonic is the root of the "code" word (κώδικας).  But also to add
  ;; the password to the kill-ring.  Other options are already taken.
  :bind ("C-c k" . password-store-copy)
  :config
  (setq password-store-time-before-clipboard-restore 30))

(use-package pass
 :ensure t
 :commands (pass))

;;; Shell (M-x shell)
(use-package shell
  :ensure nil
  :bind
  ( :map shell-mode-map
    ("C-c C-k" . comint-clear-buffer)
    ("C-c C-w" . comint-write-output))
  :config
  ;; Check my .bashrc which handles `comint-terminfo-terminal':
  ;;
  ;; # Default pager.  The check for the terminal is useful for Emacs with
  ;; # M-x shell (which is how I usually interact with bash these days).
  ;; #
  ;; # The COLORTERM is documented in (info "(emacs) General Variables").
  ;; # I found the reference to `dumb-emacs-ansi' in (info "(emacs)
  ;; # Connection Variables").
  ;; if [ "$TERM" = "dumb" ] && [ "$INSIDE_EMACS" ] || [ "$TERM" = "dumb-emacs-ansi" ] && [ "$INSIDE_EMACS" ]
  ;; then
  ;;     export PAGER="cat"
  ;;     alias less="cat"
  ;;     export TERM=dumb-emacs-ansi
  ;;     export COLORTERM=1
  ;; else
  ;;     # Quit once you try to scroll past the end of the file.
  ;;     export PAGER="less --quit-at-eof"
  ;; fi

  (setq shell-command-prompt-show-cwd t) ; Emacs 27.1
  (setq ansi-color-for-comint-mode t)
  (setq shell-input-autoexpand 'input)
  (setq shell-highlight-undef-enable t) ; Emacs 29.1
  (setq shell-has-auto-cd nil) ; Emacs 29.1
  (setq shell-get-old-input-include-continuation-lines t) ; Emacs 30.1
  (setq shell-kill-buffer-on-exit t) ; Emacs 29.1
  (setq shell-completion-fignore '("~" "#" "%"))
  (setq-default comint-scroll-to-bottom-on-input t)
  (setq-default comint-scroll-to-bottom-on-output nil)
  (setq-default comint-input-autoexpand 'input)
  (setq comint-prompt-read-only t)
  (setq comint-buffer-maximum-size 9999)
  (setq comint-completion-autolist t)
  (setq comint-input-ignoredups t)
  (setq tramp-default-remote-shell "/bin/bash")

  (setq shell-font-lock-keywords
        '(("[ \t]\\([+-][^ \t\n]+\\)" 1 font-lock-builtin-face)
          ("^[^ \t\n]+:.*" . font-lock-string-face)
          ("^\\[[1-9][0-9]*\\]" . font-lock-constant-face)))

  ;; Support for OS-specific escape sequences such as what `ls
  ;; --hyperlink' uses.  I normally don't use those, but I am checking
  ;; this to see if there are any obvious advantages/disadvantages.
  (add-hook 'comint-output-filter-functions 'comint-osc-process-output))

(use-package prot-shell
  :ensure nil
  :bind (("<f1>" . prot-shell)) ; I don't use F1 for help commands
  :hook (shell-mode . prot-shell-mode))

;;; Laptop settings
(unless (directory-empty-p "/sys/class/power_supply/")
  (add-to-list 'default-frame-alist '(width . (text-pixels . 800)))
  (add-to-list 'default-frame-alist '(height . (text-pixels . 600)))

  (use-package battery
    :ensure nil
    :hook (after-init . display-battery-mode)
    :config
;;;; Show battery status on the mode line (battery.el)
    (setq battery-mode-line-format
          (cond
           ((eq battery-status-function #'battery-linux-proc-acpi)
	        "⏻%b%p%%,%d°C ")
	       (battery-status-function
	        "⏻%b%p%% ")))))

(provide 'prot-emacs-essentials)
