;;; Read environment variables

;; NOTE 2022-09-29: It seems I no longer need `exec-path-from-shell'.
;; It was necessary in earlier test when I was using the --with-pgtk
;; build of Emacs in a Sway WM session.  I am now using GNOME on
;; Wayland and everything seems to be in order.  But I am keeping it
;; around just in case.

;; (prot-emacs-elpa-package 'exec-path-from-shell
;;   (setq exec-path-from-shell-variables
;;         '("PATH" "MANPATH" "SSH_AUTH_SOCK"))
;;   (exec-path-from-shell-initialize))

;;; Common auxiliary functions (prot-common.el)
(prot-emacs-builtin-package 'prot-common)

;;; Common custom functions (prot-simple.el)
(prot-emacs-builtin-package 'prot-simple
  (setopt prot-simple-insert-pair-alist
          '(("' Single quote"        . (39 39))     ; ' '
            ("\" Double quotes"      . (34 34))     ; " "
            ("` Elisp quote"         . (96 39))     ; ` '
            ("‘ Single apostrophe"   . (8216 8217)) ; ‘ ’
            ("“ Double apostrophes"  . (8220 8221)) ; “ ”
            ("( Parentheses"         . (40 41))     ; ( )
            ("{ Curly brackets"      . (123 125))   ; { }
            ("[ Square brackets"     . (91 93))     ; [ ]
            ("< Angled brackets"     . (60 62))     ; < >
            ("« Εισαγωγικά Gr quote" . (171 187))   ; « »
            ("= Equals signs"        . (61 61))     ; = =
            ("~ Tilde"               . (126 126))   ; ~ ~
            ("* Asterisks"           . (42 42))     ; * *
            ("/ Forward Slash"       . (47 47))     ; / /
            ("_ underscores"         . (95 95)))   ; _ _
          prot-simple-date-specifier "%F"
          prot-simple-time-specifier "%R %z"
          delete-pair-blink-delay 0.15 ; Emacs28 -- see `prot-simple-delete-pair-dwim'
          prot-simple-scratch-buffer-default-mode 'markdown-mode
          help-window-select t
          next-error-recenter '(4)) ; center of the window

  ;; General commands
  (let ((map global-map))
    (define-key map (kbd "<insert>") nil)
    (define-key map (kbd "C-z") nil)
    (define-key map (kbd "C-x C-z") nil)
    (define-key map (kbd "C-h h") nil)
    (define-key map (kbd "M-`") nil)
    (define-key map (kbd "C-h .") #'prot-simple-describe-symbol) ; overrides `display-local-help'
    (define-key map (kbd "C-h K") #'describe-keymap) ; overrides `Info-goto-emacs-key-command-node'
    (define-key map (kbd "C-h c") #'describe-char) ; overrides `describe-key-briefly'
    (define-key map (kbd "C-c s") #'prot-simple-scratch-buffer)
    ;; Commands for lines
    (define-key map (kbd "C-S-w") #'prot-simple-copy-line-or-region)
    (define-key map (kbd "C-S-y") #'prot-simple-yank-replace-line-or-region)
    (define-key map (kbd "M-SPC") #'cycle-spacing)
    (define-key map (kbd "M-o") #'delete-blank-lines)   ; alias for C-x C-o
    (define-key map (kbd "M-k") #'prot-simple-kill-line-backward)
    (define-key map (kbd "C-S-n") #'prot-simple-multi-line-next)
    (define-key map (kbd "C-S-p") #'prot-simple-multi-line-prev)
    (define-key map (kbd "<C-return>") #'prot-simple-new-line-below)
    (define-key map (kbd "<C-S-return>") #'prot-simple-new-line-above)
    ;; Commands for text insertion or manipulation
    (define-key map (kbd "C-=") #'prot-simple-insert-date)
    (define-key map (kbd "C-<") #'prot-simple-escape-url)
    (define-key map (kbd "C-'") #'prot-simple-insert-pair)
    (define-key map (kbd "M-'") #'prot-simple-insert-pair)
    (define-key map (kbd "M-\\") #'prot-simple-delete-pair-dwim)
    (define-key map (kbd "M-z") #'zap-up-to-char) ; NOT `zap-to-char'
    (define-key map (kbd "M-Z") #'prot-simple-zap-to-char-backward)
    (define-key map (kbd "<C-M-backspace>") #'backward-kill-sexp)
    (define-key map (kbd "M-c") #'capitalize-dwim)
    (define-key map (kbd "M-l") #'downcase-dwim)        ; "lower" case
    (define-key map (kbd "M-u") #'upcase-dwim)
    ;; Commands for object transposition
    (define-key map (kbd "C-t") #'prot-simple-transpose-chars)
    (define-key map (kbd "C-x C-t") #'prot-simple-transpose-lines)
    (define-key map (kbd "C-S-t") #'prot-simple-transpose-paragraphs)
    (define-key map (kbd "C-x M-t") #'prot-simple-transpose-sentences)
    (define-key map (kbd "C-M-t") #'prot-simple-transpose-sexps)
    (define-key map (kbd "M-t") #'prot-simple-transpose-words)
    ;; Commands for marking objects
    (define-key map (kbd "M-@") #'prot-simple-mark-word)       ; replaces `mark-word'
    (define-key map (kbd "C-M-SPC") #'prot-simple-mark-construct-dwim)
    (define-key map (kbd "C-M-d") #'prot-simple-downward-list)
    ;; Commands for paragraphs
    (define-key map (kbd "M-Q") #'prot-simple-unfill-region-or-paragraph)
    ;; Commands for windows and pages
    (define-key map (kbd "C-x n k") #'prot-simple-delete-page-delimiters)
    (define-key map (kbd "C-x M") #'prot-simple-monocle)
    (define-key map (kbd "C-x M-r") #'prot-simple-swap-window-buffers)
    ;; NOTE 2022-03-02: Elsewhere I provide my `logos.el' package which
    ;; has the functionality of these three commands.
    ;;
    ;; (define-key map [remap narrow-to-region] #'prot-simple-narrow-dwim)
    ;; (define-key map [remap forward-page] #'prot-simple-forward-page-dwim)
    ;; (define-key map [remap backward-page] #'prot-simple-backward-page-dwim)
    ;;
    ;; Commands for buffers
    (define-key map (kbd "M-=") #'count-words)
    (define-key map (kbd "<C-f2>") #'prot-simple-rename-file-and-buffer)
    (define-key map (kbd "C-x K") #'prot-simple-kill-buffer-current)
    (define-key map (kbd "M-s b") #'prot-simple-buffers-major-mode)
    (define-key map (kbd "M-s v") #'prot-simple-buffers-vc-root)))

;;; Keymap for buffers (Emacs28)
(let ((map ctl-x-x-map))
  (define-key map "e" #'eval-buffer)
  (define-key map "f" #'follow-mode)  ; override `font-lock-update'
  (define-key map "r" #'rename-uniquely))

;;; Mouse wheel behaviour
(prot-emacs-builtin-package 'mouse
  ;; In Emacs 27+, use Control + mouse wheel to scale text.
  (setopt mouse-wheel-scroll-amount
          '(1
            ((shift) . 5)
            ((meta) . 0.5)
            ((control) . text-scale))
          mouse-drag-copy-region nil
          make-pointer-invisible t
          mouse-wheel-progressive-speed t
          mouse-wheel-follow-mouse t)
  (add-hook 'after-init-hook #'mouse-wheel-mode)
  (define-key global-map (kbd "C-M-<mouse-3>") #'tear-off-window))

;;; Scrolling behaviour
;; These four come from the C source code.
(setq-default scroll-preserve-screen-position t
              scroll-conservatively 1 ; affects `scroll-step'
              scroll-margin 0
              next-screen-context-lines 0)

;;; Delete selection
(prot-emacs-builtin-package 'delsel
  (add-hook 'after-init-hook #'delete-selection-mode))

;;; Tooltips (tooltip-mode)
(prot-emacs-builtin-package 'tooltip
  (setopt tooltip-delay 0.5
          tooltip-short-delay 0.5
          x-gtk-use-system-tooltips nil
          tooltip-frame-parameters
          '((name . "tooltip")
            (internal-border-width . 6)
            (border-width . 0)
            (no-special-glyphs . t)))
  (add-hook 'after-init-hook #'tooltip-mode))

;;; Auto revert mode
(prot-emacs-builtin-package 'autorevert
  (setopt auto-revert-verbose t)
  (add-hook 'after-init-hook #'global-auto-revert-mode))

;;; Preserve contents of system clipboard
(setopt save-interprogram-paste-before-kill t)

;;; Newline characters for file ending
(setopt mode-require-final-newline 'visit-save)

;;; Go to last change
(prot-emacs-elpa-package 'goto-last-change
  (define-key global-map (kbd "C-z") #'goto-last-change))

;;; Repeatable key chords (repeat-mode)
(prot-emacs-builtin-package 'repeat
  (setopt repeat-on-final-keystroke t
          repeat-exit-timeout 5
          repeat-exit-key "<escape>"
          repeat-keep-prefix nil
          repeat-check-key t
          repeat-echo-function 'ignore
          ;; Technically, this is not in repeal.el, though it is the
          ;; same idea.
          set-mark-command-repeat-pop t)
  (add-hook 'after-init-hook #'repeat-mode))

;;; Make Custom UI code disposable
(prot-emacs-builtin-package 'cus-edit
  ;; Disable the damn thing
  (setopt custom-file (make-temp-file "emacs-custom-")))

;;; TMR May Ring (tmr is used to set timers)
;; Read the manual: <https://protesilaos.com/emacs/tmr>.
(prot-emacs-elpa-package 'tmr
  (setopt tmr-sound-file "/usr/share/sounds/freedesktop/stereo/alarm-clock-elapsed.oga"
          tmr-notification-urgency 'normal
          tmr-description-list 'tmr-description-history)

  ;; You do not need these if you install the package.
  (require 'tmr-notification)
  (require 'tmr-tabulated)

  (let ((map global-map))
    (define-key map (kbd "C-c t t") #'tmr)
    (define-key map (kbd "C-c t T") #'tmr-with-description)
    (define-key map (kbd "C-c t l") #'tmr-tabulated-view) ; "list timers" mnemonic
    (define-key map (kbd "C-c t c") #'tmr-clone)
    (define-key map (kbd "C-c t k") #'tmr-cancel)
    (define-key map (kbd "C-c t s") #'tmr-reschedule)
    (define-key map (kbd "C-c t e") #'tmr-edit-description)
    (define-key map (kbd "C-c t r") #'tmr-remove)
    (define-key map (kbd "C-c t R") #'tmr-remove-finished)))

(provide 'prot-emacs-essentials)
