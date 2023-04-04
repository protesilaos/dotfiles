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
  (setq prot-simple-insert-pair-alist
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
          ("_ underscores"         . (95 95)))    ; _ _
        prot-simple-date-specifier "%F"
        prot-simple-time-specifier "%R %z"
        delete-pair-blink-delay 0.15 ; Emacs28 -- see `prot-simple-delete-pair-dwim'
        prot-simple-scratch-buffer-default-mode 'markdown-mode
        help-window-select t
        next-error-recenter '(4) ; center of the window
        find-library-include-other-files nil) ; Emacs 29

  ;; General commands
  (let ((map global-map))
    (define-key map (kbd "<insert>") nil)
    ;; (define-key map (kbd "C-z") nil)
    (define-key map (kbd "C-x C-z") nil)
    (define-key map (kbd "C-x C-c") nil) ; avoid accidentally exiting Emacs
    (define-key map (kbd "C-x C-c C-c") #'save-buffers-kill-emacs)
    (define-key map (kbd "C-h h") nil)
    (define-key map (kbd "M-`") nil)
    (define-key map (kbd "C-h .") #'prot-simple-describe-symbol) ; overrides `display-local-help'
    (define-key map (kbd "C-h K") #'describe-keymap) ; overrides `Info-goto-emacs-key-command-node'
    (define-key map (kbd "C-h c") #'describe-char) ; overrides `describe-key-briefly'
    (define-key map (kbd "C-c s") #'prot-simple-scratch-buffer)
    ;; Commands for lines
    (define-key map (kbd "M-o") #'delete-blank-lines)   ; alias for C-x C-o
    (define-key map (kbd "M-k") #'prot-simple-kill-line-backward)
    (define-key map (kbd "C-S-w") #'prot-simple-copy-line-or-region)
    (define-key map (kbd "C-S-y") #'prot-simple-yank-replace-line-or-region)
    (define-key map (kbd "M-SPC") #'cycle-spacing)
    (define-key map (kbd "C-S-n") #'prot-simple-multi-line-next)
    (define-key map (kbd "C-S-p") #'prot-simple-multi-line-prev)
    (define-key map (kbd "<C-return>") #'prot-simple-new-line-below)
    (define-key map (kbd "<C-S-return>") #'prot-simple-new-line-above)

    ;; Commands for text insertion or manipulation
    (define-key map (kbd "C-=") #'prot-simple-insert-date)
    (define-key map (kbd "C-<") #'prot-simple-escape-url-dwim)
    (define-key map (kbd "C-'") #'prot-simple-insert-pair)
    (define-key map (kbd "M-'") #'prot-simple-insert-pair)
    (define-key map (kbd "M-\\") #'prot-simple-delete-pair-dwim)
    ;; (define-key map (kbd "M-z") #'zap-up-to-char) ; NOT `zap-to-char'
    ;; (define-key map (kbd "M-Z") #'prot-simple-zap-to-char-backward)
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
    ;; ;; Commands for marking objects
    (define-key map (kbd "M-@") #'prot-simple-mark-word)       ; replaces `mark-word'
    (define-key map (kbd "C-M-SPC") #'prot-simple-mark-construct-dwim)
    (define-key map (kbd "C-M-d") #'prot-simple-downward-list)
    ;; Commands for paragraphs
    (define-key map (kbd "M-Q") #'prot-simple-unfill-region-or-paragraph)
    ;; Commands for windows and pages
    (define-key map (kbd "C-x n k") #'prot-simple-delete-page-delimiters)
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

;;; Prefix keymap (prot-prefix.el)
(prot-emacs-builtin-package 'prot-prefix
  (define-key global-map (kbd "C-z") #'prot-prefix))

;;; Substitute
;; Another package of mine... Video demo:
;; <https://protesilaos.com/codelog/2023-01-16-emacs-substitute-package-demo/>.
(prot-emacs-elpa-package 'substitute
  ;; Set this to non-nil to highlight all occurences of the current
  ;; target.
  (setopt substitute-highlight t)

  ;; Set this to t if you want to always treat the letter casing
  ;; literally.  Otherwise each command accepts a `C-u' prefix
  ;; argument to do this on-demand.
  (setq substitute-fixed-letter-case nil)

  ;; Produce a message after the substitution that reports on what
  ;; happened.  It is a single line, like "Substituted `TARGET' with
  ;; `SUBSTITUTE' N times across the buffer.
  (add-hook 'substitute-post-replace-hook #'substitute-report-operation)

  ;; The mnemonic for the prefix is that M-# (or M-S-3) is close to
  ;; M-% (or M-S-5).
  (let ((map global-map))
    (define-key map (kbd "M-# s") #'substitute-target-below-point) ; Forward motion like isearch (C-s)
    (define-key map (kbd "M-# r") #'substitute-target-above-point) ; Backward motion like isearch (C-r)
    (define-key map (kbd "M-# d") #'substitute-target-in-defun)    ; "defun" mnemonic
    (define-key map (kbd "M-# b") #'substitute-target-in-buffer))) ; "buffer" mnemonic

;;; Keymap for buffers (Emacs28)
(let ((map ctl-x-x-map))
  (define-key map "e" #'eval-buffer)
  (define-key map "f" #'follow-mode)  ; override `font-lock-update'
  (define-key map "r" #'rename-uniquely)
  (define-key map "l" #'visual-line-mode))

;;; Mouse wheel behaviour
(prot-emacs-builtin-package 'mouse
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
  (setq tooltip-delay 0.5
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
  (setq auto-revert-verbose t)
  (add-hook 'after-init-hook #'global-auto-revert-mode))

;;; Preserve contents of system clipboard
(setq save-interprogram-paste-before-kill t)

;;; Newline characters for file ending
(setq mode-require-final-newline 'visit-save)

;;; Go to last change
(prot-emacs-elpa-package 'goto-last-change
  (define-key prot-prefix-repeat-map (kbd "z") #'goto-last-change)
  (put #'goto-last-change 'repeat-map 'prot-prefix-repeat-map)

  (with-eval-after-load 'prot-prefix
    (transient-append-suffix 'prot-prefix '(0 -1 -1)
      '("z" "goto-last-change" goto-last-change))))

;;; Repeatable key chords (repeat-mode)
(prot-emacs-builtin-package 'repeat
  (:delay 2)
  (setq repeat-on-final-keystroke t
        repeat-exit-timeout 5
        repeat-exit-key "<escape>"
        repeat-keep-prefix nil
        repeat-check-key t
        repeat-echo-function 'ignore
        ;; Technically, this is not in repeal.el, though it is the
        ;; same idea.
        set-mark-command-repeat-pop t)
  (add-hook 'after-init-hook #'repeat-mode))

;;; TMR May Ring (tmr is used to set timers)
;; Read the manual: <https://protesilaos.com/emacs/tmr>.
(prot-emacs-elpa-package 'tmr
  (setq tmr-sound-file "/usr/share/sounds/freedesktop/stereo/alarm-clock-elapsed.oga"
        tmr-notification-urgency 'normal
        tmr-description-list 'tmr-description-history)

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

;;; Display current time
(prot-emacs-builtin-package 'time
  (setq display-time-format "%a %e %b, %H:%M ")
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

;;; World clock
  (setq display-time-world-list t)
  (setq zoneinfo-style-world-list
        '(("America/Los_Angeles" "Los Angeles")
          ("America/Chicago" "Chicago")
          ("Brazil/Acre" "Rio Branco")
          ("America/New_York" "New York")
          ("Brazil/East" "Brasília")
          ("Europe/Lisbon" "Lisbon")
          ("Europe/Brussels" "Brussels")
          ("Europe/Athens" "Athens")
          ("Asia/Tehran" "Tehran")
          ("Asia/Tbilisi" "Tbilisi")
          ("Asia/Yekaterinburg" "Yekaterinburg")
          ("Asia/Shanghai" "Shanghai")
          ("Asia/Tokyo" "Tokyo")
          ("Asia/Vladivostok" "Vladivostok")
          ("Australia/Sydney" "Sydney")
          ("Pacific/Auckland" "Auckland")))

  ;; All of the following variables are for Emacs 28
  (setq world-clock-list t)
  (setq world-clock-time-format "%R %z  %A %d %B")
  (setq world-clock-buffer-name "*world-clock*") ; Placement handled by `display-buffer-alist'
  (setq world-clock-timer-enable t)
  (setq world-clock-timer-second 60)

  (add-hook 'after-init-hook #'display-time-mode))

;;; Pass interface (password-store)
(prot-emacs-elpa-package 'password-store
  (setq password-store-time-before-clipboard-restore 30)
  ;; Mnemonic is the root of the "code" word (κώδικας).  But also to add
  ;; the password to the kill-ring.  Other options are already taken.
  (define-key global-map (kbd "C-c k") #'password-store-copy))

(prot-emacs-elpa-package 'pass)

;;; Emacs server (allow emacsclient to connect to running session)
;; The "server" is functionally like the daemon, except it is run by
;; the first Emacs frame we launch.  When we close that frame, the
;; server is terminated.  Whereas the daemon remains active even if
;; all Emacs frames are closed.
;;
;; I experimented with the daemon for a while.  Emacs would crash
;; whenever I would encounter an error in some Lisp evaluation.
;; Whereas the server works just fine when I need to connect to it via
;; the emacsclient.
(prot-emacs-builtin-package 'server
  (add-hook 'after-init-hook #'server-start))

;;; Emacs desktop (save state of various variables)
(prot-emacs-builtin-package 'desktop
  (setq desktop-auto-save-timeout 300)
  (setq desktop-path `(,user-emacs-directory))
  (setq desktop-base-file-name "desktop")
  (setq desktop-files-not-to-save ".*")
  (setq desktop-buffers-not-to-save ".*")
  (setq desktop-globals-to-clear nil)
  (setq desktop-load-locked-desktop t)
  (setq desktop-missing-file-warning nil)
  (setq desktop-restore-eager 0)
  (setq desktop-restore-frames nil)
  (setq desktop-save 'ask-if-new)
  (dolist (symbol '(kill-ring log-edit-comment-ring))
    (add-to-list 'desktop-globals-to-save symbol))

  (desktop-save-mode 1))

;;; Record cursor position
(prot-emacs-builtin-package 'saveplace
  (setq save-place-file (locate-user-emacs-file "saveplace"))
  (setq save-place-forget-unreadable-files t)
  (save-place-mode 1))

;;; Shell (M-x shell)
(prot-emacs-builtin-package 'shell
  (:delay 5)
  (setq shell-command-prompt-show-cwd t) ; Emacs 27.1
  (setq ansi-color-for-comint-mode t)
  (setenv "PAGER" "cat") ; solves issues, such as with 'git log' and the default 'less'
  (define-key global-map (kbd "<f1>") #'shell)) ; I don't use F1 for help commands

;;; Tools for manual pages (manpages)
(prot-emacs-builtin-package 'man
  (:delay 10)
  (setq Man-notify-method 'pushy) ; does not obey `display-buffer-alist'
  (let ((map Man-mode-map))
    (define-key map (kbd "i") #'Man-goto-section)
    (define-key map (kbd "g") #'Man-update-manpage)))

;;; Proced (process monitor, similar to `top')
(prot-emacs-builtin-package 'proced
  (:delay 10)
  (setq proced-auto-update-flag t)
  (setq proced-auto-update-interval 5)
  (setq proced-descend t)
  (setq proced-filter 'user))

(provide 'prot-emacs-essentials)
