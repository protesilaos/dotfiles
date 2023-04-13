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
(prot-emacs-package prot-common)

;;; Common custom functions (prot-simple.el)
(prot-emacs-package prot-simple
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
  (prot-emacs-keybind global-map
    "<insert>" nil
    "C-x C-z" nil
    "C-x C-c" nil ; avoid accidentally exiting Emacs
    "C-x C-c C-c" #'save-buffers-kill-emacs
    "C-h h" nil
    "M-`" nil
    "C-h ." #'prot-simple-describe-symbol ; overrides `display-local-help'
    "C-h K" #'describe-keymap ; overrides `Info-goto-emacs-key-command-node'
    "C-h c" #'describe-char ; overrides `describe-key-briefly'
    "C-c s" #'prot-simple-scratch-buffer
    ;; Commands for lines
    "M-o" #'delete-blank-lines   ; alias for C-x C-o
    "M-k" #'prot-simple-kill-line-backward
    "C-S-w" #'prot-simple-copy-line-or-region
    "C-S-y" #'prot-simple-yank-replace-line-or-region
    "M-SPC" #'cycle-spacing
    "C-S-n" #'prot-simple-multi-line-next
    "C-S-p" #'prot-simple-multi-line-prev
    "<C-return>" #'prot-simple-new-line-below
    "<C-S-return>" #'prot-simple-new-line-above
    ;; Commands for text insertion or manipulation
    "C-=" #'prot-simple-insert-date
    "C-<" #'prot-simple-escape-url-dwim
    "C-'" #'prot-simple-insert-pair
    "M-'" #'prot-simple-insert-pair
    "M-\\" #'prot-simple-delete-pair-dwim
    "M-z" #'zap-up-to-char ; NOT `zap-to-char'
    "M-Z" #'prot-simple-zap-to-char-backward
    "<C-M-backspace>" #'backward-kill-sexp
    "M-c" #'capitalize-dwim
    "M-l" #'downcase-dwim        ; "lower" case
    "M-u" #'upcase-dwim
    ;; Commands for object transposition
    "C-t" #'prot-simple-transpose-chars
    "C-x C-t" #'prot-simple-transpose-lines
    "C-S-t" #'prot-simple-transpose-paragraphs
    "C-x M-t" #'prot-simple-transpose-sentences
    "C-M-t" #'prot-simple-transpose-sexps
    "M-t" #'prot-simple-transpose-words
    ;; Commands for marking objects
    "M-@" #'prot-simple-mark-word       ; replaces `mark-word'
    "C-M-SPC" #'prot-simple-mark-construct-dwim
    "C-M-d" #'prot-simple-downward-list
    ;; Commands for paragraphs
    "M-Q" #'prot-simple-unfill-region-or-paragraph
    ;; Commands for windows and pages
    "C-x n k" #'prot-simple-delete-page-delimiters
    "C-x M-r" #'prot-simple-swap-window-buffers
    ;; Commands for buffers
    "M-=" #'count-words
    "<C-f2>" #'prot-simple-rename-file-and-buffer
    "C-x k" #'prot-simple-kill-buffer-current
    "C-x K" #'kill-buffer
    "M-s b" #'prot-simple-buffers-major-mode
    "M-s v" #'prot-simple-buffers-vc-root))

;;; Prefix keymap (prot-prefix.el)
(prot-emacs-package prot-prefix
  (define-key global-map (kbd "C-z") #'prot-prefix))

;;; Substitute
;; Another package of mine... Video demo:
;; <https://protesilaos.com/codelog/2023-01-16-emacs-substitute-package-demo/>.
(prot-emacs-package substitute
  (:install t)
  (:delay 2)
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
  (prot-emacs-keybind global-map
    "M-# s" #'substitute-target-below-point ; Forward motion like isearch (C-s)
    "M-# r" #'substitute-target-above-point ; Backward motion like isearch (C-r)
    "M-# d" #'substitute-target-in-defun    ; "defun" mnemonic
    "M-# b" #'substitute-target-in-buffer)) ; "buffer" mnemonic

;;; Keymap for buffers (Emacs28)
(let ((map ctl-x-x-map))
  (define-key map "e" #'eval-buffer)
  (define-key map "f" #'follow-mode)  ; override `font-lock-update'
  (define-key map "r" #'rename-uniquely)
  (define-key map "l" #'visual-line-mode))

;;; Mouse wheel behaviour
(prot-emacs-package mouse
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
(prot-emacs-package delsel
  (add-hook 'after-init-hook #'delete-selection-mode))

;;; Tooltips (tooltip-mode)
(prot-emacs-package tooltip
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
(prot-emacs-package autorevert
  (setq auto-revert-verbose t)
  (add-hook 'after-init-hook #'global-auto-revert-mode))

;;; Preserve contents of system clipboard
(setq save-interprogram-paste-before-kill t)

;;; Newline characters for file ending
(setq mode-require-final-newline 'visit-save)

;;; Go to last change
(prot-emacs-package goto-last-change
  (:install t)
  (define-key prot-prefix-repeat-map (kbd "z") #'goto-last-change)
  (put #'goto-last-change 'repeat-map 'prot-prefix-repeat-map)

  (with-eval-after-load 'prot-prefix
    (transient-append-suffix 'prot-prefix '(0 -1 -1)
      '("z" "goto-last-change" goto-last-change))))

;;; Repeatable key chords (repeat-mode)
(prot-emacs-package repeat
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
  (repeat-mode 1))

;;; TMR May Ring (tmr is used to set timers)
;; Read the manual: <https://protesilaos.com/emacs/tmr>.
(prot-emacs-package tmr
  (:install t)
  (:delay 5)
  (setq tmr-sound-file "/usr/share/sounds/freedesktop/stereo/alarm-clock-elapsed.oga"
        tmr-notification-urgency 'normal
        tmr-description-list 'tmr-description-history)

  (prot-emacs-keybind global-map
    "C-c t t" #'tmr
    "C-c t T" #'tmr-with-description
    "C-c t l" #'tmr-tabulated-view ; "list timers" mnemonic
    "C-c t c" #'tmr-clone
    "C-c t k" #'tmr-cancel
    "C-c t s" #'tmr-reschedule
    "C-c t e" #'tmr-edit-description
    "C-c t r" #'tmr-remove
    "C-c t R" #'tmr-remove-finished))

;;; Display current time
(prot-emacs-package time
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
(prot-emacs-package password-store
  (:install t)
  (:delay 2)
  (setq password-store-time-before-clipboard-restore 30)
  ;; Mnemonic is the root of the "code" word (κώδικας).  But also to add
  ;; the password to the kill-ring.  Other options are already taken.
  (define-key global-map (kbd "C-c k") #'password-store-copy))

(prot-emacs-package pass (:install t) (:delay 2))

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
(prot-emacs-package server
  (:delay 2)
  (server-start))

;;; Emacs desktop (save state of various variables)
(prot-emacs-package desktop
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
(prot-emacs-package saveplace
  (setq save-place-file (locate-user-emacs-file "saveplace"))
  (setq save-place-forget-unreadable-files t)
  (save-place-mode 1))

;;; Shell (M-x shell)
(prot-emacs-package shell
  (:delay 5)
  (setq shell-command-prompt-show-cwd t) ; Emacs 27.1
  (setq ansi-color-for-comint-mode t)
  (setenv "PAGER" "cat") ; solves issues, such as with 'git log' and the default 'less'
  (define-key global-map (kbd "<f1>") #'shell)) ; I don't use F1 for help commands

;;; Tools for manual pages (manpages)
(prot-emacs-package man
  (:delay 10)
  (setq Man-notify-method 'pushy) ; does not obey `display-buffer-alist'
  (let ((map Man-mode-map))
    (define-key map (kbd "i") #'Man-goto-section)
    (define-key map (kbd "g") #'Man-update-manpage)))

;;; Proced (process monitor, similar to `top')
(prot-emacs-package proced
  (:delay 10)
  (setq proced-auto-update-flag t)
  (setq proced-auto-update-interval 5)
  (setq proced-descend t)
  (setq proced-filter 'user))

(provide 'prot-emacs-essentials)
