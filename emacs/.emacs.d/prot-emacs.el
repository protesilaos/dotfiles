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
          ("_ underscores"         . (95 95))))   ; _ _
  (setq prot-simple-date-specifier "%F")
  (setq prot-simple-time-specifier "%R %z")
  (setq delete-pair-blink-delay 0.15) ; Emacs28 -- see `prot-simple-delete-pair-dwim'
  (setq prot-simple-scratch-buffer-default-mode 'markdown-mode)
  (setq help-window-select t)

  ;; ;; DEPRECATED 2021-10-15: set `help-window-select' to non-nil.
  ;; (setq prot-simple-focusable-help-commands
  ;;       '( describe-symbol
  ;;          describe-function
  ;;          describe-mode
  ;;          describe-variable
  ;;          describe-key
  ;;          describe-char
  ;;          what-cursor-position
  ;;          describe-package
  ;;          view-lossage))
  ;; (prot-simple-focus-help-buffers 1)

  ;; ;; NOTE 2022-01-20: The idea is good, but the implementation needs
  ;; ;; to be refined.
  ;; (prot-simple-rename-help-buffers 1)

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
    ;; NOTE 2022-05-01: I deprecated those commands.  I don't use them
    ;; and they need to be reworked.
    ;;
    ;; (define-key map (kbd "C-M-;") #'prot-simple-cite-region)
    ;; (define-key map (kbd "C-M-^") #'prot-simple-insert-undercaret)
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

;;; pulsar.el (highlight cursor position)
;; Read the pulsar manual: <https://protesilaos.com/emacs/pulsar>.
(prot-emacs-builtin-package 'pulsar
  (setq pulsar-pulse-functions
        ;; NOTE 2022-04-09: The commented out functions are from before
        ;; the introduction of `pulsar-pulse-on-window-change'.  Try
        ;; that instead.
        '(recenter-top-bottom
          move-to-window-line-top-bottom
          reposition-window
          bookmark-jump
          ;; other-window
          ;; delete-window
          ;; delete-other-windows
          forward-page
          backward-page
          scroll-up-command
          scroll-down-command
          ;; windmove-right
          ;; windmove-left
          ;; windmove-up
          ;; windmove-down
          ;; windmove-swap-states-right
          ;; windmove-swap-states-left
          ;; windmove-swap-states-up
          ;; windmove-swap-states-down
          ;; tab-new
          ;; tab-close
          ;; tab-next
          logos-forward-page-dwim
          logos-backward-page-dwim
          org-next-visible-heading
          org-previous-visible-heading
          org-forward-heading-same-level
          org-backward-heading-same-level
          outline-backward-same-level
          outline-forward-same-level
          outline-next-visible-heading
          outline-previous-visible-heading
          outline-up-heading))

  (setq pulsar-pulse-on-window-change t)
  (setq pulsar-pulse t)
  (setq pulsar-delay 0.055)
  (setq pulsar-iterations 10)
  (setq pulsar-face 'pulsar-blue)
  (setq pulsar-highlight-face 'pulsar-yellow)

  (pulsar-global-mode 1)

  ;; pulsar does not define any key bindings.  This is just my personal
  ;; preference.  Remember to read the manual on the matter.  Evaluate:
  ;;
  ;; (info "(elisp) Key Binding Conventions")
  (let ((map global-map))
    (define-key map (kbd "C-x l") #'pulsar-pulse-line) ; override `count-lines-page'
    (define-key map (kbd "C-x L") #'pulsar-highlight-dwim))) ; or use `pulsar-highlight-line'

;;; Make Custom UI code disposable
(prot-emacs-builtin-package 'cus-edit
  ;; Disable the damn thing
  (setq custom-file (make-temp-file "emacs-custom-")))

(prot-emacs-elpa-package 'exec-path-from-shell
  (setq exec-path-from-shell-variables
        '("PATH" "MANPATH" "SSH_AUTH_SOCK"))
  (exec-path-from-shell-initialize))

;;; Modus themes (my highly accessible themes)
(prot-emacs-builtin-package 'modus-themes
  ;; Add all your customizations prior to loading the themes
  ;;
  ;; NOTE: these are not my preferences!  I am always testing various
  ;; configurations.  Though I still like what I have here.
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-mixed-fonts t
        modus-themes-subtle-line-numbers nil
        modus-themes-intense-mouseovers nil
        modus-themes-deuteranopia t
        modus-themes-tabs-accented nil
        modus-themes-variable-pitch-ui t
        modus-themes-inhibit-reload t ; only applies to `customize-set-variable' and related

        modus-themes-fringes 'subtle ; {nil,'subtle,'intense}

        ;; Options for `modus-themes-lang-checkers' are either nil (the
        ;; default), or a list of properties that may include any of those
        ;; symbols: `straight-underline', `text-also', `background',
        ;; `intense' OR `faint'.
        modus-themes-lang-checkers '(text-also)

        ;; Options for `modus-themes-mode-line' are either nil, or a list
        ;; that can combine any of `3d' OR `moody', `borderless',
        ;; `accented', a natural number for extra padding (or a cons cell
        ;; of padding and NATNUM), and a floating point for the height of
        ;; the text relative to the base font size (or a cons cell of
        ;; height and FLOAT)
        modus-themes-mode-line nil

        ;; Options for `modus-themes-markup' are either nil, or a list
        ;; that can combine any of `bold', `italic', `background',
        ;; `intense'.
        modus-themes-markup nil

        ;; Options for `modus-themes-syntax' are either nil (the default),
        ;; or a list of properties that may include any of those symbols:
        ;; `faint', `yellow-comments', `green-strings', `alt-syntax'
        modus-themes-syntax nil

        ;; Options for `modus-themes-hl-line' are either nil (the default),
        ;; or a list of properties that may include any of those symbols:
        ;; `accented', `underline', `intense'
        modus-themes-hl-line nil

        ;; Options for `modus-themes-paren-match' are either nil (the
        ;; default), or a list of properties that may include any of those
        ;; symbols: `bold', `intense', `underline'
        modus-themes-paren-match '(intense)

        ;; Options for `modus-themes-links' are either nil (the default),
        ;; or a list of properties that may include any of those symbols:
        ;; `neutral-underline' OR `no-underline', `faint' OR `no-color',
        ;; `bold', `italic', `background'
        modus-themes-links nil

        ;; Options for `modus-themes-box-buttons' are either nil (the
        ;; default), or a list that can combine any of `flat',
        ;; `accented', `faint', `variable-pitch', `underline',
        ;; `all-buttons', the symbol of any font weight as listed in
        ;; `modus-themes-weights', and a floating point number
        ;; (e.g. 0.9) for the height of the button's text.
        modus-themes-box-buttons nil

        ;; Options for `modus-themes-prompts' are either nil (the
        ;; default), or a list of properties that may include any of those
        ;; symbols: `background', `bold', `gray', `intense', `italic'
        modus-themes-prompts nil

        ;; The `modus-themes-completions' is an alist that reads three
        ;; keys: `matches', `selection', `popup'.  Each accepts a nil
        ;; value (or empty list) or a list of properties that can include
        ;; any of the following (for WEIGHT read further below):
        ;;
        ;; `matches' - `background', `intense', `underline', `italic', WEIGHT
        ;; `selection' - `accented', `intense', `underline', `italic', `text-also', WEIGHT
        ;; `popup' - same as `selected'
        ;; `t' - applies to any key not explicitly referenced (check docs)
        ;;
        ;; WEIGHT is a symbol such as `semibold', `light', or anything
        ;; covered in `modus-themes-weights'.  Bold is used in the absence
        ;; of an explicit WEIGHT.
        modus-themes-completions
        '((matches . (extrabold background))
          (selection . (semibold accented))
          (popup . (extrabold)))

        modus-themes-mail-citations nil ; {nil,'intense,'faint,'monochrome}

        ;; Options for `modus-themes-region' are either nil (the default),
        ;; or a list of properties that may include any of those symbols:
        ;; `no-extend', `bg-only', `accented'
        modus-themes-region '(no-extend accented)

        ;; Options for `modus-themes-diffs': nil, 'desaturated, 'bg-only
        modus-themes-diffs 'desaturated

        modus-themes-org-blocks nil ; {nil,'gray-background,'tinted-background}

        modus-themes-org-agenda ; this is an alist: read the manual or its doc string
        '((header-block . (variable-pitch semilight 1.4))
          (header-date . (underline-today grayscale workaholic 1.2))
          (event . (accented italic varied))
          (scheduled . rainbow)
          (habit . simplified))

        modus-themes-headings ; this is an alist: read the manual or its doc string
        '((t . (variable-pitch extrabold)))

        ;; Sample for headings:

        ;; modus-themes-headings
        ;; '((1 . (variable-pitch light 1.6))
        ;;   (2 . (variable-pitch regular 1.4))
        ;;   (3 . (variable-pitch regular 1.3))
        ;;   (4 . (1.2))
        ;;   (5 . (1.1))
        ;;   (t . (monochrome 1.05)))
        )

  ;; Load the theme files before enabling a theme (else you get an error).
  (modus-themes-load-themes)

  ;; Custom faces (for demo purposes---check the themes' manual for more
  ;; advanced uses).
  (defun prot/modus-themes-custom-faces ()
    (modus-themes-with-colors
      (custom-set-faces
       ;; Here add all your face definitions.
       `(cursor ((,class :background ,red-intense))))))

  (add-hook 'modus-themes-after-load-theme-hook #'prot/modus-themes-custom-faces)

  ;; Enable the theme at startup.  This is done after loading the files.
  ;; You only need `modus-themes-load-operandi' for the light theme or
  ;; `modus-themes-load-vivendi' for the dark one.  What I have here is
  ;; a simple test to load a light/dark theme based on some general time
  ;; ranges (just accounting for the hour and without checking for the
  ;; actual sunrise/sunset times).  Plus we have `modus-themes-toggle'
  ;; to switch themes at will.
  (let ((time (string-to-number (format-time-string "%H"))))
    (if (and (> time 5) (< time 18))
        (modus-themes-load-operandi)
      (modus-themes-load-vivendi)))

  ;; Also check my package configurations for `prot-fonts' because I use
  ;; the `modus-themes-after-load-theme-hook' for some typeface-related
  ;; tweaks (as those are made at the "face" level).
  (define-key global-map (kbd "<f5>") #'modus-themes-toggle))

;;; Enhancements to hl-line-mode (lin.el)
(prot-emacs-builtin-package 'lin
  ;; You can use this to live update the face:
  ;;
  ;; (customize-set-variable 'lin-face 'lin-green)
  (setq lin-face 'lin-blue)
  (setq lin-mode-hooks
        '(bongo-mode-hook
          dired-mode-hook
          elfeed-search-mode-hook
          git-rebase-mode-hook
          ibuffer-mode-hook
          ilist-mode-hook
          ledger-report-mode-hook
          log-view-mode-hook
          magit-log-mode-hook
          mu4e-headers-mode
          notmuch-search-mode-hook
          notmuch-tree-mode-hook
          occur-mode-hook
          org-agenda-mode-hook
          tabulated-list-mode-hook))
  (lin-global-mode 1)) ; applies to all `lin-mode-hooks'

;;; Font configurations (fontaine.el)
(prot-emacs-builtin-package 'fontaine
  ;; This is defined in Emacs C code: it belongs to font settings.
  (setq x-underline-at-descent-line t)

  ;; And this is for Emacs28.
  (setq-default text-scale-remap-header-line t)

  (setq fontaine-latest-state-file (locate-user-emacs-file "fontaine-latest-state.eld"))
  ;; Iosevka Comfy is my highly customised build of Iosevka with
  ;; monospaced and duospaced (quasi-proportional) variants as well as
  ;; support or no support for ligatures:
  ;; <https://git.sr.ht/~protesilaos/iosevka-comfy>.
  ;;
  ;; Iosevka Comfy            == monospaced, supports ligatures
  ;; Iosevka Comfy Fixed      == monospaced, no ligatures
  ;; Iosevka Comfy Duo        == quasi-proportional, supports ligatures
  ;; Iosevka Comfy Wide       == like Iosevka Comfy, but wider
  ;; Iosevka Comfy Wide Fixed == like Iosevka Comfy Fixed, but wider
  (setq fontaine-presets
        '((tiny
           :default-family "Iosevka Comfy Wide Fixed"
           :default-height 70)
          (small
           :default-family "Iosevka Comfy Fixed"
           :default-height 90)
          (regular
           :default-height 100)
          (medium
           :default-height 110)
          (large
           :default-weight semilight
           :default-height 140
           :bold-weight extrabold)
          (presentation
           :default-weight semilight
           :default-height 170
           :bold-weight extrabold)
          (t
           ;; I keep all properties for didactic purposes, but most can be
           ;; omitted.  See the fontaine manual for the technicalities:
           ;; <https://protesilaos.com/emacs/fontaine>.
           :default-family "Iosevka Comfy"
           :default-weight regular
           :default-height 100
           :fixed-pitch-family nil ; falls back to :default-family
           :fixed-pitch-weight nil ; falls back to :default-weight
           :fixed-pitch-height 1.0
           :variable-pitch-family "Iosevka Comfy Duo"
           :variable-pitch-weight nil
           :variable-pitch-height 1.0
           :bold-family nil ; use whatever the underlying face has
           :bold-weight bold
           :italic-family nil
           :italic-slant italic
           :line-spacing nil)))

  ;; Set last preset or fall back to desired style from `fontaine-presets'.
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))

  ;; The other side of `fontaine-restore-latest-preset'.
  (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset)

  (define-key global-map (kbd "C-c f") #'fontaine-set-preset)
  (define-key global-map (kbd "C-c F") #'fontaine-set-face-font))

;;; Repeatable key chords (repeat-mode)
(prot-emacs-builtin-package 'repeat
  (setq repeat-on-final-keystroke t)
  (setq set-mark-command-repeat-pop t)

  (repeat-mode 1))

;;; Handle performance for very long lines (so-long.el)
(prot-emacs-builtin-package 'so-long
  (global-so-long-mode 1))

  ;;; Orderless completion style (and prot-orderless.el)
  (prot-emacs-elpa-package 'orderless
    (setq orderless-component-separator " +")
    ;; NOTE 2022-02-06: I made some major changes and this list may need
    ;; to be refined further.  Remember to check my `completion-styles'
    ;; and the `completion-category-overrides'.
    (setq orderless-matching-styles
          '( orderless-prefixes orderless-strict-leading-initialism
             orderless-flex orderless-regexp))
    (setq orderless-style-dispatchers
          '(prot-orderless-literal-dispatcher
            prot-orderless-initialism-dispatcher
            prot-orderless-flex-dispatcher))

    ;; SPC should never complete: use it for `orderless' groups.
    ;; The `?' is a regexp construct.
    (let ((map minibuffer-local-completion-map))
      (define-key map (kbd "SPC") nil)
      (define-key map (kbd "?") nil)))

  (prot-emacs-builtin-package 'prot-orderless)

;;; Completion annotations (marginalia)
(prot-emacs-elpa-package 'marginalia
  (setq marginalia-max-relative-age 0)  ; time is absolute here!
  (marginalia-mode 1))

;;; Minibuffer configurations and Vertico
(prot-emacs-builtin-package 'minibuffer
  (setq completion-styles '(basic orderless)) ; also see `completion-category-overrides'
  (setq completion-category-defaults nil)

  ;; A list of known completion categories:
  ;;
  ;; - `bookmark'
  ;; - `buffer'
  ;; - `charset'
  ;; - `coding-system'
  ;; - `color'
  ;; - `command' (e.g. `M-x')
  ;; - `customize-group'
  ;; - `environment-variable'
  ;; - `expression'
  ;; - `face'
  ;; - `file'
  ;; - `function' (the `describe-function' command bound to `C-h f')
  ;; - `info-menu'
  ;; - `imenu'
  ;; - `input-method'
  ;; - `kill-ring'
  ;; - `library'
  ;; - `minor-mode'
  ;; - `multi-category'
  ;; - `package'
  ;; - `project-file'
  ;; - `symbol' (the `describe-symbol' command bound to `C-h o')
  ;; - `theme'
  ;; - `unicode-name' (the `insert-char' command bound to `C-x 8 RET')
  ;; - `variable' (the `describe-variable' command bound to `C-h v')
  ;;
  ;; From the `consult' package:
  ;;
  ;; - `consult-grep'
  ;; - `consult-isearch'
  ;; - `consult-kmacro'
  ;; - `consult-location'
  ;;
  ;; From the `embark' package:
  ;;
  ;; - `embark-keybinding'
  ;;
  (setq completion-category-overrides
        ;; NOTE 2021-10-25: I am adding `basic' because it works better as a
        ;; default for some contexts.  Read:
        ;; <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=50387>.
        ;;
        ;; `partial-completion' is a killer app for files, because it
        ;; can expand ~/.l/s/fo to ~/.local/share/fonts.
        ;;
        ;; If `basic' cannot match my current input, Emacs tries the
        ;; next completion style in the given order.  In other words,
        ;; `orderless' kicks in as soon as I input a space or one of its
        ;; style dispatcher characters.
        '((file (styles . (basic partial-completion orderless)))
          (project-file (styles . (basic substring partial-completion orderless)))
          (imenu (styles . (basic substring orderless)))
          (kill-ring (styles . (basic substring orderless)))
          (consult-location (styles . (basic substring orderless)))))

  (setq completion-ignore-case t)
  (setq read-buffer-completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)
  (setq-default case-fold-search t)   ; For general regexp

  (setq enable-recursive-minibuffers t)
  ;; Allow Emacs to resize mini windows, otherwise this does not work:
  ;;   (setq org-use-fast-todo-selection 'expert)
  (setq resize-mini-windows t)
  (setq minibuffer-eldef-shorten-default t)

  (setq read-answer-short t) ; also check `use-short-answers' for Emacs28
  (setq echo-keystrokes 0.25)
  (setq kill-ring-max 60)               ; Keep it small

  ;; Do not allow the cursor to move inside the minibuffer prompt.  I
  ;; got this from the documentation of Daniel Mendler's Vertico
  ;; package: <https://github.com/minad/vertico>.
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))

  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Also adapted from Vertico.
  (defun crm-indicator (args)
    "Add prompt indicator to `completing-read-multiple' filter ARGS."
    ;; The `error' face just makes the text red.
    (cons (concat (propertize "[CRM] " 'face 'error) (car args)) (cdr args)))

  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  (file-name-shadow-mode 1)
  (minibuffer-depth-indicate-mode 1)
  (minibuffer-electric-default-mode 1))

(prot-emacs-builtin-package 'savehist
  (setq savehist-file (locate-user-emacs-file "savehist"))
  (setq history-length 10000)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t)
  (add-hook 'after-init-hook #'savehist-mode))

(prot-emacs-elpa-package 'vertico
  ;; Those are the default values, but check the user option
  ;; `vertico-multiform-categories' for per-category tweaks.
  (setq vertico-scroll-margin 0)
  (setq vertico-count 10)
  (setq vertico-resize nil)
  (setq vertico-cycle t)

  (vertico-mode 1)

  (let ((map vertico-map))
    (define-key map (kbd "M-,") #'vertico-quick-insert)
    (define-key map (kbd "M-.") #'vertico-quick-exit))

  ;; This works with `file-name-shadow-mode'.  When you are in a
  ;; sub-directory and use, say, `find-file' to go to your home '~/' or
  ;; root '/' directory, Vertico will clear the old path to keep only
  ;; your current input.
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))

;;; Enhanced minibuffer commands (consult.el)
(prot-emacs-elpa-package 'consult
  (setq consult-line-numbers-widen t)
  ;; (setq completion-in-region-function #'consult-completion-in-region)
  (setq consult-async-min-input 3)
  (setq consult-async-input-debounce 0.5)
  (setq consult-async-input-throttle 0.8)
  (setq consult-narrow-key ">")
  (setq consult-imenu-config
        '((emacs-lisp-mode :toplevel "Functions"
                           :types ((?f "Functions" font-lock-function-name-face)
                                   (?m "Macros"    font-lock-keyword-face)
                                   (?p "Packages"  font-lock-constant-face)
                                   (?t "Types"     font-lock-type-face)
                                   (?v "Variables" font-lock-variable-name-face)))))
  ;; Search C-h f for more "bookmark jump" handlers.
  (setq consult-bookmark-narrow
        `((?d "Docview" ,#'doc-view-bookmark-jump)
          (?e "Eshell" ,#'eshell-bookmark-jump)
          (?f "File" ,#'bookmark-default-handler)
          (?h "Help" ,#'help-bookmark-jump)
          (?i "Info" ,#'Info-bookmark-jump)
          (?m "Man" ,#'Man-bookmark-jump)
          ;; (?p "PDF" ,#'pdf-view-bookmark-jump)
          (?v "VC Dir" ,#'vc-dir-bookmark-jump)
          (?w "EWW" ,#'prot-eww-bookmark-jump)))
  (setq register-preview-delay 0.8
        register-preview-function #'consult-register-format)
  (setq consult-find-args "find . -not ( -wholename */.* -prune )")
  (setq consult-preview-key 'any)

  (add-hook 'completion-list-mode-hook #'consult-preview-at-point-mode)

  (require 'consult-imenu) ; the `imenu' extension is in its own file

  (let ((map global-map))
    (define-key map (kbd "C-x r b") #'consult-bookmark) ; override `bookmark-jump'
    (define-key map (kbd "C-x M-:") #'consult-complex-command)
    (define-key map (kbd "C-x M-m") #'consult-minor-mode-menu)
    (define-key map (kbd "C-x M-k") #'consult-kmacro)
    (define-key map [remap goto-line] #'consult-goto-line)
    (define-key map (kbd "M-K") #'consult-keep-lines) ; M-S-k is similar to M-S-5 (M-%)
    (define-key map (kbd "M-F") #'consult-focus-lines) ; same principle
    (define-key map (kbd "M-s M-b") #'consult-buffer)
    (define-key map (kbd "M-s M-f") #'consult-find)
    (define-key map (kbd "M-s M-g") #'consult-grep)
    (define-key map (kbd "M-s M-h") #'consult-history)
    (define-key map (kbd "M-s M-i") #'consult-imenu)
    (define-key map (kbd "M-s M-l") #'consult-line)
    (define-key map (kbd "M-s M-m") #'consult-mark)
    (define-key map (kbd "M-s M-s") #'consult-outline)
    (define-key map (kbd "M-s M-y") #'consult-yank-pop)
    (define-key map (kbd "C-x r r") #'consult-register)) ; Use the register's prefix
  (define-key consult-narrow-map (kbd "?") #'consult-narrow-help)

  ;; see my `pulsar' package, which is declared further above:
  ;; <https://protesilaos.com/emacs/pulsar>
  (setq consult-after-jump-hook nil) ; reset it to avoid conflicts with my function
  (dolist (fn '(pulsar-recenter-top pulsar-reveal-entry))
    (add-hook 'consult-after-jump-hook fn)))

;;; Switch to directories (consult-dir.el)
(prot-emacs-elpa-package 'consult-dir
  (setq consult-dir-shadow-filenames nil)
  (setq consult-dir-sources '( consult-dir--source-bookmark
                               consult-dir--source-default
                               consult-dir--source-project
                               consult-dir--source-recentf))

  ;; Overrides `list-directory' in the `global-map', though I never used
  ;; that anyway.
  (dolist (map (list global-map minibuffer-local-filename-completion-map))
    (define-key map (kbd "C-x C-d") #'consult-dir)))

;;; Extended minibuffer actions and more (embark.el and prot-embark.el)
(prot-emacs-elpa-package 'embark
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; (setq prefix-help-command #'describe-prefix-bindings) ; the default of the above
  (setq embark-quit-after-action t)     ; XXX: Read the doc string!
  (setq embark-cycle-key (kbd "C-."))   ; see the `embark-act' key
  (setq embark-confirm-act-all nil)
  (setq embark-indicators
        '(embark-mixed-indicator
          embark-highlight-indicator))
  ;; NOTE 2021-07-31: The mixed indicator starts out with a minimal view
  ;; and then pops up the verbose buffer, so those variables matter.
  (setq embark-verbose-indicator-excluded-actions
        '("\\`customize-" "\\(local\\|global\\)-set-key"
          set-variable embark-cycle embark-keymap-help embark-isearch))
  (setq embark-verbose-indicator-buffer-sections
        `(target "\n" shadowed-targets " " cycle "\n" bindings))
  (setq embark-mixed-indicator-both nil)
  (setq embark-mixed-indicator-delay 1.2)
  ;;  NOTE 2021-07-28: This is used when `embark-indicator' is set to
  ;;  `embark-mixed-indicator' or `embark-verbose-indicator'.  We can
  ;;  specify the window parameters here, but I prefer to do that in my
  ;;  `display-buffer-alist' (search this document) because it is easier
  ;;  to keep track of all my rules in one place.
  (setq embark-verbose-indicator-display-action nil)

  (define-key global-map (kbd "C-,") #'embark-act)
  (define-key embark-collect-mode-map (kbd "C-,") #'embark-act)
  (let ((map minibuffer-local-completion-map))
    (define-key map (kbd "C-,") #'embark-act)
    (define-key map (kbd "C->") #'embark-become))
  (let ((map embark-region-map))
    (define-key map (kbd "a") #'align-regexp)
    (define-key map (kbd "i") #'epa-import-keys-region)
    (define-key map (kbd "r") #'repunctuate-sentences) ; overrides `rot13-region'
    (define-key map (kbd "s") #'sort-lines)
    (define-key map (kbd "u") #'untabify))
  (let ((map embark-symbol-map))
    (define-key map (kbd ".") #'embark-find-definition)
    (define-key map (kbd "k") #'describe-keymap)))

;; Needed for correct exporting while using Embark with Consult
;; commands.
(prot-emacs-elpa-package 'embark-consult)

(prot-emacs-builtin-package 'prot-embark
  (prot-embark-keymaps 1)
  (prot-embark-setup-packages 1))

;;; Projects (project.el and prot-project.el)
(prot-emacs-builtin-package 'project
  ;; ;; Use this for Emacs 27 (I am on 28)
  ;; (add-to-list 'prot-emacs-ensure-install 'project)
  (setq project-switch-commands
        '((?f "File" project-find-file)
          (?s "Subdir" prot-project-find-subdir)
          (?g "Grep" project-find-regexp)
          (?d "Dired" project-dired)
          (?b "Buffer" project-switch-to-buffer)
          (?q "Query replace" project-query-replace-regexp)
          (?t "Tag switch" prot-project-retrieve-tag)
          (?m "Magit" prot-project-magit-status)
          (?v "VC dir" project-vc-dir)
          (?l "Log VC" prot-project-commit-log)
          (?e "Eshell" project-eshell)))
  (define-key global-map (kbd "C-x p q") #'project-query-replace-regexp)) ; C-x p is `project-prefix-map'

(prot-emacs-builtin-package 'prot-project
  (setq prot-project-project-roots '("~/Git/Projects/" "~/Git/build/"))
  (setq prot-project-commit-log-limit 25)
  (setq prot-project-large-file-lines 1000)
  (let ((map global-map))
    (define-key map (kbd "C-x p <delete>") #'prot-project-remove-project)
    (define-key map (kbd "C-x p l") #'prot-project-commit-log)
    (define-key map (kbd "C-x p m") #'prot-project-magit-status)
    (define-key map (kbd "C-x p s") #'prot-project-find-subdir)
    (define-key map (kbd "C-x p t") #'prot-project-retrieve-tag)))

;;; Extra features for projects (project-x.el)
;; Project repo: <https://github.com/karthink/project-x>.  This is one
;; of the packages I handle manually via git, at least until it becomes
;; available through an ELPA.
;;
;; `prot-emacs-manual-package' is defined in my init.el
(prot-emacs-manual-package 'project-x
  (setq project-x-window-list-file (locate-user-emacs-file "project-x-window-list"))
  (setq project-x-local-identifier ".project")
  (project-x-mode 1))

;;; Completion for recent files and directories (prot-recentf.el)
(prot-emacs-builtin-package 'recentf
  (setq recentf-save-file (locate-user-emacs-file "recentf"))
  (setq recentf-max-saved-items 200)
  (setq recentf-exclude '(".gz" ".xz" ".zip" "/elpa/" "/ssh:" "/sudo:"))
  (add-hook 'after-init-hook #'recentf-mode))

(prot-emacs-builtin-package 'prot-recentf
  (add-to-list 'recentf-keep 'prot-recentf-keep-predicate)
  (let ((map global-map))
    (define-key map (kbd "C-x C-r") #'prot-recentf-recent-files-or-dirs)))

;;; Corfu (in-buffer completion popup)
(prot-emacs-elpa-package 'corfu
  ;; (dolist (mode '( message-mode-hook text-mode-hook prog-mode-hook
  ;;                  shell-mode-hook eshell-mode-hook))
  ;;   (add-hook mode #'corfu-mode))
  (global-corfu-mode 1)
  (define-key corfu-map (kbd "<tab>") #'corfu-complete)

  ;; Adapted from Corfu's manual.
  (defun contrib/corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico is not active.
Useful for prompts such as `eval-expression' and `shell-command'."
    (unless (bound-and-true-p vertico--input)
      (corfu-mode 1)))

  (add-hook 'minibuffer-setup-hook #'contrib/corfu-enable-always-in-minibuffer 1))

;;; CAPE (extra completion-at-point backends)
(prot-emacs-elpa-package 'cape
  (setq cape-dabbrev-min-length 3)
  (dolist (backend '( cape-symbol cape-keyword cape-file cape-dabbrev))
    (add-to-list 'completion-at-point-functions backend)))

;;; Template-based in-buffer completion (tempel.el)
(prot-emacs-elpa-package 'tempel

  ;; Setup completion at point
  (defun contrib/tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook 'contrib/tempel-setup-capf))

  (let ((map global-map))
    (define-key map (kbd "M-+") #'tempel-complete) ; Alternative: `tempel-expand'
    (define-key map (kbd "M-*") #'tempel-insert))
  (let ((map tempel-map))
    (define-key map (kbd "RET") #'tempel-done)
    (define-key map (kbd "C-p") #'tempel-previous)
    (define-key map (kbd "C-n") #'tempel-next)))

;;; Enhance command-line completion (pcmpl-args)
(prot-emacs-elpa-package 'pcmpl-args)

;;; Dabbrev (dynamic word completion)
(prot-emacs-builtin-package 'dabbrev
  (setq dabbrev-abbrev-char-regexp "\\sw\\|\\s_")
  (setq dabbrev-abbrev-skip-leading-regexp "[$*/=~']")
  (setq dabbrev-backward-only nil)
  (setq dabbrev-case-distinction 'case-replace)
  (setq dabbrev-case-fold-search nil)
  (setq dabbrev-case-replace 'case-replace)
  (setq dabbrev-check-other-buffers t)
  (setq dabbrev-eliminate-newlines t)
  (setq dabbrev-upcase-means-case-search t)
  (let ((map global-map))
    (define-key map (kbd "M-/") #'dabbrev-expand)
    (define-key map (kbd "C-x M-/") #'dabbrev-completion)))

;;; Abbreviations or Abbrevs
(prot-emacs-builtin-package 'abbrev
  (setq abbrev-file-name (locate-user-emacs-file "abbrevs"))
  (setq only-global-abbrevs nil)

  (let ((table global-abbrev-table))
    (define-abbrev table "meweb" "https://protesilaos.com")
    (define-abbrev table "megit" "https://git.sr.ht/~protesilaos")
    ;; to speed up technical writing about Emacs in Greek (I wrote the
    ;; Emacs TUTORIAL in Greek)
    (define-abbrev table "εμαψσ" "Emacs")
    (define-abbrev table "βθφφερ" "αποσβεστήρας")
    (define-abbrev table "μινιβθφφερ" "μικροαποσβεστήρας"))

  (let ((table text-mode-abbrev-table))
    (define-abbrev table "latex" "LaTeX")
    (define-abbrev table "github" "GitHub")
    (define-abbrev table "gitlab" "GitLab")
    (define-abbrev table "sourcehut" "SourceHut")
    (define-abbrev table "libreplanet" "LibrePlanet")
    (define-abbrev table "emacsconf" "EmacsConf")
    (define-abbrev table "auctex" "AUCTeX")
    (define-abbrev table "Emacs27" "Emacs 27")
    (define-abbrev table "Emacs28" "Emacs 28")
    (define-abbrev table "Emacs29" "Emacs 29")
    (define-abbrev table "asciidoc" "AsciiDoc"))

  (with-eval-after-load 'message
    (let ((table message-mode-abbrev-table)
          (name "Protesilaos (or simply \"Prot\")"))
      (define-abbrev table "bestregards" (format "Best regards,\n%s" name))
      (define-abbrev table "allthebest" (format "All the best,\n%s" name))
      (define-abbrev table "abest" "All the best,\nProt")
      (define-abbrev table "bregards" "Best regards,\nProt")))

  (let ((map global-map))
    (define-key map (kbd "C-x a e") #'expand-abbrev) ; default, just here for visibility
    (define-key map (kbd "C-x a u") #'unexpand-abbrev))

  ;; message-mode derives from text-mode, so we don't need a separate
  ;; hook for it.
  (dolist (hook '(text-mode-hook git-commit-mode-hook))
    (add-hook hook #'abbrev-mode)))

;;; Isearch, occur, grep, and extras (prot-search.el)
(prot-emacs-builtin-package 'isearch
  (setq search-highlight t)
  (setq search-whitespace-regexp ".*?")
  (setq isearch-lax-whitespace t)
  (setq isearch-regexp-lax-whitespace nil)
  (setq isearch-lazy-highlight t)
  ;; All of the following variables were introduced in Emacs 27.1.
  (setq isearch-lazy-count t)
  (setq lazy-count-prefix-format nil)
  (setq lazy-count-suffix-format " (%s/%s)")
  (setq isearch-yank-on-move 'shift)
  (setq isearch-allow-scroll 'unlimited)
  ;; These variables are from Emacs 28
  (setq isearch-repeat-on-direction-change t)
  (setq lazy-highlight-initial-delay 0.5)
  (setq lazy-highlight-no-delay-length 3)
  (setq isearch-wrap-pause t)

  (define-key minibuffer-local-isearch-map (kbd "M-/") #'isearch-complete-edit)
  (let ((map isearch-mode-map))
    (define-key map (kbd "C-g") #'isearch-cancel) ; instead of `isearch-abort'
    (define-key map (kbd "M-/") #'isearch-complete)))

(prot-emacs-builtin-package 'replace
  (setq list-matching-lines-jump-to-current-line nil)
  (add-hook 'occur-mode-hook #'hl-line-mode)
  (add-hook 'occur-mode-hook #'prot-common-truncate-lines-silently) ; from `prot-common.el'
  (define-key occur-mode-map (kbd "t") #'toggle-truncate-lines))

(prot-emacs-builtin-package 'grep)

(prot-emacs-builtin-package 'prot-search
  (setq prot-search-outline-regexp-alist
        '((emacs-lisp-mode . "^\\((\\|;;;+ \\)")
          (org-mode . "^\\(\\*+ +\\|#\\+[Tt][Ii][Tt][Ll][Ee]:\\)")
          (conf-toml-mode . "^\\[")
          (markdown-mode . "^#+ +")))
  (setq prot-search-todo-keywords
        (concat "TODO\\|FIXME\\|NOTE\\|REVIEW\\|XXX\\|KLUDGE"
                "\\|HACK\\|WARN\\|WARNING\\|DEPRECATED\\|BUG"))

  (let ((map global-map))
    (define-key map (kbd "M-s %") #'prot-search-isearch-replace-symbol)
    (define-key map (kbd "M-s M-%") #'prot-search-replace-markup) ; see `prot-search-markup-replacements'
    (define-key map (kbd "M-s M-<") #'prot-search-isearch-beginning-of-buffer)
    (define-key map (kbd "M-s M->") #'prot-search-isearch-end-of-buffer)
    (define-key map (kbd "M-s g") #'prot-search-grep)
    (define-key map (kbd "M-s u") #'prot-search-occur-urls)
    (define-key map (kbd "M-s t") #'prot-search-occur-todo-keywords)
    (define-key map (kbd "M-s M-t") #'prot-search-grep-todo-keywords) ; With C-u it runs `prot-search-git-grep-todo-keywords'
    (define-key map (kbd "M-s M-o") #'prot-search-occur-outline)
    (define-key map (kbd "M-s M-u") #'prot-search-occur-browse-url))
  (let ((map isearch-mode-map))
    (define-key map (kbd "<up>") #'prot-search-isearch-repeat-backward)
    (define-key map (kbd "<down>") #'prot-search-isearch-repeat-forward)
    (define-key map (kbd "<backspace>") #'prot-search-isearch-abort-dwim)
    (define-key map (kbd "<C-return>") #'prot-search-isearch-other-end)))

;;; Test regular expressions (re-builder)
(prot-emacs-builtin-package 're-builder
  (setq reb-re-syntax 'read))

;;; wgrep (writable grep)
(prot-emacs-elpa-package 'wgrep
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-change-readonly-file t)
  (let ((map grep-mode-map))
    (define-key map (kbd "e") #'wgrep-change-to-wgrep-mode)
    (define-key map (kbd "C-x C-q") #'wgrep-change-to-wgrep-mode)
    (define-key map (kbd "C-c C-c") #'wgrep-finish-edit)))

;;; Cross-references (xref.el)
(prot-emacs-builtin-package 'xref
  ;; All those have been changed for Emacs 28
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read) ; for M-.
  (setq xref-show-xrefs-function #'xref-show-definitions-buffer) ; for grep and the like
  (setq xref-file-name-display 'project-relative)
  (setq xref-search-program 'grep))

;;; Dired file manager (and prot-dired.el extras)
(prot-emacs-builtin-package 'dired
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches
        "-AGFhlv --group-directories-first --time-style=long-iso")
  (setq dired-dwim-target t)
  (setq dired-auto-revert-buffer #'dired-directory-changed-p) ; also see `dired-do-revert-buffer'
  (setq dired-make-directory-clickable t) ; Emacs 29.1
  (setq dired-free-space nil) ; Emacs 29.1
  (setq dired-mouse-drag-files t) ; Emacs 29.1

  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  (add-hook 'dired-mode-hook #'hl-line-mode))

(prot-emacs-builtin-package 'dired-aux
  (setq dired-isearch-filenames 'dwim)
  ;; The following variables were introduced in Emacs 27.1
  (setq dired-create-destination-dirs 'ask)
  (setq dired-vc-rename-file t)
  ;; And this is for Emacs 28
  (setq dired-do-revert-buffer (lambda (dir) (not (file-remote-p dir))))

  (let ((map dired-mode-map))
    (define-key map (kbd "C-+") #'dired-create-empty-file)
    (define-key map (kbd "M-s f") #'nil)
    (define-key map (kbd "C-x v v") #'dired-vc-next-action))) ; Emacs 28

;; ;; NOTE 2021-05-10: I do not use `find-dired' and related commands
;; ;; because there are other tools that offer a better interface, such
;; ;; as `consult-find', `consult-grep', `project-find-file',
;; ;; `project-find-regexp', `prot-vc-git-grep'.
;; (prot-emacs-builtin-package 'find-dired
;;   (setq find-ls-option
;;         '("-ls" . "-AGFhlv --group-directories-first --time-style=long-iso"))
;;   (setq find-name-arg "-iname"))

(prot-emacs-builtin-package 'dired-x
  (setq dired-clean-up-buffers-too t)
  (setq dired-clean-confirm-killing-deleted-buffers t)
  (setq dired-x-hands-off-my-keys t)    ; easier to show the keys I use
  (setq dired-bind-man nil)
  (setq dired-bind-info nil)
  (define-key dired-mode-map (kbd "I") #'dired-info))

(prot-emacs-builtin-package 'prot-dired
  (setq prot-dired-image-viewers '("feh" "sxiv"))
  (setq prot-dired-media-players '("mpv" "vlc"))
  (setq prot-dired-media-extensions
        "\\.\\(mp[34]\\|ogg\\|flac\\|webm\\|mkv\\)")
  (setq prot-dired-image-extensions
        "\\.\\(png\\|jpe?g\\|tiff\\)")
  (setq dired-guess-shell-alist-user ; those are the defaults for ! and & in Dired
        `((,prot-dired-image-extensions (prot-dired-image-viewer))
          (,prot-dired-media-extensions (prot-dired-media-player))))

  (add-hook 'dired-mode-hook #'prot-dired-setup-imenu)

  (let ((map dired-mode-map))
    (define-key map (kbd "i") #'prot-dired-insert-subdir) ; override `dired-maybe-insert-subdir'
    (define-key map (kbd "/") #'prot-dired-limit-regexp)
    (define-key map (kbd "C-c C-l") #'prot-dired-limit-regexp)
    (define-key map (kbd "M-n") #'prot-dired-subdirectory-next)
    (define-key map (kbd "C-c C-n") #'prot-dired-subdirectory-next)
    (define-key map (kbd "M-p") #'prot-dired-subdirectory-previous)
    (define-key map (kbd "C-c C-p") #'prot-dired-subdirectory-previous)
    (define-key map (kbd "M-s G") #'prot-dired-grep-marked-files))) ; M-s g is `prot-search-grep'

(prot-emacs-elpa-package 'dired-subtree
  (setq dired-subtree-use-backgrounds nil)
  (let ((map dired-mode-map))
    (define-key map (kbd "<tab>") #'dired-subtree-toggle)
    (define-key map (kbd "<backtab>") #'dired-subtree-remove))) ; S-TAB

(prot-emacs-builtin-package 'wdired
  (setq wdired-allow-to-change-permissions t)
  (setq wdired-create-parent-directories t))

(prot-emacs-builtin-package 'image-dired
  (setq image-dired-external-viewer "xdg-open")
  (setq image-dired-thumb-size 80)
  (setq image-dired-thumb-margin 2)
  (setq image-dired-thumb-relief 0)
  (setq image-dired-thumbs-per-row 4)
  (define-key image-dired-thumbnail-mode-map
    (kbd "<return>") #'image-dired-thumbnail-display-external))

;;; dired-like mode for the trash (trashed.el)
(prot-emacs-elpa-package 'trashed
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))

;;; Keymap for buffers (Emacs28)
(let ((map ctl-x-x-map))              ; Emacs 28
  (define-key map "e" #'eval-buffer)
  (define-key map "f" #'follow-mode)  ; override `font-lock-update'
  (define-key map "r" #'rename-uniquely))

(with-eval-after-load 'org
  (define-key ctl-x-x-map "i" #'prot-org-id-headlines)
  (define-key ctl-x-x-map "h" #'prot-org-ox-html))

;;; Unique names for buffers
(prot-emacs-builtin-package 'uniquify
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-strip-common-suffix t)
  (setq uniquify-after-kill-buffer-p t))

;;; Ibuffer and extras (dired-like buffer list manager)
(prot-emacs-builtin-package 'ibuffer
  (setq ibuffer-expert t)
  (setq ibuffer-display-summary nil)
  (setq ibuffer-use-other-window nil)
  (setq ibuffer-show-empty-filter-groups nil)
  (setq ibuffer-movement-cycle nil)
  (setq ibuffer-default-sorting-mode 'filename/process)
  (setq ibuffer-use-header-line t)
  (setq ibuffer-default-shrink-to-minimum-size nil)
  (setq ibuffer-formats
        '((mark modified read-only locked " "
                (name 40 40 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " " filename-and-process)
          (mark " "
                (name 16 -1)
                " " filename)))
  (setq ibuffer-saved-filter-groups nil)
  (setq ibuffer-old-time 48)
  (add-hook 'ibuffer-mode-hook #'hl-line-mode)
  (define-key global-map (kbd "C-x C-b") #'ibuffer)
  (let ((map ibuffer-mode-map))
    (define-key map (kbd "* f") #'ibuffer-mark-by-file-name-regexp)
    (define-key map (kbd "* g") #'ibuffer-mark-by-content-regexp) ; "g" is for "grep"
    (define-key map (kbd "* n") #'ibuffer-mark-by-name-regexp)
    (define-key map (kbd "s n") #'ibuffer-do-sort-by-alphabetic)  ; "sort name" mnemonic
    (define-key map (kbd "/ g") #'ibuffer-filter-by-content)))

;;; Window rules and basic tweaks (window.el)
(prot-emacs-builtin-package 'window
  (setq display-buffer-alist
        `(;; no window
          ("\\`\\*Async Shell Command\\*\\'"
           (display-buffer-no-window))
          ;; top side window
          ("\\**prot-elfeed-bongo-queue.*"
           (display-buffer-reuse-window display-buffer-in-side-window)
           (window-height . 0.16)
           (side . top)
           (slot . -2))
          ("\\*\\(prot-elfeed-mpv-output\\|world-clock\\).*"
           (display-buffer-in-side-window)
           (window-height . 0.16)
           (side . top)
           (slot . -1))
          ("\\*\\(Flymake diagnostics\\|Package-Lint\\).*"
           (display-buffer-in-side-window)
           (window-height . 0.16)
           (side . top)
           (slot . 0))
          ("\\*Messages.*"
           (display-buffer-in-side-window)
           (window-height . 0.16)
           (side . top)
           (slot . 1))
          ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|Flymake log\\)\\*"
           (display-buffer-in-side-window)
           (window-height . 0.16)
           (side . top)
           (slot . 2))
          ;; left side window
          ("\\*\\(.* # Help.*\\|Help\\)\\*"    ; See the hooks for `visual-line-mode'
           (display-buffer-reuse-mode-window display-buffer-in-side-window)
           (window-width . 0.25)
           (side . left)
           (slot . 0))
          ;; right side window
          ("\\*keycast\\*"
           (display-buffer-in-side-window)
           (dedicated . t)
           (window-width . 0.25)
           (side . right)
           (slot . -1)
           (window-parameters . ((no-other-window . t)
                                 (mode-line-format . none))))
          ;; bottom side window
          ("\\*Org Select\\*"
           (display-buffer-in-side-window)
           (dedicated . t)
           (side . bottom)
           (slot . 0)
           (window-parameters . ((mode-line-format . none))))
          ;; bottom buffer (NOT side window)
          ("\\*Embark Actions\\*"
           (display-buffer-reuse-mode-window display-buffer-at-bottom)
           (window-height . fit-window-to-buffer)
           (window-parameters . ((no-other-window . t)
                                 (mode-line-format . none))))
          ;; ("\\*\\(Embark\\)?.*Completions.*"
          ;;  (display-buffer-reuse-mode-window display-buffer-at-bottom)
          ;;  (window-parameters . ((no-other-window . t))))
          ("\\*\\(Output\\|Register Preview\\).*"
           (display-buffer-reuse-mode-window display-buffer-at-bottom))
          ;; below current window
          ("\\*.*\\(e?shell\\|v?term\\).*"
           (display-buffer-reuse-mode-window display-buffer-below-selected))
          ("\\*\\vc-\\(incoming\\|outgoing\\|git : \\).*"
           (display-buffer-reuse-mode-window display-buffer-below-selected)
           ;; NOTE 2021-10-06: we cannot `fit-window-to-buffer' because
           ;; the height is not known in advance.
           (window-height . 0.2))
          ("\\*\\(Calendar\\|Bookmark Annotation\\).*"
           (display-buffer-reuse-mode-window display-buffer-below-selected)
           (window-height . fit-window-to-buffer))))
  (setq window-combination-resize t)
  (setq even-window-sizes 'height-only)
  (setq window-sides-vertical nil)
  (setq switch-to-buffer-in-dedicated-window 'pop)

  (add-hook 'help-mode-hook #'visual-line-mode)
  (add-hook 'custom-mode-hook #'visual-line-mode)

  (let ((map global-map))
    (define-key map (kbd "C-x <down>") #'next-buffer)
    (define-key map (kbd "C-x <up>") #'previous-buffer)
    (define-key map (kbd "C-x C-n") #'next-buffer)     ; override `set-goal-column'
    (define-key map (kbd "C-x C-p") #'previous-buffer) ; override `mark-page'
    (define-key map (kbd "C-x !") #'delete-other-windows-vertically)
    (define-key map (kbd "C-x _") #'balance-windows)      ; underscore
    (define-key map (kbd "C-x -") #'fit-window-to-buffer) ; hyphen
    (define-key map (kbd "C-x +") #'balance-windows-area)
    (define-key map (kbd "C-x }") #'enlarge-window)
    (define-key map (kbd "C-x {") #'shrink-window)
    (define-key map (kbd "C-x >") #'enlarge-window-horizontally) ; override `scroll-right'
    (define-key map (kbd "C-x <") #'shrink-window-horizontally)) ; override `scroll-left'
  (let ((map resize-window-repeat-map))
    (define-key map ">" #'enlarge-window-horizontally)
    (define-key map "<" #'shrink-window-horizontally)))

;;; Window history (winner-mode)
(prot-emacs-builtin-package 'winner
  (add-hook 'after-init-hook #'winner-mode)

  ;; ;; NOTE 2021-07-31: Those are superseded by the commands
  ;; ;; `prot-tab-winner-undo' and `prot-tab-winner-redo' in prot-tab.el
  ;; ;; (search this document).
  ;; (let ((map global-map))
  ;;   (define-key map (kbd "C-x <right>") #'winner-redo)
  ;;   (define-key map (kbd "C-x <left>") #'winner-undo))
  )

;;; Directional window motions (windmove)
(prot-emacs-builtin-package 'windmove
  (setq windmove-create-window nil)     ; Emacs 27.1
  (let ((map global-map))
    ;; Those override some commands that are already available with
    ;; C-M-u, C-M-f, C-M-b.
    (define-key map (kbd "C-M-<up>") #'windmove-up)
    (define-key map (kbd "C-M-<right>") #'windmove-right)
    (define-key map (kbd "C-M-<down>") #'windmove-down)
    (define-key map (kbd "C-M-<left>") #'windmove-left)
    (define-key map (kbd "C-M-S-<up>") #'windmove-swap-states-up)
    (define-key map (kbd "C-M-S-<right>") #'windmove-swap-states-right) ; conflicts with `org-increase-number-at-point'
    (define-key map (kbd "C-M-S-<down>") #'windmove-swap-states-down)
    (define-key map (kbd "C-M-S-<left>") #'windmove-swap-states-left)))

;;; Tabs for window layouts (tab-bar.el and prot-tab.el)
(prot-emacs-builtin-package 'tab-bar
  (setq tab-bar-close-button-show nil)
  (setq tab-bar-close-last-tab-choice nil)
  (setq tab-bar-close-tab-select 'recent)
  (setq tab-bar-new-tab-choice t)
  (setq tab-bar-new-tab-to 'right)
  (setq tab-bar-position nil)
  (setq tab-bar-show nil)
  (setq tab-bar-tab-hints nil)
  (setq tab-bar-tab-name-function 'tab-bar-tab-name-current)

  (tab-bar-mode -1)                     ; see `prot-tab-status-line'

  ;; Same concept as `winner-mode'.  See the `prot-tab-winner-undo' and
  ;; its counterpart.
  (tab-bar-history-mode 1))

(prot-emacs-builtin-package 'prot-tab
  (setq prot-tab-tab-select-num-threshold 3)
  (setq tab-bar-format                    ; Emacs 28
        '(prot-tab-format-space-single
          prot-tab-format-mule-info
          prot-tab-format-modified
          tab-bar-format-tabs-groups
          prot-tab-format-space-double
          prot-tab-format-position
          prot-tab-format-space-double
          prot-tab-format-vc
          prot-tab-format-space-double
          prot-tab-format-modes         ; FIXME 2021-07-30: Make it work with `minions'.
          tab-bar-format-align-right
          prot-tab-format-misc-info
          prot-tab-format-space-double
          tab-bar-format-global
          prot-tab-format-space-single))

  (add-hook 'after-init-hook #'prot-tab-status-line)

  (let ((map global-map))
    (define-key map (kbd "C-x <right>") #'prot-tab-winner-redo)
    (define-key map (kbd "C-x <left>") #'prot-tab-winner-undo)
    (define-key map (kbd "<f8>") #'prot-tab-status-line) ; unopinionated alternative: `prot-tab-bar-toggle'
    (define-key map (kbd "C-x t t") #'prot-tab-select-tab-dwim)))

;; ;; This is only included as a reference.
;; (prot-emacs-builtin-package 'tab-line
;;   (global-tab-line-mode -1))

;;; Transposition and rotation of windows
(prot-emacs-elpa-package 'transpose-frame
  (let ((map global-map))
    (define-key map (kbd "C-x M-r") #'rotate-frame-clockwise)))

;;; Built-in bookmarking framework (bookmark.el and prot-bookmark.el)
(prot-emacs-builtin-package 'bookmark
  (setq bookmark-use-annotations nil)
  (setq bookmark-automatically-show-annotations t)
  (setq bookmark-set-fringe-mark t) ; Emacs28

  (add-hook 'bookmark-bmenu-mode-hook #'hl-line-mode))

(prot-emacs-builtin-package 'prot-bookmark
  (prot-bookmark-extra-keywords 1))

;;; Ibuffer-like bookmark list (blist.el)
;; Project repo: <https://gitlab.com/mmemmew/blist>.  Its dependency is
;; `ilist', by the same author: <https://gitlab.com/mmemmew/blist>.
;;
;; I handle those manually via git, at least until they become available
;; through an ELPA.
;;
;; `prot-emacs-manual-package' is defined in my init.el
(prot-emacs-elpa-package 'ilist)

(prot-emacs-elpa-package 'blist
  (setq blist-expert t)
  (setq blist-discard-empty-p t)

  ;; NOTE 2021-09-16: This package is still in its early days.  Things
  ;; will change.
  (with-eval-after-load 'prot-eww
    (blist-define-criterion "eww" "EWW"
      (eq (bookmark-get-handler bookmark)
          #'prot-eww-bookmark-jump)))

  (with-eval-after-load 'prot-eshell
    (blist-define-criterion "eshell" "Eshell"
      (eq (bookmark-get-handler bookmark)
          #'prot-eshell-bookmark-jump)))

  (blist-define-criterion "info" "Info"
    (eq (bookmark-get-handler bookmark)
        #'Info-bookmark-jump))

  (setq blist-filter-groups
        (list
         (cons "EWW" #'blist-eww-p)
         (cons "Eshell" #'blist-eshell-p)
         (cons "Info" #'blist-info-p)
         (cons "Default" #'blist-default-p)))

  (define-key global-map (kbd "C-x r l") #'blist-list-bookmarks))

(prot-emacs-builtin-package 'denote
  (setq denote-directory (expand-file-name "~/Documents/notes/"))
  (setq denote-known-keywords
        '("emacs" "philosophy" "politics" "economics"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-org-capture-specifiers "%l\n%i\n%?")

  (require 'denote-link)

  (with-eval-after-load 'org-capture
    (require 'denote-org-capture)
    (add-to-list 'org-capture-templates
                 '("n" "New note (with denote.el)" plain
                   (file denote-last-path)
                   #'denote-org-capture
                   :no-save t
                   :immediate-finish nil
                   :kill-buffer t
                   :jump-to-captured t))))

;;; Custom extensions for "focus mode" (logos.el)
(prot-emacs-elpa-package 'olivetti
  (setq olivetti-body-width 0.7)
  (setq olivetti-minimum-body-width 80)
  (setq olivetti-recall-visual-line-mode-entry-state t))

(prot-emacs-builtin-package 'logos
  (setq logos-outlines-are-pages t)
  (setq logos-outline-regexp-alist
        `((emacs-lisp-mode . ,(format "\\(^;;;+ \\|%s\\)" logos--page-delimiter))
          (org-mode . ,(format "\\(^\\*+ +\\|^-\\{5\\}$\\|%s\\)" logos--page-delimiter))
          (markdown-mode . ,(format "\\(^\\#+ +\\|^[*-]\\{5\\}$\\|^\\* \\* \\*$\\|%s\\)" logos--page-delimiter))
          (conf-toml-mode . "^\\[")
          (t . ,(or outline-regexp logos--page-delimiter))))

  ;; These apply when `logos-focus-mode' is enabled.  Their value is
  ;; buffer-local.
  (setq-default logos-hide-mode-line t)
  (setq-default logos-hide-buffer-boundaries t)
  (setq-default logos-hide-fringe t)
  (setq-default logos-variable-pitch t) ; see my `fontaine' configurations
  (setq-default logos-buffer-read-only nil)
  (setq-default logos-scroll-lock nil)
  (setq-default logos-olivetti t)

  (let ((map global-map))
    (define-key map [remap narrow-to-region] #'logos-narrow-dwim)
    (define-key map [remap forward-page] #'logos-forward-page-dwim)
    (define-key map [remap backward-page] #'logos-backward-page-dwim)
    ;; I don't think I ever saw a package bind M-] or M-[...
    (define-key map (kbd "M-]") #'logos-forward-page-dwim)
    (define-key map (kbd "M-[") #'logos-backward-page-dwim)
    (define-key map (kbd "<f9>") #'logos-focus-mode))

;;;; Extra tweaks
  ;; Read the logos manual: <https://protesilaos.com/emacs/logos>.

  ;; place point at the top when changing pages, but not in `prog-mode'
  (defun prot/logos--recenter-top ()
    "Use `recenter' to reposition the view at the top."
    (unless (derived-mode-p 'prog-mode)
      (recenter 1))) ; Use 0 for the absolute top

  (add-hook 'logos-page-motion-hook #'prot/logos--recenter-top))

;;; TMR May Ring (tmr is used to set timers)
(prot-emacs-builtin-package 'tmr
  (setq tmr-sound-file
        "/usr/share/sounds/freedesktop/stereo/alarm-clock-elapsed.oga")
  (setq tmr-notification-urgency 'normal)
  (setq tmr-descriptions-list (list "Boil water" "Prepare tea" "Bake bread"))

  ;; You do not need these if you install the package.
  (require 'tmr-sound)
  (require 'tmr-notification)
  (require 'tmr-tabulated)

  (let ((map global-map))
    (define-key map (kbd "C-c t t") #'tmr)
    (define-key map (kbd "C-c t T") #'tmr-with-description)
    (define-key map (kbd "C-c t c") #'tmr-clone)
    (define-key map (kbd "C-c t l") #'tmr-tabulated-view) ; "list timers" mnemonic
    (define-key map (kbd "C-c t k") #'tmr-cancel)
    (define-key map (kbd "C-c t K") #'tmr-remove-finished)))

;;; Diff-mode (and prot-diff.el extensions)
(prot-emacs-builtin-package 'diff-mode
  (setq diff-default-read-only t)
  (setq diff-advance-after-apply-hunk t)
  (setq diff-update-on-the-fly t)
  ;; The following are from Emacs 27.1
  (setq diff-refine nil)                ; I do it on demand
  (setq diff-font-lock-prettify nil)    ; better for patches
  ;; The following is further controlled by
  ;; `prot-diff-modus-themes-diffs'
  (setq diff-font-lock-syntax 'hunk-also))

(prot-emacs-builtin-package 'prot-diff
  (prot-diff-modus-themes-diffs)
  (add-hook 'modus-themes-after-load-theme-hook #'prot-diff-modus-themes-diffs)

  (prot-diff-extra-keywords 1)

  ;; `prot-diff-buffer-dwim' replaces the default for `vc-diff' (which I
  ;; bind to another key---see VC section).
  (define-key global-map (kbd "C-x v =") #'prot-diff-buffer-dwim)
  (let ((map diff-mode-map))
    (define-key map (kbd "C-c C-b") #'prot-diff-refine-cycle) ; replace `diff-refine-hunk'
    (define-key map (kbd "C-c C-n") #'prot-diff-narrow-dwim)))

;;; Version control framework (vc.el and prot-vc.el)
(prot-emacs-builtin-package 'vc
  ;; Those offer various types of functionality, such as blaming,
  ;; viewing logs, showing a dedicated buffer with changes to affected
  ;; files.
  (require 'vc-annotate)
  (require 'vc-dir)
  (require 'vc-git)
  (require 'add-log)
  (require 'log-view)

  ;; This one is for editing commit messages.
  (require 'log-edit)
  (setq log-edit-confirm 'changed)
  (setq log-edit-keep-buffer nil)
  (setq log-edit-require-final-newline t)
  (setq log-edit-setup-add-author nil)

  ;; Note that `prot-vc-git-setup-mode' will run the following when
  ;; activated:
  ;;
  ;;   (remove-hook 'log-edit-hook #'log-edit-show-files)
  ;;
  ;; If you need the window to pop back up, do it manually with C-c C-f
  ;; which calls `log-edit-show-files'.

  (setq vc-find-revision-no-save t)
  (setq vc-annotate-display-mode 'scale) ; scale to oldest
  ;; I use a different account for git commits
  (setq add-log-mailing-address "info@protesilaos.com")
  (setq add-log-keep-changes-together t)
  (setq vc-git-diff-switches '("--patch-with-stat" "--histogram"))
  (setq vc-git-print-log-follow t)
  (setq vc-git-revision-complete-only-branches nil) ; Emacs 28
  (setq vc-git-root-log-format
        '("%d %h %ad %an: %s"
          ;; The first shy group matches the characters drawn by --graph.
          ;; We use numbered groups because `log-view-message-re' wants the
          ;; revision number to be group 1.
          "^\\(?:[*/\\|]+\\)\\(?:[*/\\| ]+\\)?\
\\(?2: ([^)]+) \\)?\\(?1:[0-9a-z]+\\) \
\\(?4:[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\) \
\\(?3:.*?\\):"
          ((1 'log-view-message)
           (2 'change-log-list nil lax)
           (3 'change-log-name)
           (4 'change-log-date))))

  (add-hook 'log-view-mode-hook #'hl-line-mode)

  ;; NOTE: I override lots of the defaults
  (let ((map global-map))
    (define-key map (kbd "C-x v b") #'vc-retrieve-tag)  ; "branch" switch
    (define-key map (kbd "C-x v t") #'vc-create-tag)
    (define-key map (kbd "C-x v f") #'vc-log-incoming)  ; the actual git fetch
    (define-key map (kbd "C-x v o") #'vc-log-outgoing)
    (define-key map (kbd "C-x v F") #'vc-update)        ; "F" because "P" is push
    (define-key map (kbd "C-x v d") #'vc-diff))
  (let ((map vc-dir-mode-map))
    (define-key map (kbd "b") #'vc-retrieve-tag)
    (define-key map (kbd "t") #'vc-create-tag)
    (define-key map (kbd "O") #'vc-log-outgoing)
    (define-key map (kbd "o") #'vc-dir-find-file-other-window)
    (define-key map (kbd "f") #'vc-log-incoming) ; replaces `vc-dir-find-file' (use RET)
    (define-key map (kbd "F") #'vc-update)       ; symmetric with P: `vc-push'
    (define-key map (kbd "d") #'vc-diff)         ; parallel to D: `vc-root-diff'
    (define-key map (kbd "k") #'vc-dir-clean-files)
    (define-key map (kbd "G") #'vc-revert)
    (let ((prot-vc-git-branch-map (make-sparse-keymap)))
      (define-key map "B" prot-vc-git-branch-map)
      (define-key prot-vc-git-branch-map "n" #'vc-create-tag) ; new branch/tag
      (define-key prot-vc-git-branch-map "s" #'vc-retrieve-tag) ; switch branch/tag
      (define-key prot-vc-git-branch-map "c" #'prot-vc-git-checkout-remote) ; "checkout" remote
      (define-key prot-vc-git-branch-map "l" #'vc-print-branch-log))
    (let ((prot-vc-git-stash-map (make-sparse-keymap)))
      (define-key map "S" prot-vc-git-stash-map)
      (define-key prot-vc-git-stash-map "c" 'vc-git-stash) ; "create" named stash
      (define-key prot-vc-git-stash-map "s" 'vc-git-stash-snapshot)))
  (let ((map vc-git-stash-shared-map))
    (define-key map "a" 'vc-git-stash-apply-at-point)
    (define-key map "c" 'vc-git-stash) ; "create" named stash
    (define-key map "D" 'vc-git-stash-delete-at-point)
    (define-key map "p" 'vc-git-stash-pop-at-point)
    (define-key map "s" 'vc-git-stash-snapshot))
  (let ((map vc-annotate-mode-map))
    (define-key map (kbd "M-q") #'vc-annotate-toggle-annotation-visibility)
    (define-key map (kbd "C-c C-c") #'vc-annotate-goto-line)
    (define-key map (kbd "<return>") #'vc-annotate-find-revision-at-line))
  (let ((map log-view-mode-map))
    (define-key map (kbd "<tab>") #'log-view-toggle-entry-display)
    (define-key map (kbd "<return>") #'log-view-find-revision)
    (define-key map (kbd "s") #'vc-log-search)
    (define-key map (kbd "o") #'vc-log-outgoing)
    (define-key map (kbd "f") #'vc-log-incoming)
    (define-key map (kbd "F") #'vc-update)
    (define-key map (kbd "P") #'vc-push)))

(prot-emacs-builtin-package 'prot-vc
  (setq prot-vc-log-limit 100)
  (setq prot-vc-log-bulk-action-limit 50)
  (setq prot-vc-git-log-edit-show-commits t)
  (setq prot-vc-git-log-edit-show-commit-count 10)
  (setq prot-vc-shell-output "*prot-vc-output*")
  (setq prot-vc-patch-output-dirs (list "~/" "~/Desktop/"))
  (add-to-list' log-edit-headers-alist '("Amend"))

  ;; This refashions log view and log edit buffers
  (prot-vc-git-setup-mode 1)

  ;; NOTE: I override lots of the defaults
  (let ((map global-map))
    (define-key map (kbd "C-x v i") #'prot-vc-git-log-insert-commits)
    (define-key map (kbd "C-x v p") #'prot-vc-project-or-dir)
    (define-key map (kbd "C-x v SPC") #'prot-vc-custom-log)
    (define-key map (kbd "C-x v g") #'prot-vc-git-grep)
    (define-key map (kbd "C-x v G") #'prot-vc-git-log-grep)
    (define-key map (kbd "C-x v a") #'prot-vc-git-patch-apply)
    (define-key map (kbd "C-x v c") #'prot-vc-git-patch-create-dwim)
    (define-key map (kbd "C-x v s") #'prot-vc-git-show)
    (define-key map (kbd "C-x v r") #'prot-vc-git-find-revision)
    (define-key map (kbd "C-x v B") #'prot-vc-git-blame-region-or-file)
    (define-key map (kbd "C-x v R") #'prot-vc-git-reset))
  (let ((map vc-git-log-edit-mode-map))
    (define-key map (kbd "C-C C-n") #'prot-vc-git-log-edit-extract-file-name)
    (define-key map (kbd "C-C C-i") #'prot-vc-git-log-insert-commits)
    ;; Also done by `prot-vc-git-setup-mode', but I am putting it here
    ;; as well for visibility.
    (define-key map (kbd "C-c C-c") #'prot-vc-git-log-edit-done)
    (define-key map (kbd "C-c C-a") #'prot-vc-git-log-edit-toggle-amend)
    (define-key map (kbd "M-p") #'prot-vc-git-log-edit-previous-comment)
    (define-key map (kbd "M-n") #'prot-vc-git-log-edit-next-comment)
    (define-key map (kbd "M-s") #'prot-vc-git-log-edit-complete-comment)
    (define-key map (kbd "M-r") #'prot-vc-git-log-edit-complete-comment))
  (let ((map log-view-mode-map))
    (define-key map (kbd "<C-tab>") #'prot-vc-log-view-toggle-entry-all)
    (define-key map (kbd "a") #'prot-vc-git-patch-apply)
    (define-key map (kbd "c") #'prot-vc-git-patch-create-dwim)
    (define-key map (kbd "R") #'prot-vc-git-log-reset)
    (define-key map (kbd "w") #'prot-vc-log-kill-hash)))

;;; git-email.el for preparing patches
;; Project repo: <https://git.sr.ht/~yoctocell/git-email>.  This is one
;; of the packages I handle manually via git, at least until it becomes
;; available through an ELPA.
;;
;; `prot-emacs-manual-package' is defined in my init.el
(prot-emacs-manual-package 'git-email
  (with-eval-after-load 'notmuch
    (require 'git-email-notmuch)
    (git-email-notmuch-mode 1))
  (define-key global-map (kbd "C-x v RET") #'git-email-format-patch) ; VC prefix and C-m
  (define-key dired-mode-map (kbd "C-x v RET") #'git-email-send-email))

;;; Interactive and powerful git front-end (Magit)
(prot-emacs-elpa-package 'magit
  (setq magit-define-global-key-bindings nil)
  (define-key global-map (kbd "C-c g") #'magit-status)

  (require 'git-commit)
  (setq git-commit-summary-max-length 50)
  (setq git-commit-known-pseudo-headers
        '("Signed-off-by"
          "Acked-by"
          "Modified-by"
          "Cc"
          "Suggested-by"
          "Reported-by"
          "Tested-by"
          "Reviewed-by"))
  (setq git-commit-style-convention-checks
        '(non-empty-second-line
          overlong-summary-line))

  (require 'magit-diff)
  (setq magit-diff-refine-hunk t)

  (require 'magit-repos)
  (setq magit-repository-directories
        '(("~/Git/Projects" . 1))))

;;; Smerge and Ediff
(prot-emacs-builtin-package 'smerge-mode)

(prot-emacs-builtin-package 'ediff
  (setq ediff-keep-variants nil)
  (setq ediff-make-buffers-readonly-at-startup nil)
  (setq ediff-merge-revisions-with-ancestor t)
  (setq ediff-show-clashes-only t)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)

  ;; Tweak those for safer identification and removal
  (setq ediff-combination-pattern
        '("<<<<<<< prot-ediff-combine Variant A" A
          ">>>>>>> prot-ediff-combine Variant B" B
          "####### prot-ediff-combine Ancestor" Ancestor
          "======= prot-ediff-combine End"))

  ;; TODO automate process in a robust way, or at least offer a good key
  ;; binding.
  (defun prot/ediff-flush-combination-pattern ()
    "Remove my custom `ediff-combination-pattern' markers.

This is a quick-and-dirty way to get rid of the markers that are
left behind by `smerge-ediff' when combining the output of two
diffs.  While this could be automated via a hook, I am not yet
sure this is a good approach."
    (interactive)
    (flush-lines ".*prot-ediff.*" (point-min) (point-max) nil)))

;;; Eshell and prot-eshell.el
(prot-emacs-builtin-package 'eshell
  (require 'esh-mode)
  (require 'esh-module)
  (setq eshell-modules-list             ; It works but may need review
        '(eshell-alias
          eshell-basic
          eshell-cmpl
          eshell-dirs
          eshell-glob
          eshell-hist
          eshell-ls
          eshell-pred
          eshell-prompt
          eshell-script
          eshell-term
          eshell-tramp
          eshell-unix))
  (setenv "PAGER" "cat") ; solves issues, such as with 'git log' and the default 'less'
  (require 'em-cmpl)
  (require 'em-dirs)
  (setq eshell-cd-on-directory t)

  (require 'em-tramp)
  (setq password-cache t)
  (setq password-cache-expiry 600)

  (require 'em-hist)
  (setq eshell-hist-ignoredups t)
  (setq eshell-save-history-on-exit t))

(prot-emacs-builtin-package 'prot-eshell
  (setq prot-eshell-output-buffer "*Exported Eshell output*")
  (setq prot-eshell-output-delimiter "* * *")
  (let ((map eshell-mode-map))
    (define-key map (kbd "M-k") #'eshell-kill-input)
    (define-key map (kbd "C-c C-f") #'prot-eshell-ffap-find-file)
    (define-key map (kbd "C-c C-j") #'prot-eshell-ffap-dired-jump)
    (define-key map (kbd "C-c C-w") #'prot-eshell-ffap-kill-save)
    (define-key map (kbd "C-c C->") #'prot-eshell-redirect-to-buffer)
    (define-key map (kbd "C-c C-e") #'prot-eshell-export)
    (define-key map (kbd "C-c C-r") #'prot-eshell-root-dir))
  (let ((map eshell-cmpl-mode-map))
    (define-key map (kbd "C-c TAB") #'prot-eshell-ffap-insert) ; C-c C-i
    (define-key map (kbd "C-c C-h") #'prot-eshell-narrow-output-highlight-regexp))
  (let ((map eshell-hist-mode-map))
    (define-key map (kbd "M-s") #'nil) ; I use this prefix for lots of more useful commands
    (define-key map (kbd "M-r") #'prot-eshell-complete-history)
    (define-key map (kbd "C-c C-d") #'prot-eshell-complete-recent-dir)
    (define-key map (kbd "C-c C-s") #'prot-eshell-find-subdirectory-recursive)))

;;; Shell (M-x shell)
(prot-emacs-builtin-package 'shell
  (setq shell-command-prompt-show-cwd t) ; Emacs 27.1
  (setq ansi-color-for-comint-mode t))

;;; Org-mode (personal information manager)
;; Pro tip: If you are reading the source code, use C-c '
;; (`org-edit-special') to put the code block in a dedicated buffer and
;; then activate `prot-outline-minor-mode-safe' to conveniently browse
;; this massive code block.
(prot-emacs-builtin-package 'org
  (setq org-directory (convert-standard-filename "~/Documents/org"))
  (setq org-imenu-depth 7)
;;;; general settings
  (setq org-adapt-indentation nil)      ; No, non, nein, όχι!
  (setq org-special-ctrl-a/e nil)
  (setq org-special-ctrl-k nil)
  (setq org-M-RET-may-split-line '((default . nil)))
  (setq org-hide-emphasis-markers nil)
  (setq org-hide-macro-markers nil)
  (setq org-hide-leading-stars nil)
  (setq org-cycle-separator-lines 0)
  (setq org-structure-template-alist    ; CHANGED in Org 9.3, Emacs 27.1
        '(("s" . "src")
          ("E" . "src emacs-lisp")
          ("e" . "example")
          ("q" . "quote")
          ("v" . "verse")
          ("V" . "verbatim")
          ("c" . "center")
          ("C" . "comment")))
  (setq org-catch-invisible-edits 'show)
  (setq org-return-follows-link nil)
  (setq org-loop-over-headlines-in-active-region 'start-level)
  (setq org-modules '(ol-info ol-eww))
  (setq org-use-sub-superscripts '{})
  (setq org-insert-heading-respect-content t)

;;;; refile, todo
  (setq org-refile-targets
        '((org-agenda-files . (:maxlevel . 2))
          (nil . (:maxlevel . 2))))
  (setq org-refile-use-outline-path t)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-use-cache t)
  (setq org-reverse-note-order nil)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "MAYBE(m)" "WAIT(w@/!)" "|" "CANCEL(c@)" "DONE(d!)")))
  (setq org-todo-keyword-faces
        '(("WAIT" . '(bold org-todo))
          ("MAYBE" . '(bold shadow))
          ("CANCEL" . '(bold org-done))))
  (setq org-use-fast-todo-selection 'expert)
  (setq org-priority-faces
        '((?A . '(bold org-priority))
          (?B . org-priority)
          (?C . '(shadow org-priority))))
  (setq org-fontify-done-headline nil)
  (setq org-fontify-todo-headline nil)
  (setq org-fontify-quote-and-verse-blocks t)
  (setq org-fontify-whole-heading-line nil)
  (setq org-fontify-whole-block-delimiter-line nil)
  (setq org-highlight-latex-and-related nil) ; other options affect elisp regexp in src blocks
  (setq org-enforce-todo-dependencies t)
  (setq org-enforce-todo-checkbox-dependencies t)
  (setq org-track-ordered-property-with-tag t)
  (setq org-highest-priority ?A)
  (setq org-lowest-priority ?C)
  (setq org-default-priority ?A)

;;;; tags
  (setq org-tag-alist ; I don't really use those, but whatever
        '(("meeting")
          ("admin")
          ("emacs")
          ("modus")
          ("politics")
          ("economics")
          ("philosophy")
          ("book")
          ("essay")
          ("mail")
          ("purchase")
          ("hardware")
          ("software")
          ("website")))

  (setq org-auto-align-tags nil)
  (setq org-tags-column 0)

;;;; log
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-log-note-clock-out nil)
  (setq org-log-redeadline 'time)
  (setq org-log-reschedule 'time)
  (setq org-read-date-prefer-future 'time)

;;;; links
  (setq org-link-keep-stored-after-insertion nil)
  ;; TODO 2021-10-15 org-link-make-description-function

;;;; capture
  (setq org-capture-templates
        `(("b" "Basic task for future review" entry
           (file+headline "tasks.org" "Tasks to be reviewed")
           ,(concat "* %^{Title}\n"
                    ":PROPERTIES:\n"
                    ":CAPTURED: %U\n"
                    ":END:\n\n"
                    "%i%l")
           :empty-lines-after 1)
          ("c" "Clock in to a task" entry
           (file+headline "tasks.org" "Clocked tasks")
           ,(concat "* TODO %^{Title}\n"
                    "SCHEDULED: %T\n"
                    ":PROPERTIES:\n"
                    ":EFFORT: %^{Effort estimate in minutes|5|10|15|30|45|60|90|120}\n"
                    ":END:\n\n"
                    "%a\n")
           :prepend t
           :clock-in t
           :clock-keep t
           :immediate-finish t
           :empty-lines-after 1)
          ("m" "Memorandum of conversation" entry
           (file+headline "tasks.org" "Tasks to be reviewed")
           ,(concat "* Memorandum of conversation with %^{Person}\n"
                    ":PROPERTIES:\n"
                    ":CAPTURED: %U\n"
                    ":END:\n\n"
                    "%i%?")
           :empty-lines-after 1)
          ("t" "Task with a due date" entry
           (file+headline "tasks.org" "Tasks with a date")
           ,(concat "* TODO %^{Title} %^g\n"
                    "SCHEDULED: %^t\n"
                    ":PROPERTIES:\n"
                    ":CAPTURED: %U\n"
                    ":END:\n\n"
                    "%a\n%i%?")
           :empty-lines-after 1)
          ("e" "Email note" entry
           (file+headline "tasks.org" "Tasks to be reviewed")
           ,(concat "* MAYBE %:subject :mail:\n"
                    ":PROPERTIES:\n"
                    ":CAPTURED: %U\n"
                    ":END:\n\n"
                    "%a\n%i%?")
           :empty-lines-after 1)))

  (setq org-capture-templates-contexts
        '(("e" ((in-mode . "notmuch-search-mode")
                (in-mode . "notmuch-show-mode")
                (in-mode . "notmuch-tree-mode")))))

;;;; agenda
;;;;; Basic agenda setup
  (setq org-default-notes-file (thread-last org-directory (expand-file-name "notes.org")))
  (setq org-agenda-files `(,org-directory "~/Documents"))
  (setq org-agenda-span 'week)
  (setq org-agenda-start-on-weekday 1)  ; Monday
  (setq org-agenda-confirm-kill t)
  (setq org-agenda-show-all-dates t)
  (setq org-agenda-show-outline-path nil)
  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-skip-comment-trees t)
  (setq org-agenda-menu-show-matcher t)
  (setq org-agenda-menu-two-columns nil)
  (setq org-agenda-sticky nil)
  (setq org-agenda-custom-commands-contexts nil)
  (setq org-agenda-max-entries nil)
  (setq org-agenda-max-todos nil)
  (setq org-agenda-max-tags nil)
  (setq org-agenda-max-effort nil)

  ;; NOTE 2021-12-07: In my `prot-org.el' (see further below), I add
  ;; `org-agenda-to-appt' to various relevant hooks.
  ;;
  ;; Create reminders for tasks with a due date when this file is read.
  (run-at-time (* 60 5) nil #'org-agenda-to-appt)

;;;;; General agenda view options
  ;; NOTE 2021-12-07: Check further below my `org-agenda-custom-commands'
  (setq org-agenda-prefix-format
        '((agenda . " %i %-12:c%?-12t% s")
          (todo . " %i %-12:c")
          (tags . " %i %-12:c")
          (search . " %i %-12:c")))
  (setq org-agenda-sorting-strategy
        '(((agenda habit-down time-up priority-down category-keep)
           (todo priority-down category-keep)
           (tags priority-down category-keep)
           (search category-keep))))
  (setq org-agenda-breadcrumbs-separator "->")
  (setq org-agenda-todo-keyword-format "%-1s")
  (setq org-agenda-fontify-priorities 'cookies)
  (setq org-agenda-category-icon-alist nil)
  (setq org-agenda-remove-times-when-in-prefix nil)
  (setq org-agenda-remove-timeranges-from-blocks nil)
  (setq org-agenda-compact-blocks nil)
  (setq org-agenda-block-separator ?—)

;;;;; Agenda marks
  (setq org-agenda-bulk-mark-char "#")
  (setq org-agenda-persistent-marks nil)

;;;;; Agenda diary entries
  (setq org-agenda-insert-diary-strategy 'date-tree)
  (setq org-agenda-insert-diary-extract-time nil)
  (setq org-agenda-include-diary nil)

;;;;; Agenda follow mode
  (setq org-agenda-start-with-follow-mode nil)
  (setq org-agenda-follow-indirect t)

;;;;; Agenda multi-item tasks
  (setq org-agenda-dim-blocked-tasks t)
  (setq org-agenda-todo-list-sublevels t)

;;;;; Agenda filters and restricted views
  (setq org-agenda-persistent-filter nil)
  (setq org-agenda-restriction-lock-highlight-subtree t)

;;;;; Agenda items with deadline and scheduled timestamps
  (setq org-agenda-include-deadlines t)
  (setq org-deadline-warning-days 5)
  (setq org-agenda-skip-scheduled-if-done nil)
  (setq org-agenda-skip-scheduled-if-deadline-is-shown t)
  (setq org-agenda-skip-timestamp-if-deadline-is-shown t)
  (setq org-agenda-skip-deadline-if-done nil)
  (setq org-agenda-skip-deadline-prewarning-if-scheduled 1)
  (setq org-agenda-skip-scheduled-delay-if-deadline nil)
  (setq org-agenda-skip-additional-timestamps-same-entry nil)
  (setq org-agenda-skip-timestamp-if-done nil)
  (setq org-agenda-search-headline-for-time nil)
  (setq org-scheduled-past-days 365)
  (setq org-deadline-past-days 365)
  (setq org-agenda-move-date-from-past-immediately-to-today t)
  (setq org-agenda-show-future-repeats t)
  (setq org-agenda-prefer-last-repeat nil)
  (setq org-agenda-timerange-leaders
        '("" "(%d/%d): "))
  (setq org-agenda-scheduled-leaders
        '("Scheduled: " "Sched.%2dx: "))
  (setq org-agenda-inactive-leader "[")
  (setq org-agenda-deadline-leaders
        '("Deadline:  " "In %3d d.: " "%2d d. ago: "))
  ;; Time grid
  (setq org-agenda-time-leading-zero t)
  (setq org-agenda-timegrid-use-ampm nil)
  (setq org-agenda-use-time-grid t)
  (setq org-agenda-show-current-time-in-grid t)
  (setq org-agenda-current-time-string
        (concat "Now " (make-string 70 ?-)))
  (setq org-agenda-time-grid
        '((daily today require-timed)
          (0600 0700 0800 0900 1000 1100
                1200 1300 1400 1500 1600
                1700 1800 1900 2000 2100)
          " ....." "-----------------"))
  (setq org-agenda-default-appointment-duration nil)

;;;;; Agenda global to-do list
  (setq org-agenda-todo-ignore-with-date t)
  (setq org-agenda-todo-ignore-timestamp t)
  (setq org-agenda-todo-ignore-scheduled t)
  (setq org-agenda-todo-ignore-deadlines t)
  (setq org-agenda-todo-ignore-time-comparison-use-seconds t)
  (setq org-agenda-tags-todo-honor-ignore-options nil)

;;;;; Agenda tagged items
  (setq org-agenda-show-inherited-tags t)
  (setq org-agenda-use-tag-inheritance
        '(todo search agenda))
  (setq org-agenda-hide-tags-regexp nil)
  (setq org-agenda-remove-tags nil)
  (setq org-agenda-tags-column -100)

;;;;; Agenda entry
  ;; NOTE: I do not use this right now.  Leaving everything to its
  ;; default value.
  (setq org-agenda-start-with-entry-text-mode nil)
  (setq org-agenda-entry-text-maxlines 5)
  (setq org-agenda-entry-text-exclude-regexps nil)
  (setq org-agenda-entry-text-leaders "    > ")

;;;;; Agenda logging and clocking
  ;; NOTE: I do not use these yet, though I plan to.  Leaving everything
  ;; to its default value for the time being.
  (setq org-agenda-log-mode-items '(closed clock))
  (setq org-agenda-clock-consistency-checks
        '((:max-duration "10:00" :min-duration 0 :max-gap "0:05" :gap-ok-around
                         ("4:00")
                         :default-face ; This should definitely be reviewed
                         ((:background "DarkRed")
                          (:foreground "white"))
                         :overlap-face nil :gap-face nil :no-end-time-face nil
                         :long-face nil :short-face nil)))
  (setq org-agenda-log-mode-add-notes t)
  (setq org-agenda-start-with-log-mode nil)
  (setq org-agenda-start-with-clockreport-mode nil)
  (setq org-agenda-clockreport-parameter-plist '(:link t :maxlevel 2))
  (setq org-agenda-search-view-always-boolean nil)
  (setq org-agenda-search-view-force-full-words nil)
  (setq org-agenda-search-view-max-outline-level 0)
  (setq org-agenda-search-headline-for-time t)
  (setq org-agenda-use-time-grid t)
  (setq org-agenda-cmp-user-defined nil)
  (setq org-agenda-sort-notime-is-late t) ; Org 9.4
  (setq org-agenda-sort-noeffort-is-high t) ; Org 9.4

;;;;; Agenda column view
  ;; NOTE I do not use these, but may need them in the future.
  (setq org-agenda-view-columns-initially nil)
  (setq org-agenda-columns-show-summaries t)
  (setq org-agenda-columns-compute-summary-properties t)
  (setq org-agenda-columns-add-appointments-to-effort-sum nil)
  (setq org-agenda-auto-exclude-function nil)
  (setq org-agenda-bulk-custom-functions nil)

;;;;; Agenda habits
  (require 'org-habit)
  (setq org-habit-graph-column 50)
  (setq org-habit-preceding-days 9)

;;;; code blocks
  (setq org-confirm-babel-evaluate nil)
  (setq org-src-window-setup 'current-window)
  (setq org-edit-src-persistent-message nil)
  (setq org-src-fontify-natively t)
  (setq org-src-preserve-indentation t)
  (setq org-src-tab-acts-natively t)
  (setq org-edit-src-content-indentation 0)

;;;; export
  (setq org-export-with-toc t)
  (setq org-export-headline-levels 8)
  (setq org-export-dispatch-use-expert-ui nil)
  (setq org-html-htmlize-output-type nil)
  (setq org-html-head-include-default-style nil)
  (setq org-html-head-include-scripts nil)
  (require 'ox-texinfo)
  (require 'ox-md)
  ;; FIXME: how to remove everything else?
  (setq org-export-backends '(html texinfo md))

;;;; IDs
  (setq org-id-link-to-org-use-id
        'create-if-interactive-and-no-custom-id)

;;;; Hooks and key bindings

  ;; See my `pulsar' package, which is declared further above (otherwise
  ;; I would wrap this in `with-eval-after-load'):
  ;; <https://protesilaos.com/emacs/pulsar>
  (dolist (hook '(org-agenda-after-show-hook org-follow-link-hook))
    (add-hook hook #'pulsar-recenter-top)
    (add-hook hook #'pulsar-reveal-entry))

  (let ((map global-map))
    (define-key map (kbd "C-c a") #'org-agenda)
    (define-key map (kbd "C-c c") #'org-capture)
    (define-key map (kbd "C-c l") #'org-store-link))
  (let ((map org-mode-map))
    (define-key map (kbd "C-'") nil)
    (define-key map (kbd "C-,") nil)
    (define-key map (kbd "<C-return>") nil)
    (define-key map (kbd "<C-S-return>") nil)
    (define-key map (kbd "C-M-S-<right>") nil)
    (define-key map (kbd "C-M-S-<left>") nil)
    (define-key map (kbd "C-c S-l") #'org-toggle-link-display)
    (define-key map (kbd "C-c C-S-l") #'org-insert-last-stored-link)))

;;;; Custom extensions (prot-org.el)
(prot-emacs-builtin-package 'prot-org
  (setq org-agenda-format-date #'prot-org-agenda-format-date-aligned)

  ;; Check the variable `prot-org-custom-daily-agenda' in prot-org.el
  (setq org-agenda-custom-commands
        `(("A" "Daily agenda and top priority tasks"
           ,prot-org-custom-daily-agenda
           ((org-agenda-fontify-priorities nil)
            (org-agenda-dim-blocked-tasks nil)))
          ("P" "Plain text daily agenda and top priorities"
           ,prot-org-custom-daily-agenda
           ((org-agenda-with-colors nil)
            (org-agenda-prefix-format "%t %s")
            (org-agenda-current-time-string ,(car (last org-agenda-time-grid)))
            (org-agenda-fontify-priorities nil)
            (org-agenda-remove-tags t))
           ("agenda.txt"))))

  ;; I bind `org-agenda' to C-c a, so this one puts me straight into my
  ;; custom block agenda.
  (define-key global-map (kbd "C-c A") (lambda () (interactive) (org-agenda nil "A")))

  (add-to-list 'org-capture-templates
               '("j" "Music suggestion (jukebox)" entry
                 (file+headline "tasks.org" "Music suggestions")
                 #'prot-org-capture-jukebox
                 :empty-lines-after 1
                 :immediate-finish t)))

(prot-emacs-elpa-package 'org-modern
  (setq org-modern-label-border 1)
  (setq org-modern-variable-pitch nil)
  (setq org-modern-timestamp t)
  (setq org-modern-table t)
  (setq org-modern-table-vertical 1)
  (setq org-modern-table-horizontal 0)
  (setq org-modern-list ; I swap the defaults for + and *
        '((?+ . "•")
          (?- . "–")
          (?* . "◦")))

  ;; NOTE 2022-03-05: The variables that are commented out are the
  ;; defaults.

  ;; (setq org-modern-star ["◉""○""◈""◇""⁕"])
  ;; (setq org-modern-hide-stars 'leading)
  ;; (setq org-modern-checkbox
  ;;       '((?X . #("▢✓" 0 2 (composition ((2)))))
  ;;         (?- . #("▢–" 0 2 (composition ((2)))))
  ;;         (?\s . #("▢" 0 1 (composition ((1)))))))
  ;; (setq org-modern-horizontal-rule t)
  ;; (setq org-modern-priority t)
  ;; (setq org-modern-todo t)
  ;; (setq org-modern-tag t)
  ;; (setq org-modern-block t)
  ;; (setq org-modern-keyword t)
  ;; (setq org-modern-statistics t)
  ;; (setq org-modern-progress ["○""◔""◐""◕""●"])

  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

  ;; NOTE 2022-03-06: I am experimenting with various styles here.  DO NOT COPY.
  ;;
  ;; Also note that I do not wrap this in `with-eval-after-load' because
  ;; the `modus-themes' are always loaded first in my config.
  (defun prot/org-modern-face-tweaks ()
    (modus-themes-with-colors
      (custom-set-faces
       `(org-modern-label ((,class :box (:line-width (-1 . ,org-modern-label-border) :color ,bg-main))))
       `(org-modern-done ((,class :inherit org-modern-label :background ,bg-special-faint-mild :foreground ,green-alt-other)))
       `(org-modern-priority ((,class :inherit org-modern-label :background ,bg-special-faint-calm :foreground ,magenta-alt-other)))
       `(org-modern-statistics ((,class :inherit org-modern-label :background ,bg-special-faint-warm :foreground ,yellow-alt-other)))
       `(org-modern-tag ((,class :inherit org-modern-label :background ,bg-special-faint-calm :foreground ,magenta)))
       `(org-modern-todo ((,class :inherit org-modern-label :background ,bg-special-faint-warm :foreground ,red-alt-other)))
       `(org-modern-date-active ((,class :inherit org-modern-label :background ,bg-alt :foreground ,fg-main)))
       `(org-modern-date-inactive ((,class :inherit org-modern-date-active :foreground ,fg-dim)))
       `(org-modern-time-active ((,class :inherit org-modern-label :background ,bg-active :foreground ,fg-main)))
       `(org-modern-time-inactive ((,class :inherit org-modern-date-inactive))))))

  (add-hook 'modus-themes-after-load-theme-hook #'prot/org-modern-face-tweaks))

;;; Calendar and Diary (and prot-diary.el)
(prot-emacs-builtin-package 'calendar
  (setq calendar-mark-diary-entries-flag t)
  (setq calendar-mark-holidays-flag t)
  (setq calendar-mode-line-format nil)
  (setq calendar-time-display-form
        '(24-hours ":" minutes
                   (when time-zone
                     (format "(%s)" time-zone))))
  (setq calendar-week-start-day 1)      ; Monday
  (setq calendar-date-style 'iso)
  (setq calendar-date-display-form calendar-iso-date-display-form)
  (setq calendar-time-zone-style 'numeric) ; Emacs 28.1

  (require 'solar)
  (setq calendar-latitude 35.17         ; Not my actual coordinates
        calendar-longitude 33.36)

  (require 'cal-dst)
  (setq calendar-standard-time-zone-name "+0200")
  (setq calendar-daylight-time-zone-name "+0300")

  (require 'diary-lib)
  (setq diary-mail-addr user-mail-address)
  (setq diary-date-forms diary-iso-date-forms)
  (setq diary-comment-start ";;")
  (setq diary-comment-end "")
  (setq diary-nonmarking-symbol "!")
  (setq diary-show-holidays-flag t)
  (setq diary-display-function #'diary-fancy-display) ; better than its alternative
  (setq diary-header-line-format nil)
  (setq diary-list-include-blanks nil)
  (setq diary-number-of-entries 2)
  (setq diary-mail-days 2)
  (setq diary-abbreviated-year-flag nil)

  (add-hook 'calendar-today-visible-hook #'calendar-mark-today)
  (add-hook 'diary-list-entries-hook 'diary-sort-entries t)
  (add-hook 'diary-mode-hook #'goto-address-mode) ; buttonise plain text links

  ;; Those presuppose (setq diary-display-function #'diary-fancy-display)
  (add-hook 'diary-list-entries-hook 'diary-include-other-diary-files)
  (add-hook 'diary-mark-entries-hook 'diary-mark-included-diary-files)

  ;; Prevent Org from interfering with my key bindings.
  (remove-hook 'calendar-mode-hook #'org--setup-calendar-bindings)

  (let ((map calendar-mode-map))
    (define-key map (kbd "s") #'calendar-sunrise-sunset)
    (define-key map (kbd "l") #'lunar-phases)
    (define-key map (kbd "i") nil) ; Org sets this, much to my chagrin (see `remove-hook' above)
    (define-key map (kbd "i a") #'diary-insert-anniversary-entry)
    (define-key map (kbd "i b") #'diary-insert-block-entry)
    (define-key map (kbd "i c") #'diary-insert-cyclic-entry)
    (define-key map (kbd "i d") #'diary-insert-entry) ; for current "day"
    (define-key map (kbd "i i") #'diary-insert-entry) ; most common action, easier to type
    (define-key map (kbd "i m") #'diary-insert-monthly-entry)
    (define-key map (kbd "i w") #'diary-insert-weekly-entry)
    (define-key map (kbd "i y") #'diary-insert-yearly-entry)
    (define-key map (kbd "M-n") #'calendar-forward-month)
    (define-key map (kbd "M-p") #'calendar-backward-month)))

(prot-emacs-builtin-package 'appt
  (setq appt-display-diary nil)
  (setq appt-disp-window-function #'appt-disp-window)
  (setq appt-display-mode-line t)
  (setq appt-display-interval 3)
  (setq appt-audible nil)
  (setq appt-warning-time-regexp "appt \\([0-9]+\\)")
  (setq appt-message-warning-time 6)

  (run-at-time 10 nil #'appt-activate 1))

(prot-emacs-builtin-package 'prot-diary
  ;; The idea is to get a reminder via email when I launch Emacs in the
  ;; morning and this file is evaluated.  Obviously this is not a super
  ;; sophisticated approach, though I do not need one.
  (let ((time (string-to-number (format-time-string "%H"))))
    (when (and (> time 4) (< time 9))
      (run-at-time (* 60 5) nil #'prot-diary-mail-entries)))

  (require 'holidays)
  (setq calendar-holidays (append holiday-solar-holidays prot-diary-local-holidays))

  (with-eval-after-load 'prot-outline
    (add-hook 'diary-mode-hook #'prot-outline-minor-mode-safe))

  (let ((map diary-mode-map))
    (define-key map (kbd "<M-return>") #'prot-diary-newline-indent)
    (define-key map (kbd "M-n") #'prot-diary-heading-next)
    (define-key map (kbd "M-p") #'prot-diary-heading-previous)
    (define-key map (kbd "C-c C-a") #'prot-diary-align-timestamped-entries))
  (let ((map global-map))
    (define-key map (kbd "C-c d c") #'calendar)
    (define-key map (kbd "C-c d d") #'prot-diary-display-entries)
    (define-key map (kbd "C-c d e") #'prot-diary-edit-diary)
    (define-key map (kbd "C-c d i") #'prot-diary-insert-entry)
    (define-key map (kbd "C-c d m") #'prot-diary-mail-entries)))

;;; Client-agnostic email settings (and prot-mail.el)
(prot-emacs-builtin-package 'auth-source
  (setq auth-sources '("~/.authinfo.gpg"))
  (setq user-full-name "Protesilaos Stavrou")
  (setq user-mail-address "public@protesilaos.com"))

(prot-emacs-builtin-package 'mm-encode
  (setq mm-encrypt-option nil) ; use 'guided if you need more control
  (setq mm-sign-option nil))   ; same

(prot-emacs-builtin-package 'mml-sec
  (setq mml-secure-openpgp-encrypt-to-self t)
  (setq mml-secure-openpgp-sign-with-sender t)
  (setq mml-secure-smime-encrypt-to-self t)
  (setq mml-secure-smime-sign-with-sender t))

(prot-emacs-builtin-package 'message
  (setq mail-user-agent 'message-user-agent)
  (setq mail-header-separator (purecopy "*****"))
  (setq message-elide-ellipsis "\n> [... %l lines elided]\n")
  (setq compose-mail-user-agent-warnings nil)
  (setq message-mail-user-agent t)      ; use `mail-user-agent'
  (setq mail-signature "Protesilaos Stavrou\nhttps://protesilaos.com\n")
  (setq message-signature "Protesilaos Stavrou\nhttps://protesilaos.com\n")

  ;; Instead of using a citation format like this:
  ;;
  ;; On DATE, PERSON wrote:
  ;; > MESSAGE
  ;;
  ;; I disable the citation line and `message-ignored-cited-headers' to
  ;; get this template instead:
  ;;
  ;; > From: PERSON
  ;; > Date: DATE
  ;; >
  ;; > MESSAGE
  ;;
  ;; (setq message-citation-line-format "On %Y-%m-%d, %R %z, %f wrote:\n")
  ;; (setq message-citation-line-function 'message-insert-formatted-citation-line)
  (setq message-citation-line-function nil)
  (setq message-ignored-cited-headers nil) ; default is "." for all headers

  (setq message-confirm-send nil)
  (setq message-kill-buffer-on-exit t)
  (setq message-wide-reply-confirm-recipients t)
  (add-to-list 'mm-body-charset-encoding-alist '(utf-8 . base64))

  (add-hook 'message-setup-hook #'message-sort-headers))

(prot-emacs-builtin-package 'gnus-dired ; does not require `gnus'
  (add-hook 'dired-mode-hook #'gnus-dired-mode))

(prot-emacs-builtin-package 'prot-mail
  ;; NOTE 2021-05-14: This is a generic indicator for new mail in the
  ;; maildir.  As I now use notmuch (see relevant section in this
  ;; document) I have an alternative approach in prot-notmuch.el.
  (setq prot-mail-maildir-path-regexp "~/.mail/*/Inbox/new/") ; shell regexp
  (setq prot-mail-mode-line-indicator-commands
        '(notmuch-refresh-this-buffer))
  ;; mode line indicator with the number of new mails
  (prot-mail-mail-indicator -1))

;;; Notmuch (mail indexer and mail user agent (MUA))
;; I install notmuch from the distro's repos because the CLI program is
;; not dependent on Emacs.  Though the package also includes notmuch.el
;; which is what we use here (they are maintained by the same people).
(add-to-list 'load-path "/usr/share/emacs/site-lisp/")
(prot-emacs-builtin-package 'notmuch

;;; Account settings
  (setq notmuch-identities
        (let ((identities))
          (dolist (m `(,(prot-mail-auth-get-field "prv" :user)
                       ,(prot-mail-auth-get-field "inf" :user)
                       ,(prot-mail-auth-get-field "pub" :user)))
            (push (format "%s <%s>" user-full-name m) identities))
          identities))
  (setq notmuch-fcc-dirs
        `((,(prot-mail-auth-get-field "prv" :user) . "prv/Sent")
          (,(prot-mail-auth-get-field "inf" :user) . "inf/Sent")
          (,(prot-mail-auth-get-field "pub" :user) . "pub/Sent")))

;;; General UI
  (setq notmuch-show-logo nil)
  (setq notmuch-column-control 1.0)
  (setq notmuch-hello-auto-refresh t)
  (setq notmuch-hello-recent-searches-max 20)
  (setq notmuch-hello-thousands-separator "")
  (setq notmuch-hello-sections '(notmuch-hello-insert-saved-searches notmuch-hello-insert-alltags))
  (setq notmuch-show-all-tags-list t)

;;; Search
  (setq notmuch-search-oldest-first nil)
  (setq notmuch-search-result-format
        '(("date" . "%12s  ")
          ("count" . "%-7s  ")
          ("authors" . "%-20s  ")
          ("subject" . "%-80s  ")
          ("tags" . "(%s)")))
  (setq notmuch-tree-result-format
        '(("date" . "%12s  ")
          ("authors" . "%-20s  ")
          ((("tree" . "%s")
            ("subject" . "%s"))
           . " %-80s  ")
          ("tags" . "(%s)")))
  (setq notmuch-search-line-faces
        '(("unread" . notmuch-search-unread-face)
          ("flag" . notmuch-search-flagged-face)))
  (setq notmuch-show-empty-saved-searches t)
  (setq notmuch-saved-searches
        `(( :name "inbox"
            :query "tag:inbox"
            :sort-order newest-first
            :key ,(kbd "i"))
          ( :name "unread (inbox)"
            :query "tag:unread and tag:inbox"
            :sort-order newest-first
            :key ,(kbd "u"))
          ( :name "unread all"
            :query "tag:unread not tag:archived"
            :sort-order newest-first
            :key ,(kbd "U"))
          ( :name "references"
            :query "tag:ref not tag:archived"
            :sort-order newest-first
            :key ,(kbd "r"))
          ( :name "todo"
            :query "tag:todo not tag:archived"
            :sort-order newest-first
            :key ,(kbd "t"))
          ( :name "mailing lists"
            :query "tag:list not tag:archived"
            :sort-order newest-first
            :key ,(kbd "m"))
          ;; Emacs
          ( :name "emacs-devel"
            :query "(from:emacs-devel@gnu.org or to:emacs-devel@gnu.org) not tag:archived"
            :sort-order newest-first
            :key ,(kbd "e d"))
          ( :name "emacs-orgmode"
            :query "(from:emacs-orgmode@gnu.org or to:emacs-orgmode@gnu.org) not tag:archived"
            :sort-order newest-first
            :key ,(kbd "e o"))
          ( :name "emacs-bugs"
            :query "'to:\"/*@debbugs.gnu.org*/\"' not tag:archived"
            :sort-order newest-first :key ,(kbd "e b"))
          ( :name "emacs-humanities"
            :query "(from:emacs-humanities@gnu.org or to:emacs-humanities@gnu.org) not tag:archived"
            :sort-order newest-first :key ,(kbd "e h"))
          ( :name "emacs-elpher"
            :query "(from:~michel-slm/elpher@lists.sr.ht or to:~michel-slm/elpher@lists.sr.ht) not tag:archived"
            :sort-order newest-first :key ,(kbd "e e"))
          ;; Others
          ( :name "notmuch"
            :query "(from:notmuch@notmuchmail.org or to:notmuch@notmuchmail.org) not tag:archived"
            :sort-order newest-first
            :key ,(kbd "on"))
          ( :name "sourcehut"
            :query "(from:~sircmpwn/sr.ht-discuss@lists.sr.ht or to:~sircmpwn/sr.ht-discuss@lists.sr.ht) not tag:archived"
            :sort-order newest-first
            :key ,(kbd "os"))))

;;; Tags
  (setq notmuch-archive-tags '("-inbox" "+archived"))
  (setq notmuch-message-replied-tags '("+replied"))
  (setq notmuch-message-forwarded-tags '("+forwarded"))
  (setq notmuch-show-mark-read-tags '("-unread"))
  (setq notmuch-draft-tags '("+draft"))
  (setq notmuch-draft-folder "drafts")
  (setq notmuch-draft-save-plaintext 'ask)
  ;; ;; NOTE 2021-06-18: See an updated version in the `prot-notmuch'
  ;; ;; section below.
  ;; (setq notmuch-tagging-keys
  ;;       `((,(kbd "a") notmuch-archive-tags "Archive (remove from inbox)")
  ;;         (,(kbd "c") ("+archived" "-inbox" "-list" "-todo" "-ref" "-unread") "Complete and archive")
  ;;         (,(kbd "d") ("+del" "-inbox" "-archived" "-unread") "Mark for deletion")
  ;;         (,(kbd "f") ("+flag" "-unread") "Flag as important")
  ;;         ;; (,(kbd "r") notmuch-show-mark-read-tags "Mark as read")
  ;;         (,(kbd "r") ("+ref" "-unread") "Reference for the future")
  ;;         (,(kbd "s") ("+spam" "+del" "-inbox" "-unread") "Mark as spam")
  ;;         (,(kbd "t") ("+todo" "-unread") "To-do")
  ;;         (,(kbd "u") ("+unread") "Mark as unread")))
  (setq notmuch-tag-formats
        '(("unread" (propertize tag 'face 'notmuch-tag-unread))
          ("flag" (propertize tag 'face 'notmuch-tag-flagged))))
  (setq notmuch-tag-deleted-formats
        '(("unread" (notmuch-apply-face bare-tag `notmuch-tag-deleted))
          (".*" (notmuch-apply-face tag `notmuch-tag-deleted))))

;;; Email composition
  (setq notmuch-mua-compose-in 'current-window)
  (setq notmuch-mua-hidden-headers nil) ; TODO 2021-05-12: Review hidden headers
  (setq notmuch-address-command nil)    ; FIXME 2021-05-13: Make it work with EBDB
  (setq notmuch-always-prompt-for-sender t)
  (setq notmuch-mua-cite-function 'message-cite-original-without-signature)
  (setq notmuch-mua-reply-insert-header-p-function 'notmuch-show-reply-insert-header-p-never)
  (setq notmuch-mua-user-agent-function #'notmuch-mua-user-agent-full)
  (setq notmuch-maildir-use-notmuch-insert t)
  (setq notmuch-crypto-process-mime t)
  (setq notmuch-crypto-get-keys-asynchronously t)
  (setq notmuch-mua-attachment-regexp   ; see `notmuch-mua-send-hook'
        (concat "\\b\\(attache\?ment\\|attached\\|attach\\|"
                "pi[èe]ce\s+jointe?\\|"
                "συνημμ[εέ]νο\\|επισυν[αά]πτω\\)\\b"))

;;; Reading messages
  (setq notmuch-show-relative-dates t)
  (setq notmuch-show-all-multipart/alternative-parts nil)
  (setq notmuch-show-indent-messages-width 0)
  (setq notmuch-show-indent-multipart nil)
  (setq notmuch-show-part-button-default-action 'notmuch-show-save-part)
  (setq notmuch-show-text/html-blocked-images ".") ; block everything
  (setq notmuch-wash-citation-lines-prefix 6)
  (setq notmuch-wash-citation-lines-suffix 6)
  (setq notmuch-wash-wrap-lines-length 100)
  (setq notmuch-unthreaded-show-out nil)
  (setq notmuch-message-headers '("To" "Cc" "Subject" "Date"))
  (setq notmuch-message-headers-visible t)

;;; Hooks and key bindings
  (add-hook 'notmuch-mua-send-hook #'notmuch-mua-attachment-check)
  (remove-hook 'notmuch-show-hook #'notmuch-show-turn-on-visual-line-mode)
  (remove-hook 'notmuch-search-hook 'notmuch-hl-line-mode) ; Check my `lin' package
  (add-hook 'notmuch-show-hook (lambda () (setq-local header-line-format nil)))

  (let ((map global-map))
    (define-key map (kbd "C-c m") #'notmuch)
    (define-key map (kbd "C-x m") #'notmuch-mua-new-mail)) ; override `compose-mail'
  (let ((map notmuch-search-mode-map)) ; I normally don't use the tree view, otherwise check `notmuch-tree-mode-map'
    (define-key map (kbd "/") #'notmuch-search-filter) ; alias for l
    (define-key map (kbd "r") #'notmuch-search-reply-to-thread) ; easier to reply to all by default
    (define-key map (kbd "R") #'notmuch-search-reply-to-thread-sender))
  (let ((map notmuch-show-mode-map))
    (define-key map (kbd "r") #'notmuch-show-reply) ; easier to reply to all by default
    (define-key map (kbd "R") #'notmuch-show-reply-sender))
  (define-key notmuch-hello-mode-map (kbd "C-<tab>") nil))

(prot-emacs-builtin-package 'prot-notmuch
  ;; Those are for the actions that are available after pressing 'k'
  ;; (`notmuch-tag-jump').  For direct actions, refer to the key
  ;; bindings below.
  (setq notmuch-tagging-keys
        `((,(kbd "a") notmuch-archive-tags "Archive (remove from inbox)")
          (,(kbd "c") prot-notmuch-mark-complete-tags "Complete and archive")
          (,(kbd "d") prot-notmuch-mark-delete-tags "Mark for deletion")
          (,(kbd "f") prot-notmuch-mark-flag-tags "Flag as important")
          (,(kbd "s") prot-notmuch-mark-spam-tags "Mark as spam")
          (,(kbd "t") prot-notmuch-mark-todo-tags "To-do")
          (,(kbd "x") prot-notmuch-mark-reference-tags "Reference for the future")
          (,(kbd "r") ("-unread") "Mark as read")
          (,(kbd "u") ("+unread") "Mark as unread")))

  (add-to-list 'notmuch-tag-formats
               '("encrypted" (propertize tag 'face 'prot-notmuch-encrypted-tag)))
  (add-to-list 'notmuch-tag-formats
               '("sent" (propertize tag 'face 'prot-notmuch-sent-tag)))
  (add-to-list 'notmuch-tag-formats
               '("ref" (propertize tag 'face 'prot-notmuch-ref-tag)))
  (add-to-list 'notmuch-tag-formats
               '("todo" (propertize tag 'face 'prot-notmuch-todo-tag)))
  (add-to-list 'notmuch-tag-formats
               '("spam" (propertize tag 'face 'prot-notmuch-spam-tag)))

  ;; NOTE 2021-05-14: I have an alternative method of finding new mail
  ;; in a maildir tree by using the find command.  It is somewhat
  ;; simplistic, though it worked just fine: see prot-mail.el.  I prefer
  ;; this implementation instead, as it leverages notmuch and so I can
  ;; pass arbitrary search terms to it.
  (setq prot-notmuch-mode-line-count-args "tag:unread and tag:inbox")
  (setq prot-notmuch-mode-line-indicator-commands
        '(notmuch notmuch-refresh-this-buffer))
  ;; Mode line indicator with the number of new mails.
  (prot-notmuch-mail-indicator 1)

  (dolist (fn '(prot-notmuch-check-valid-sourcehut-email
                prot-notmuch-ask-sourcehut-control-code))
    (add-hook 'notmuch-mua-send-hook fn))

  (let ((map notmuch-search-mode-map))
    (define-key map (kbd "a") nil) ; the default is too easy to hit accidentally
    (define-key map (kbd "A") #'notmuch-search-archive-thread)
    (define-key map (kbd "D") #'prot-notmuch-search-delete-thread)
    (define-key map (kbd "T") #'prot-notmuch-search-todo-thread)
    (define-key map (kbd "X") #'prot-notmuch-search-reference-thread)
    (define-key map (kbd "C") #'prot-notmuch-search-complete-thread)
    (define-key map (kbd "S") #'prot-notmuch-search-spam-thread)
    (define-key map (kbd "g") #'prot-notmuch-refresh-buffer))
  (let ((map notmuch-show-mode-map))
    (define-key map (kbd "a") nil) ; the default is too easy to hit accidentally
    (define-key map (kbd "A") #'notmuch-show-archive-message-then-next-or-next-thread)
    (define-key map (kbd "D") #'prot-notmuch-show-delete-message)
    (define-key map (kbd "T") #'prot-notmuch-show-todo-message)
    (define-key map (kbd "X") #'prot-notmuch-show-reference-message)
    (define-key map (kbd "C") #'prot-notmuch-show-complete-message)
    (define-key map (kbd "S") #'prot-notmuch-show-spam-message))
  (define-key notmuch-show-stash-map (kbd "S") #'prot-notmuch-stash-sourcehut-link)
  ;; Like C-c M-h for `message-insert-headers'
  (define-key notmuch-message-mode-map (kbd "C-c M-e") #'prot-notmuch-patch-add-email-control-code))

(prot-emacs-elpa-package 'ol-notmuch)

;;; Sending email (SMTP)
(prot-emacs-builtin-package 'smtpmail
  (setq smtpmail-default-smtp-server "mail.gandi.net")
  (setq smtpmail-smtp-server "mail.gandi.net")
  (setq smtpmail-stream-type 'ssl)
  (setq smtpmail-smtp-service 465)
  (setq smtpmail-queue-mail nil))

(prot-emacs-builtin-package 'sendmail
  (setq send-mail-function 'smtpmail-send-it))

;;; EBDB (mail contacts)
(prot-emacs-elpa-package 'ebdb
  (require 'ebdb-message)
  (require 'ebdb-notmuch) ; FIXME 2021-05-13: does not activate the corfu-mode UI
  (setq ebdb-sources (locate-user-emacs-file "ebdb"))
  (setq ebdb-permanent-ignores-file (locate-user-emacs-file "ebdb-permanent-ignores"))

  (setq ebdb-mua-pop-up nil)
  (setq ebdb-default-window-size 0.25)
  (setq ebdb-mua-default-formatter ebdb-default-multiline-formatter)

  (setq ebdb-mua-auto-update-p 'existing)
  (setq ebdb-mua-reader-update-p 'existing)
  (setq ebdb-mua-sender-update-p 'create)
  (setq ebdb-message-auto-update-p 'create)

  (setq ebdb-message-try-all-headers t)
  (setq ebdb-message-headers
        '((sender "From" "Resent-From" "Reply-To" "Sender")
          (recipients "Resent-To" "Resent-Cc" "Resent-CC" "To" "Cc" "CC" "Bcc" "BCC")))
  (setq ebdb-message-all-addresses t)

  (setq ebdb-complete-mail 'capf)
  (setq ebdb-mail-avoid-redundancy t)
  (setq ebdb-completion-display-record nil)
  (setq ebdb-complete-mail-allow-cycling nil)

  (setq ebdb-record-self "ace719a4-61f8-4bee-a1ca-2f07e2292305")
  (setq ebdb-user-name-address-re 'self) ; match the above
  (setq ebdb-save-on-exit t)

  (with-eval-after-load 'prot-mail ; check my `prot-mail.el'
    (add-hook 'message-setup-hook #'prot-mail-ebdb-message-setup))

  (let ((map ebdb-mode-map))
    (define-key map (kbd "D") #'ebdb-delete-field-or-record)
    (define-key map (kbd "M") #'ebdb-mail) ; disables `ebdb-mail-each'
    (define-key map (kbd "m") #'ebdb-toggle-record-mark)
    (define-key map (kbd "t") #'ebdb-toggle-all-record-marks)
    (define-key map (kbd "T") #'ebdb-toggle-records-format) ; disables `ebdb-toggle-all-records-format'
    (define-key map (kbd "U") #'ebdb-unmark-all-records)))

(prot-emacs-builtin-package 'rcirc
  (setq rcirc-server-alist
        `(("irc.libera.chat"
           :channels ("#emacs" "#org-mode" "#rcirc" "#sr.ht")
           :port 6697 :encryption tls
           :password ,(prot-mail-auth-get-field "libera" :secret))))

  (setq rcirc-prompt "%t> ") ; Read the docs or use (customize-set-variable 'rcirc-prompt "%t> ")

  (setq rcirc-default-nick "protesilaos"
        rcirc-default-user-name rcirc-default-nick
        rcirc-default-full-name "Protesilaos Stavrou")

  ;; ;; NOTE 2021-11-28: demo from the days of EmacsConf 2021.  I don't
  ;; ;; actually need this.
  ;; (setq rcirc-bright-nicks '("bandali" "sachac" "zaeph"))

  ;; NOTE 2021-11-28: Is there a canonical way to disable this?
  (setq rcirc-timeout-seconds most-positive-fixnum)

  (rcirc-track-minor-mode 1)

  (define-key global-map (kbd "C-c i") #'irc))

;;; Bongo music or media manager (and prot-bongo.el)
(prot-emacs-elpa-package 'bongo
  (setq bongo-default-directory "~/Music/")
  (setq bongo-prefer-library-buffers nil)
  (setq bongo-insert-whole-directory-trees t)
  (setq bongo-logo nil)
  (setq bongo-display-track-icons nil)
  (setq bongo-display-track-lengths nil)
  (setq bongo-display-header-icons nil)
  (setq bongo-display-playback-mode-indicator t)
  (setq bongo-display-inline-playback-progress nil) ; t slows down the playlist buffer
  (setq bongo-join-inserted-tracks nil)
  (setq bongo-field-separator (propertize " · " 'face 'shadow))
  (setq bongo-mark-played-tracks t)
  (setq bongo-vlc-program-name "cvlc")
  (bongo-mode-line-indicator-mode -1)
  (bongo-header-line-mode -1)
  (let ((map global-map))
    (define-key map (kbd "C-c b") #'bongo)
    (define-key map (kbd "<C-XF86AudioPlay>") #'bongo-pause/resume)
    (define-key map (kbd "<C-XF86AudioNext>") #'bongo-next)
    (define-key map (kbd "<C-XF86AudioPrev>") #'bongo-previous)
    (define-key map (kbd "<C-M-XF86AudioPlay>") #'bongo-play-random)
    (define-key map (kbd "<M-XF86AudioPlay>") #'bongo-show)
    (define-key map (kbd "<S-XF86AudioNext>") #'bongo-seek-forward-10)
    (define-key map (kbd "<S-XF86AudioPrev>") #'bongo-seek-backward-10)
    ;; Same as above for the pgtk build of Emacs 29.  Only tested it
    ;; with SwayWM.  GNOME 42 may have its own bindings for the
    ;; multimedia keys that require changes at the level of the desktop
    ;; environment.
    (define-key map (kbd "C-<AudioPlay>") #'bongo-pause/resume)
    (define-key map (kbd "C-<AudioNext>") #'bongo-next)
    (define-key map (kbd "C-<AudioPrev>") #'bongo-previous)
    (define-key map (kbd "C-M-<AudioPlay>") #'bongo-play-random)
    (define-key map (kbd "M-<AudioPlay>") #'bongo-show)
    (define-key map (kbd "S-<AudioNext>") #'bongo-seek-forward-10)
    (define-key map (kbd "S-<AudioPrev>") #'bongo-seek-backward-10))
  (let ((map bongo-playlist-mode-map))
    (define-key map (kbd "n") #'bongo-next-object)
    (define-key map (kbd "p") #'bongo-previous-object)
    (define-key map (kbd "R") #'bongo-rename-line)
    (define-key map (kbd "j") #'bongo-dired-line)       ; Jump to dir of file at point
    (define-key map (kbd "J") #'dired-jump)             ; Jump to library buffer
    (define-key map (kbd "I") #'bongo-insert-special)))

(with-eval-after-load 'bongo
  (prot-emacs-builtin-package 'prot-bongo
    (setq prot-bongo-enabled-backends '(mpv vlc))
    (setq prot-bongo-playlist-section-delimiter (make-string 30 ?*))
    (setq prot-bongo-playlist-heading-delimiter "§")
    (setq prot-bongo-playlist-directory
          (concat
           (file-name-as-directory bongo-default-directory)
           (file-name-as-directory "playlists")))
    ;; Those set up a few extras: read each function's doc string.  Pass
    ;; an argument to undo their effects.
    (prot-bongo-enabled-backends)
    (prot-bongo-remove-headers)
    (prot-bongo-imenu-setup)
    (add-hook 'dired-mode-hook #'prot-bongo-dired-library-enable)
    (add-hook 'wdired-mode-hook #'prot-bongo-dired-library-disable)
    (add-hook 'prot-bongo-playlist-change-track-hook #'prot-bongo-playlist-recenter)
    (let ((map bongo-playlist-mode-map))
      (define-key map (kbd "C-c C-n") #'prot-bongo-playlist-heading-next)
      (define-key map (kbd "C-c C-p") #'prot-bongo-playlist-heading-previous)
      (define-key map (kbd "M-n") #'prot-bongo-playlist-section-next)
      (define-key map (kbd "M-p") #'prot-bongo-playlist-section-previous)
      (define-key map (kbd "M-h") #'prot-bongo-playlist-mark-section)
      (define-key map (kbd "M-d") #'prot-bongo-playlist-kill-section)
      (define-key map (kbd "g") #'prot-bongo-playlist-reset)
      (define-key map (kbd "D") #'prot-bongo-playlist-terminate)
      (define-key map (kbd "r") #'prot-bongo-playlist-random-toggle)
      (define-key map (kbd "i") #'prot-bongo-playlist-insert-playlist-file))
    (let ((map bongo-dired-library-mode-map))
      (define-key map (kbd "<C-return>") #'prot-bongo-dired-insert)
      (define-key map (kbd "C-c SPC") #'prot-bongo-dired-insert)
      (define-key map (kbd "C-c +") #'prot-bongo-dired-make-playlist-file))))

;;; Elfeed feed reader, prot-elfeed.el and prot-elfeed-bongo.el
(prot-emacs-elpa-package 'elfeed
  (setq elfeed-use-curl t)
  (setq elfeed-curl-max-connections 10)
  (setq elfeed-db-directory (concat user-emacs-directory "elfeed/"))
  (setq elfeed-enclosure-default-dir "~/Downloads/")
  (setq elfeed-search-filter "@4-months-ago +unread")
  (setq elfeed-sort-order 'descending)
  (setq elfeed-search-clipboard-type 'CLIPBOARD)
  (setq elfeed-search-title-max-width 100)
  (setq elfeed-search-title-min-width 30)
  (setq elfeed-search-trailing-width 25)
  (setq elfeed-show-truncate-long-urls t)
  (setq elfeed-show-unique-buffers t)
  (setq elfeed-search-date-format '("%F %R" 16 :left))

  ;; Make sure to also check the section on shr and eww for how I handle
  ;; `shr-width' there.
  (add-hook 'elfeed-show-mode-hook
            (lambda () (setq-local shr-width (current-fill-column))))

  (prot-emacs-builtin-package 'prot-elfeed-bongo)

  (define-key global-map (kbd "C-c e") #'elfeed)
  (let ((map elfeed-search-mode-map))
    (define-key map (kbd "w") #'elfeed-search-yank)
    (define-key map (kbd "g") #'elfeed-update)
    (define-key map (kbd "G") #'elfeed-search-update--force)
    (define-key map (kbd "b") #'prot-elfeed-bongo-insert-item)
    (define-key map (kbd "h") #'prot-elfeed-bongo-switch-to-playlist)) ; "hop" mnemonic
  (let ((map elfeed-show-mode-map))
    (define-key map (kbd "w") #'elfeed-show-yank)
    (define-key map (kbd "b") #'prot-elfeed-bongo-insert-item)))

(with-eval-after-load 'elfeed
  (prot-emacs-builtin-package 'prot-elfeed
    (setq prot-elfeed-tag-faces t)
    (prot-elfeed-fontify-tags)
    (add-hook 'elfeed-search-mode-hook #'prot-elfeed-load-feeds)

    (let ((map elfeed-search-mode-map))
      (define-key map (kbd "s") #'prot-elfeed-search-tag-filter)
      (define-key map (kbd "o") #'prot-elfeed-search-open-other-window)
      (define-key map (kbd "q") #'prot-elfeed-kill-buffer-close-window-dwim)
      (define-key map (kbd "v") #'prot-elfeed-mpv-dwim)
      (define-key map (kbd "+") #'prot-elfeed-toggle-tag))
    (let ((map elfeed-show-mode-map))
      (define-key map (kbd "a") #'prot-elfeed-show-archive-entry)
      (define-key map (kbd "e") #'prot-elfeed-show-eww)
      (define-key map (kbd "q") #'prot-elfeed-kill-buffer-close-window-dwim)
      (define-key map (kbd "v") #'prot-elfeed-mpv-dwim)
      (define-key map (kbd "+") #'prot-elfeed-toggle-tag))))

;;; Proced (process monitor, similar to `top')
(prot-emacs-builtin-package 'proced
  (setq proced-auto-update-flag t)
  (setq proced-auto-update-interval 5)
  (setq proced-descend t)
  (setq proced-filter 'user))

(prot-emacs-builtin-package 'prot-proced
  (prot-proced-extra-keywords 1))

;;; Pass interface (password-store)
(prot-emacs-elpa-package 'password-store
  (setq password-store-time-before-clipboard-restore 30)
  ;; Mnemonic is the root of the "code" word (κώδικας).  But also to add
  ;; the password to the kill-ring.  Other options are already taken.
  (define-key global-map (kbd "C-c k") #'password-store-copy))

(prot-emacs-elpa-package 'pass)

;;; Simple HTML Renderer (shr), Emacs Web Wowser (eww), Elpher, and prot-eww.el
(prot-emacs-builtin-package 'browse-url
  (setq browse-url-browser-function 'eww-browse-url)
  (setq browse-url-secondary-browser-function 'browse-url-default-browser))

(prot-emacs-builtin-package 'shr
  (setq shr-use-colors nil)             ; t is bad for accessibility
  (setq shr-use-fonts nil)              ; t is not for me
  (setq shr-max-image-proportion 0.6)
  (setq shr-image-animate nil)          ; No GIFs, thank you!
  (setq shr-width nil)                  ; check `prot-eww-readable'
  (setq shr-discard-aria-hidden t)
  (setq shr-cookie-policy nil))

(prot-emacs-builtin-package 'url-cookie
  (setq url-cookie-untrusted-urls '(".*")))

(prot-emacs-builtin-package 'eww
  (setq eww-restore-desktop t)
  (setq eww-desktop-remove-duplicates t)
  (setq eww-header-line-format nil)
  (setq eww-search-prefix "https://duckduckgo.com/html/?q=")
  (setq eww-download-directory (expand-file-name "~/Documents/eww-downloads"))
  (setq eww-suggest-uris
        '(eww-links-at-point
          thing-at-point-url-at-point))
  (setq eww-bookmarks-directory (locate-user-emacs-file "eww-bookmarks/"))
  (setq eww-history-limit 150)
  (setq eww-use-external-browser-for-content-type
        "\\`\\(video/\\|audio\\)") ; On GNU/Linux check your mimeapps.list
  (setq eww-browse-url-new-window-is-tab nil)
  (setq eww-form-checkbox-selected-symbol "[X]")
  (setq eww-form-checkbox-symbol "[ ]")
  ;; NOTE `eww-retrieve-command' is for Emacs28.  I tried the following
  ;; two values.  The first would not render properly some plain text
  ;; pages, such as by messing up the spacing between paragraphs.  The
  ;; second is more reliable but feels slower.  So I just use the
  ;; default (nil), though I find wget to be a bit faster.  In that case
  ;; one could live with the occasional errors by using `eww-download'
  ;; on the offending page, but I prefer consistency.
  ;;
  ;; '("wget" "--quiet" "--output-document=-")
  ;; '("chromium" "--headless" "--dump-dom")
  (setq eww-retrieve-command nil)

  (define-key eww-link-keymap (kbd "v") nil) ; stop overriding `eww-view-source'
  (define-key eww-mode-map (kbd "L") #'eww-list-bookmarks)
  (define-key dired-mode-map (kbd "E") #'eww-open-file) ; to render local HTML files
  (define-key eww-buffers-mode-map (kbd "d") #'eww-bookmark-kill)   ; it actually deletes
  (define-key eww-bookmark-mode-map (kbd "d") #'eww-bookmark-kill)) ; same

(prot-emacs-elpa-package 'elpher)    ; NOTE 2021-07-24: work-in-progress

(prot-emacs-builtin-package 'prot-eww
  (setq prot-eww-save-history-file
        (locate-user-emacs-file "prot-eww-visited-history"))
  (setq prot-eww-save-visited-history t)
  (setq prot-eww-bookmark-link nil)

  (add-hook 'prot-eww-history-mode-hook #'hl-line-mode)

  (define-prefix-command 'prot-eww-map)
  (define-key global-map (kbd "C-c w") 'prot-eww-map)
  (let ((map prot-eww-map))
    (define-key map (kbd "b") #'prot-eww-visit-bookmark)
    (define-key map (kbd "e") #'prot-eww-browse-dwim)
    (define-key map (kbd "s") #'prot-eww-search-engine))
  (let ((map eww-mode-map))
    (define-key map (kbd "B") #'prot-eww-bookmark-page)
    (define-key map (kbd "D") #'prot-eww-download-html)
    (define-key map (kbd "F") #'prot-eww-find-feed)
    (define-key map (kbd "H") #'prot-eww-list-history)
    (define-key map (kbd "b") #'prot-eww-visit-bookmark)
    (define-key map (kbd "e") #'prot-eww-browse-dwim)
    (define-key map (kbd "o") #'prot-eww-open-in-other-window)
    (define-key map (kbd "E") #'prot-eww-visit-url-on-page)
    (define-key map (kbd "J") #'prot-eww-jump-to-url-on-page)
    (define-key map (kbd "R") #'prot-eww-readable)
    (define-key map (kbd "Q") #'prot-eww-quit)))

(prot-emacs-elpa-package 'osm
  (let ((map global-map))
    (define-key map (kbd "C-c o h") #'osm-home)
    (define-key map (kbd "C-c o s") #'osm-search)
    (define-key map (kbd "C-c o t") #'osm-server)
    (define-key map (kbd "C-c o g") #'osm-goto)
    (define-key map (kbd "C-c o j") #'osm-bookmark-jump))

  ;; Load Org link support
  (with-eval-after-load 'org
    (require 'osm-ol)))

;;; Go to last change
(prot-emacs-elpa-package 'goto-last-change
  (define-key global-map (kbd "C-z") #'goto-last-change))

;;; Cycle between related buffers (transient-cycles.el)
(prot-emacs-elpa-package 'transient-cycles
  (setq transient-cycles-show-cycling-keys t)
  ;; (setq transient-cycles-default-cycle-backwards-key [left])
  ;; (setq transient-cycles-default-cycle-forwards-key [right])
  ;; (setq transient-cycles-buffer-siblings-cycle-backwards-key [left])
  ;; (setq transient-cycles-buffer-siblings-cycle-forwards-key [right])
  ;; (setq transient-cycles-window-buffers-cycle-backwards-key [left])
  ;; (setq transient-cycles-window-buffers-cycle-forwards-key [right])
  ;; (setq transient-cycles-tab-bar-cycle-backwards-key [left])
  ;; (setq transient-cycles-tab-bar-cycle-forwards-key [right])
  (setq transient-cycles-buffer-siblings-major-modes
        '(("\\*.*eshell.*" . eshell-mode)
          ("\\*.*shell.*" . shell-mode)
          (".*vc-dir.*" . vc-dir-mode)
          ("^magit: .*" . magit-status-mode)
          ("\\*.*eww.*\\*" . eww-mode)
          ("\\*Embark Collect: .*" . embark-collect-mode)))

  (transient-cycles-buffer-siblings-mode 1)
  (transient-cycles-window-buffers-mode 1)
  (transient-cycles-tab-bar-mode 1))

(prot-emacs-elpa-package 'avy
  (setq avy-all-windows nil) ; only the current window
  (setq avy-all-windows-alt t) ; all windows with C-u
  (setq avy-single-candidate-jump t)
  (setq avy-background nil)
  (setq avy-case-fold-search nil) ; case is significant
  (setq avy-timeout-seconds 0.5)
  (setq avy-style 'pre) ; prefixes candidate; otherwise use `at-full'
  (define-key global-map (kbd "C-.") #'avy-goto-char-timer))

;;; Mode line
(setq mode-line-percent-position '(-3 "%p"))
(setq mode-line-position-column-line-format '(" %l,%c")) ; Emacs 28
(setq mode-line-defining-kbd-macro
      (propertize " Macro" 'face 'mode-line-emphasis))

;; Thanks to Daniel Mendler for this!  It removes the square brackets
;; that denote recursive edits in the modeline.  I do not need them
;; because I am using Daniel's `recursion-indicator':
;; <https://github.com/minad/recursion-indicator>.
(setq-default mode-line-modes
              (seq-filter (lambda (s)
                            (not (and (stringp s)
                                      (string-match-p
                                       "^\\(%\\[\\|%\\]\\)$" s))))
                          mode-line-modes))

(setq mode-line-compact nil)            ; Emacs 28
(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-remote
                mode-line-frame-identification
                mode-line-buffer-identification
                "  "
                mode-line-position
                mode-line-modes
                "  "
                (vc-mode vc-mode)
                "  "
                mode-line-misc-info
                mode-line-end-spaces))

;;; Moody.el (simple mode line configuration utility)
(prot-emacs-elpa-package 'moody)

(prot-emacs-builtin-package 'prot-moody
  ;; Adjust this and then evaluate `prot-moody-set-height'.  Not all
  ;; fonts work equally well with the same value.
  (setq prot-moody-font-height-multiplier 1.35)

  ;; Also check the Modus themes' `modus-themes-mode-line' which can set
  ;; the styles specifically for Moody.
  (prot-moody-set-height -1))

;;; Hide modeline "lighters" (minions.el)
(prot-emacs-elpa-package 'minions
  (setq minions-mode-line-lighter ";")
  ;; NOTE: This will be expanded whenever I find a mode that should not
  ;; be hidden
  (setq minions-prominent-modes
        (list 'defining-kbd-macro
              'flymake-mode
              'prot-simple-monocle))
  (minions-mode 1))

;;; Mode line recursion indicators
(prot-emacs-elpa-package 'recursion-indicator
  (setq recursion-indicator-general "&")
  (setq recursion-indicator-minibuffer "@")
  (recursion-indicator-mode 1))

;;; Display current time
(prot-emacs-builtin-package 'time
  (setq display-time-format "%a %e %b, %H:%M")
  ;;;; Covered by `display-time-format'
  ;; (setq display-time-24hr-format t)
  ;; (setq display-time-day-and-date t)
  (setq display-time-interval 60)
  (setq display-time-default-load-average nil)
  ;; NOTE 2021-04-19: For all those, I have implemented a custom
  ;; solution that also shows the number of new items.  Refer to my
  ;; email settings, specifically `prot-mail-mail-indicator'.
  ;;
  ;; NOTE 2021-05-16: Or better check `prot-notmuch-mail-indicator'.
  (setq display-time-mail-directory nil)
  (setq display-time-mail-function nil)
  (setq display-time-use-mail-icon nil)
  (setq display-time-mail-string nil)
  (setq display-time-mail-face nil)

;;; World clock
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
          ("Asia/Vladivostok" "Vladivostok")))

  ;; All of the following variables are for Emacs 28
  (setq world-clock-list t)
  (setq world-clock-time-format "%R %z  %A %d %B")
  (setq world-clock-buffer-name "*world-clock*") ; Placement handled by `display-buffer-alist'
  (setq world-clock-timer-enable t)
  (setq world-clock-timer-second 60)

  ;; ;; NOTE 2021-10-04: Check `prot-tab-status-line'.
  ;; (add-hook 'after-init-hook #'display-time-mode)
  )

;;; Keycast mode
(prot-emacs-elpa-package 'keycast
  ;; Those are for `keycast-mode'
  (setq keycast-mode-line-window-predicate 'moody-window-active-p) ; assumes `moody.el'
  (setq keycast-separator-width 1)
  (setq keycast-mode-line-remove-tail-elements nil)

  (dolist (input '(self-insert-command
                   org-self-insert-command))
    (add-to-list 'keycast-substitute-alist `(,input "." "Typing…")))

  (dolist (event '(mouse-event-p
                   mouse-movement-p
                   mwheel-scroll))
    (add-to-list 'keycast-substitute-alist `(,event nil)))

  ;; Those are for the `keycast-log-mode'
  (setq keycast-log-format "%-20K%C\n")
  (setq keycast-log-frame-alist
        '((minibuffer . nil)))
  (setq keycast-log-newest-first t)

  ;; Specify `keycast-insert-after' buffer identification.  This make it
  ;; possible to seamlessly toggle `prot-moody-set-height' without
  ;; disrupting keycast.
  (with-eval-after-load 'prot-moody
    (add-hook 'prot-moody-set-height-hook #'prot-moody-keycast-insert-after)))

;;; Window divider mode
(setq window-divider-default-right-width 1)
(setq window-divider-default-bottom-width 1)
(setq window-divider-default-places 'right-only)

;;; Fringe mode
(prot-emacs-builtin-package 'fringe
  (fringe-mode nil)
  (setq-default fringes-outside-margins nil)
  (setq-default indicate-buffer-boundaries nil)
  (setq-default indicate-empty-lines nil)
  (setq-default overflow-newline-into-fringe t))

;;; Color tools (ct.el)
(prot-emacs-elpa-package 'ct)

;;; Rainbow mode for colour previewing (rainbow-mode.el)
(prot-emacs-elpa-package 'rainbow-mode
  (setq rainbow-ansi-colors nil)
  (setq rainbow-x-colors nil))

;;; Depth-based code colourisation (prism.el)
(prot-emacs-elpa-package 'prism
  (setq prism-comments t))

(prot-emacs-builtin-package 'prot-prism
  (setq prot-prism-negative-space-sensitive-modes
        '(sh-mode yaml-mode))

  (setq prot-prism-presets-function #'prot-prism--colors))

;;; Line numbers and relevant indicators (prot-sideline.el)
(prot-emacs-builtin-package 'prot-sideline
  (require 'display-line-numbers)
  ;; Set absolute line numbers.  A value of "relative" is also useful.
  (setq display-line-numbers-type t)
  ;; Those two variables were introduced in Emacs 27.1
  (setq display-line-numbers-major-tick 0)
  (setq display-line-numbers-minor-tick 0)
  ;; Use absolute numbers in narrowed buffers
  (setq-default display-line-numbers-widen t)

  (prot-emacs-elpa-package 'diff-hl
    (setq diff-hl-draw-borders nil)
    (setq diff-hl-side 'left))

  (require 'hl-line)
  (setq hl-line-sticky-flag nil)
  (setq hl-line-overlay-priority -50) ; emacs28

  (require 'whitespace)

  (let ((map global-map))
    (define-key map (kbd "<f6>") #'prot-sideline-negative-space-toggle)
    (define-key map (kbd "<f7>") #'prot-sideline-mode)
    (define-key map (kbd "C-c z") #'delete-trailing-whitespace)))

;;; Outline mode, outline minor mode, and extras (prot-outline.el)
(prot-emacs-builtin-package 'outline
  (setq outline-minor-mode-highlight 'override) ; emacs28
  (setq outline-minor-mode-cycle t)             ; emacs28
  (let ((map outline-minor-mode-map))
    ;; ;; NOTE 2021-07-25: Those two are already defined (emacs28).
    ;; (define-key map (kbd "TAB") #'outline-cycle)
    ;; (define-key map (kbd "<backtab>") #'outline-cycle-buffer) ; S-TAB
    (define-key map (kbd "C-c C-n") #'outline-next-visible-heading)
    (define-key map (kbd "C-c C-p") #'outline-previous-visible-heading)
    (define-key map (kbd "C-c C-f") #'outline-forward-same-level)
    (define-key map (kbd "C-c C-b") #'outline-backward-same-level)
    (define-key map (kbd "C-c C-a") #'outline-show-all)
    (define-key map (kbd "C-c C-o") #'outline-hide-other)
    (define-key map (kbd "C-c C-u") #'outline-up-heading)))

(prot-emacs-builtin-package 'prot-outline
  (let ((map outline-minor-mode-map))
    (define-key map (kbd "C-c C-v") #'prot-outline-move-major-heading-down)
    (define-key map (kbd "M-<down>") #'prot-outline-move-major-heading-down)
    (define-key map (kbd "C-c M-v") #'prot-outline-move-major-heading-up)
    (define-key map (kbd "M-<up>") #'prot-outline-move-major-heading-up)
    (define-key map (kbd "C-x n s") #'prot-outline-narrow-to-subtree))
  (define-key global-map (kbd "<f10>") #'prot-outline-minor-mode-safe))

;;; Cursor appearance (cursory.el)
(prot-emacs-builtin-package 'cursory
  (setq cursory-presets
        '((bar
           :cursor-type (bar . 2)
           :cursor-in-non-selected-windows hollow
           :blink-cursor-blinks 10
           :blink-cursor-interval 0.5
           :blink-cursor-delay 0.2)
          (box
           :cursor-type box
           :cursor-in-non-selected-windows hollow
           :blink-cursor-blinks 10
           :blink-cursor-interval 0.5
           :blink-cursor-delay 0.2)
          (underscore
           :cursor-type (hbar . 3)
           :cursor-in-non-selected-windows hollow
           :blink-cursor-blinks 50
           :blink-cursor-interval 0.2
           :blink-cursor-delay 0.2)))

  (setq cursory-latest-state-file (locate-user-emacs-file "cursory-latest-state.eld"))

  ;; Set last preset or fall back to desired style from `cursory-presets'.
  (cursory-set-preset (or (cursory-restore-latest-preset) 'bar))

  ;; The other side of `cursory-restore-latest-preset'.
  (add-hook 'kill-emacs-hook #'cursory-store-latest-preset)

  ;; We have to use the "point" mnemonic, because C-c c is often the
  ;; suggested binding for `org-capture'.
  (define-key global-map (kbd "C-c p") #'cursory-set-preset))

;;; Mouse wheel behaviour
(prot-emacs-builtin-package 'mouse
  ;; In Emacs 27+, use Control + mouse wheel to scale text.
  (setq mouse-wheel-scroll-amount
        '(1
          ((shift) . 5)
          ((meta) . 0.5)
          ((control) . text-scale)))
  (setq mouse-drag-copy-region nil)
  (setq make-pointer-invisible t)
  (setq mouse-wheel-progressive-speed t)
  (setq mouse-wheel-follow-mouse t)
  (add-hook 'after-init-hook #'mouse-wheel-mode)
  (define-key global-map (kbd "C-M-<mouse-3>") #'tear-off-window))

;;; Scrolling behaviour
;; These four come from the C source code.
(setq-default scroll-preserve-screen-position t)
(setq-default scroll-conservatively 1) ; affects `scroll-step'
(setq-default scroll-margin 0)
(setq-default next-screen-context-lines 0)

;;; Delete selection
(prot-emacs-builtin-package 'delsel
  (add-hook 'after-init-hook #'delete-selection-mode))

;;; Tooltips (tooltip-mode)
(prot-emacs-builtin-package 'tooltip
  (setq tooltip-delay 0.5)
  (setq tooltip-short-delay 0.5)
  (setq x-gtk-use-system-tooltips nil)
  (setq tooltip-frame-parameters
        '((name . "tooltip")
          (internal-border-width . 6)
          (border-width . 0)
          (no-special-glyphs . t)))
  (add-hook 'after-init-hook #'tooltip-mode))

;;; Dired-like list for registers (rlist)
;; Project repo: <https://gitlab.com/mmemmew/rlist>.  This is one of the
;; packages I handle manually via git, at least until it becomes
;; available through an ELPA.
;;
;; `prot-emacs-manual-package' is defined in my init.el
(prot-emacs-manual-package 'rlist
  (setq rlist-expert t)
  (setq rlist-verbose t)
  (let ((map global-map))
    (define-key map (kbd "C-x r <backspace>") #'rlist-list-registers)
    (define-key map (kbd "C-x r <delete>") #'rlist-list-registers)))

;;; Auto revert mode
(prot-emacs-builtin-package 'autorevert
  (setq auto-revert-verbose t)
  (add-hook 'after-init-hook #'global-auto-revert-mode))

;;; Preserve contents of system clipboard
(setq save-interprogram-paste-before-kill t)

;;; Newline characters for file ending
(setq mode-require-final-newline 'visit-save)

;;; Zap characters
(let ((map global-map))
  (define-key map (kbd "M-z") #'zap-up-to-char)
  (define-key map (kbd "M-Z") #'zap-to-char)) ; M-S-z

;;; Package lists
(prot-emacs-builtin-package 'package
  ;; All variables are for Emacs 28+
  (setq package-name-column-width 40)
  (setq package-version-column-width 14)
  (setq package-status-column-width 12)
  (setq package-archive-column-width 8)
  (add-hook 'package-menu-mode-hook #'hl-line-mode))

;;; Plain text (text-mode with prot-text.el)
(prot-emacs-builtin-package 'text-mode)

(prot-emacs-builtin-package 'prot-text
  (add-to-list 'auto-mode-alist '("\\(README\\|CHANGELOG\\|COPYING\\|LICENSE\\)\\'" . text-mode))
  (define-key text-mode-map (kbd "<M-return>") #'prot-text-insert-heading)
  (define-key org-mode-map (kbd "<M-return>") #'org-meta-return) ; don't override M-RET here
  (define-key org-mode-map (kbd "M-;") nil))

;;; Markdown (markdown-mode)
(prot-emacs-elpa-package 'markdown-mode
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (setq markdown-fontify-code-blocks-natively t))
;; Allows for fenced block focus with C-c ' (same as Org blocks).
(prot-emacs-elpa-package 'edit-indirect)

;;; YAML (yaml-mode)
(prot-emacs-elpa-package 'yaml-mode
  (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode)))

;;; CSS (css-mode)
(prot-emacs-builtin-package 'css-mode
  (add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))
  (add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
  (setq css-fontify-colors nil))

;;; Shell scripts (sh-mode)
(prot-emacs-builtin-package 'sh-script
  (add-to-list 'auto-mode-alist '("PKGBUILD" . sh-mode)))

;;; Paragraphs and fill-mode (prot-fill.el)
(prot-emacs-builtin-package 'prot-fill
  (setq prot-fill-default-column 72)
  (setq prot-fill-prog-mode-column 72)  ; Set this to another value if you want
  ;; Those variables come from various sources, though they feel part of the
  ;; same conceptual framework.
  (setq sentence-end-double-space t)
  (setq sentence-end-without-period nil)
  (setq colon-double-space nil)
  (setq use-hard-newlines nil)
  (setq adaptive-fill-mode t)
  (prot-fill-fill-mode 1)
  (add-hook 'after-init-hook #'column-number-mode))

;;; Comments (newcomment.el and prot-comment.el)
(prot-emacs-builtin-package 'newcomment
  (setq comment-empty-lines t)
  (setq comment-fill-column nil)
  (setq comment-multi-line t)
  (setq comment-style 'multi-line)
  (let ((map global-map))
    (define-key map (kbd "C-:") #'comment-kill)         ; C-S-;
    (define-key map (kbd "M-;") #'comment-indent)))

(prot-emacs-builtin-package 'prot-comment
  (setq prot-comment-comment-keywords
        '("TODO" "NOTE" "XXX" "REVIEW" "FIXME"))
  (setq prot-comment-timestamp-format-concise "%F")
  (setq prot-comment-timestamp-format-verbose "%F %T %z")
  (let ((map global-map))
    (define-key map (kbd "C-;") #'prot-comment-comment-dwim)
    (define-key map (kbd "C-x C-;") #'prot-comment-timestamp-keyword)))

;;; Configure 'electric' behaviour
(prot-emacs-builtin-package 'electric
  (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
  (setq electric-pair-preserve-balance t)
  (setq electric-pair-pairs
        '((8216 . 8217)
          (8220 . 8221)
          (171 . 187)))
  (setq electric-pair-skip-self 'electric-pair-default-skip-self)
  (setq electric-pair-skip-whitespace nil)
  (setq electric-pair-skip-whitespace-chars '(9 10 32))
  (setq electric-quote-context-sensitive t)
  (setq electric-quote-paragraph t)
  (setq electric-quote-string nil)
  (setq electric-quote-replace-double t)
  (electric-pair-mode -1)
  (electric-quote-mode -1)
  ;; I don't like auto indents in Org and related.  They are okay for
  ;; programming.
  (electric-indent-mode -1)
  (add-hook 'prog-mode-hook #'electric-indent-local-mode))

;;; Parentheses (show-paren-mode)
(prot-emacs-builtin-package 'paren
  (setq show-paren-style 'parenthesis)
  (setq show-paren-when-point-in-periphery nil)
  (setq show-paren-when-point-inside-paren nil)
  (setq show-paren-context-when-offscreen 'child-frame) ; Emacs 29
  (add-hook 'after-init-hook #'show-paren-mode))

;;; Tabs, indentation, and the TAB key
(setq-default tab-always-indent 'complete)
(setq-default tab-first-completion 'word-or-paren-or-punct) ; Emacs 27
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;;; Flyspell and prot-spell.el (spell check)
(prot-emacs-builtin-package 'flyspell
  (setq flyspell-issue-message-flag nil)
  (setq flyspell-issue-welcome-flag nil)
  (setq ispell-program-name "aspell")
  (setq ispell-dictionary "en_GB")
  (define-key flyspell-mode-map (kbd "C-;") nil))

(prot-emacs-builtin-package 'prot-spell
  (setq prot-spell-dictionaries
        '(("EN English" . "en")
          ("EL Ελληνικά" . "el")
          ("FR Français" . "fr")
          ("ES Espanõl" . "es")))
  (let ((map global-map))
    (define-key map (kbd "M-$") #'prot-spell-spell-dwim)
    (define-key map (kbd "C-M-$") #'prot-spell-change-dictionary)))

;;; Flymake
(prot-emacs-builtin-package 'flymake
  (setq flymake-fringe-indicator-position 'left-fringe)
  (setq flymake-suppress-zero-counters t)
  (setq flymake-start-on-flymake-mode t)
  (setq flymake-no-changes-timeout nil)
  (setq flymake-start-on-save-buffer t)
  (setq flymake-proc-compilation-prevents-syntax-check t)
  (setq flymake-wrap-around nil)
  (setq flymake-mode-line-format
        '("" flymake-mode-line-exception flymake-mode-line-counters))
  (setq flymake-mode-line-counter-format
        '(" " flymake-mode-line-error-counter
          flymake-mode-line-warning-counter
          flymake-mode-line-note-counter ""))

  (let ((map flymake-mode-map))
    (define-key map (kbd "C-c ! s") #'flymake-start)
    (define-key map (kbd "C-c ! d") #'flymake-show-buffer-diagnostics) ; Emacs28
    (define-key map (kbd "C-c ! n") #'flymake-goto-next-error)
    (define-key map (kbd "C-c ! p") #'flymake-goto-prev-error)))

(prot-emacs-elpa-package 'flymake-diagnostic-at-point
  (setq flymake-diagnostic-at-point-display-diagnostic-function
        'flymake-diagnostic-at-point-display-minibuffer))

;;; Flymake + Shellcheck
(prot-emacs-elpa-package 'flymake-shellcheck
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load))

;;; Flymake + Proselint
(prot-emacs-elpa-package 'flymake-proselint
  (add-hook 'text-mode-hook #'flymake-proselint-setup))

;;; Elisp packaging requirements
(prot-emacs-elpa-package 'package-lint-flymake
  (add-hook 'flymake-diagnostic-functions #'package-lint-flymake))

;;; Eldoc (elisp live documentation feedback)
(prot-emacs-builtin-package 'eldoc
  (global-eldoc-mode 1))

;;; Tools for manual pages (manpages)
(prot-emacs-builtin-package 'man
  (let ((map Man-mode-map))
    (define-key map (kbd "i") #'Man-goto-section)
    (define-key map (kbd "g") #'Man-update-manpage)))

;;; Emacs server and desktop
(prot-emacs-builtin-package 'server
  (add-hook 'after-init-hook #'server-start))

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

;;; Backups
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backup/"))))
(setq backup-by-copying t)
(setq version-control t)
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)
(setq create-lockfiles nil)
