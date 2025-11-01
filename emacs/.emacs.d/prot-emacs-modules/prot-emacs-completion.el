;;; General minibuffer settings
(prot-emacs-configure
;;;; Completion styles
  (setq completion-styles '(basic substring initials flex orderless)) ; also see `completion-category-overrides'
  (setq completion-pcm-leading-wildcard t)) ; Emacs 31: make `partial-completion' behave like `substring'

;;;; Completion category overrides
(prot-emacs-configure
  ;; Reset all the per-category defaults so that (i) we use the
  ;; standard `completion-styles' and (ii) can specify our own styles
  ;; in the `completion-category-overrides' without having to
  ;; explicitly override everything.
  (setq completion-category-defaults nil)

  ;; A non-exhaustve list of known completion categories:
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
  ;; - `project-buffer'
  ;; - `project-file'
  ;; - `symbol' (the `describe-symbol' command bound to `C-h o')
  ;; - `theme'
  ;; - `unicode-name' (the `insert-char' command bound to `C-x 8 RET')
  ;; - `variable' (the `describe-variable' command bound to `C-h v')
  ;; - `consult-grep'
  ;; - `consult-isearch'
  ;; - `consult-kmacro'
  ;; - `consult-location'
  ;; - `embark-keybinding'
  (setq completion-category-overrides
        (if prot-emacs-completion-ui
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
              (bookmark (styles . (basic substring)))
              (library (styles . (basic substring)))
              (embark-keybinding (styles . (basic substring)))
              (imenu (styles . (basic substring orderless)))
              (consult-location (styles . (basic substring orderless)))
              (kill-ring (styles . (emacs22 orderless)))
              (eglot (styles . (emacs22 substring orderless))))
          '((file (styles . (basic partial-completion orderless)) (eager-display . t))
            (bookmark (styles . (basic substring)))
            (library (styles . (basic substring)))
            (embark-keybinding (styles . (basic substring)) (eager-display . t))
            (imenu (styles . (basic substring orderless)) (eager-display . t))
            (consult-location (styles . (basic substring orderless)) (eager-display . t))
            (kill-ring (styles . (emacs22 orderless)) (eager-display . t))
            (eglot (styles . (emacs22 substring orderless)))))))

;;; Orderless completion style (and prot-orderless.el)
(prot-emacs-configure
  (prot-emacs-install orderless)
  ;; Remember to check my `completion-styles' and the
  ;; `completion-category-overrides'.
  (setq orderless-matching-styles '(orderless-prefixes orderless-regexp))
  (setq orderless-smart-case nil)

  ;; SPC should never complete: use it for `orderless' groups.
  ;; The `?' is a regexp construct.
  (prot-emacs-keybind minibuffer-local-completion-map
    "SPC" nil
    "?" nil))

(setq completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq-default case-fold-search t)   ; For general regexp
(setq read-file-name-completion-ignore-case t)

(prot-emacs-configure
  (setq read-minibuffer-restore-windows nil)
  (setq enable-recursive-minibuffers t) ; Emacs 28
  (minibuffer-depth-indicate-mode 1))

(prot-emacs-configure
  (setq minibuffer-default-prompt-format " [%s]") ; Emacs 29
  (minibuffer-electric-default-mode 1))

(prot-emacs-configure
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  ;; Not everything here comes from rfn-eshadow.el, but this is fine.
  (setq resize-mini-windows t)
  (setq read-answer-short t) ; also check `use-short-answers' for Emacs28
  (setq echo-keystrokes 0.25)
  (setq kill-ring-max 60) ; Keep it small

  ;; Do not allow the cursor to move inside the minibuffer prompt.  I
  ;; got this from the documentation of Daniel Mendler's Vertico
  ;; package: <https://github.com/minad/vertico>.
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))

  (setq crm-prompt (format "%s %%p" (propertize "[%d]" 'face 'shadow))) ; Emacs 31

  (file-name-shadow-mode 1))

(prot-emacs-configure
  (add-hook 'minibuffer-setup #'prot-common-truncate-lines-silently)

  ;; (setq completions-header-format (propertize "%s candidates:\n" 'face 'bold-italic))
  (setq completions-header-format "")
  (setq completions-highlight-face 'completions-highlight)
  (setq completions-max-height 10)
  (setq completions-sort 'historical)

  ;; These settings make the generic minibuffer completion interface
  ;; work more like my `mct' package.  Though it still has some issues
  ;; with how candidates are selected.  Those might be fixed
  ;; eventually.  The concrete problem I see with these settings is
  ;; how the Completions can behave in very different ways, so we need
  ;; to figure out which combination of these options works correctly.
  (unless prot-emacs-completion-ui
    (setq completion-auto-help 'always)
    (setq completion-show-help nil)
    (setq completion-show-inline-help nil)
    (setq completion-auto-select nil)
    (setq completion-auto-deselect nil)
    (setq completions-detailed t)
    (setq completions-format 'one-column)
    (setq minibuffer-completion-auto-choose nil)
    (setq minibuffer-visible-completions nil) ; Emacs 30
    ;; This one is for Emacs 31.  It relies on what I am doing with the `completion-category-overrides'.
    (setq completion-eager-display 'auto)
    ;; This is also for Emacs 31 and it too leverages the `completion-category-overrides' if set to `auto'.
    (setq completion-eager-update t)

    (prot-emacs-keybind minibuffer-local-completion-map
      "<up>" #'minibuffer-previous-line-completion
      "<down>" #'minibuffer-next-line-completion
      "C-l" #'minibuffer-completion-help) ; "list completions" mnemonic

    (prot-emacs-keybind completion-in-region-mode-map
      "<up>" #'minibuffer-previous-completion
      "<down>" #'minibuffer-next-completion
      "RET" #'minibuffer-choose-completion)

    (defun prot/completions-tweak-style ()
      "Tweak the style of the Completions buffer."
      (setq-local mode-line-format nil)
      (setq-local cursor-in-non-selected-windows nil)
      (when (and completions-header-format
                 (not (string-blank-p completions-header-format)))
        (setq-local display-line-numbers-offset -1))
      (display-line-numbers-mode 1)
      (unless (eq (prot-common-completion-category) 'file)
        (setq-local minibuffer-visible-completions t)))

    (prot-emacs-hook
      completion-list-mode-hook
      (prot/completions-tweak-style prot-common-truncate-lines-silently))))

;;;; `savehist' (minibuffer and related histories)
(prot-emacs-configure
  (setq savehist-file (locate-user-emacs-file "savehist"))
  (setq history-length 100)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t)
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'kill-ring))
  (savehist-mode 1))

(prot-emacs-configure
;;;; `dabbrev' (dynamic word completion (dynamic abbreviations))
  (setq dabbrev-abbrev-char-regexp "\\sw\\|\\s_")
  (setq dabbrev-abbrev-skip-leading-regexp "[$*/=~']")
  (setq dabbrev-backward-only nil)
  (setq dabbrev-case-distinction 'case-replace)
  (setq dabbrev-case-fold-search nil)
  (setq dabbrev-case-replace 'case-replace)
  (setq dabbrev-check-other-buffers t)
  (setq dabbrev-eliminate-newlines t)
  (setq dabbrev-upcase-means-case-search t)
  (setq dabbrev-ignored-buffer-modes '(archive-mode image-mode docview-mode pdf-view-mode)))

;;;; `abbrev' (Abbreviations, else Abbrevs)
(prot-emacs-configure
  ;; message-mode derives from text-mode, so we don't need a separate
  ;; hook for it.
  (prot-emacs-hook
    (text-mode-hook prog-mode-hook git-commit-mode-hook)
    abbrev-mode)

  (setq only-global-abbrevs nil)

  (prot-emacs-abbrev global-abbrev-table
    "meweb"   "https://protesilaos.com"
    "megit"   "https://github.com/protesilaos"
    "mehub"   "https://github.com/protesilaos"
    "meclone" "git@github.com/protesilaos/"
    "melab"   "https://gitlab.com/protesilaos"
    "medrive" "hyper://5cr7mxac8o8aymun698736tayrh1h4kbqf359cfk57swjke716gy/"
    ";web"   "https://protesilaos.com"
    ";git"   "https://github.com/protesilaos"
    ";hub"   "https://github.com/protesilaos"
    ";clone" "git@github.com/protesilaos/"
    ";lab"   "https://gitlab.com/protesilaos"
    ";drive" "hyper://5cr7mxac8o8aymun698736tayrh1h4kbqf359cfk57swjke716gy/")

  (prot-emacs-abbrev text-mode-abbrev-table
    "asciidoc"       "AsciiDoc"
    "auctex"         "AUCTeX"
    "cafe"           "café"
    "cliche"         "cliché"
    "clojurescript"  "ClojureScript"
    "emacsconf"      "EmacsConf"
    "github"         "GitHub"
    "gitlab"         "GitLab"
    "javascript"     "JavaScript"
    "latex"          "LaTeX"
    "libreplanet"    "LibrePlanet"
    "linkedin"       "LinkedIn"
    "paypal"         "PayPal"
    "sourcehut"      "SourceHut"
    "texmacs"        "TeXmacs"
    "typescript"     "TypeScript"
    "visavis"        "vis-à-vis"
    "deja"           "déjà"
    "youtube"        "YouTube"
    ";up"            "🙃"
    ";uni"           "🦄"
    ";laugh"         "🤣"
    ";smile"         "😀"
    ";sun"           "☀️")

  ;; Allow abbrevs with a prefix colon, semicolon, or underscore.  I demonstrated
  ;; this here: <https://protesilaos.com/codelog/2024-02-03-emacs-abbrev-mode/>.
  (abbrev-table-put global-abbrev-table :regexp "\\(?:^\\|[\t\s]+\\)\\(?1:[:;_].*\\|.*\\)")

  (with-eval-after-load 'text-mode
    (abbrev-table-put text-mode-abbrev-table :regexp "\\(?:^\\|[\t\s]+\\)\\(?1:[:;_].*\\|.*\\)"))

  (with-eval-after-load 'org
    (prot-emacs-abbrev org-mode-abbrev-table
      ";dev" "{{{development-version}}}"
      ";key" #'prot-abbrev-org-macro-key
      ";cmd" #'prot-abbrev-org-macro-key-command)
    (abbrev-table-put org-mode-abbrev-table :regexp "\\(?:^\\|[\t\s]+\\)\\(?1:[:;_].*\\|.*\\)"))

  (with-eval-after-load 'message
    (prot-emacs-abbrev message-mode-abbrev-table
      "bestregards"  "Best regards,\nProtesilaos (or simply \"Prot\")"
      "allthebest"   "All the best,\nProtesilaos (or simply \"Prot\")"
      "niceday"      "Have a nice day,\nProtesilaos (or simply \"Prot\")"
      "abest"        "All the best,\nProt"
      "bregards"     "Best regards,\nProt"
      "nday"         "Have a nice day,\nProt"
      "nosrht"       "P.S. I am phasing out SourceHut: <https://protesilaos.com/codelog/2024-01-27-sourcehut-no-more/>.
Development continues on GitHub with GitLab as a mirror."))

  ;; The `prot-emacs-abbrev' macro, which simplifies how we use
  ;; `define-abbrev', does not only expand a static text.  It can take
  ;; a pair of string and function to trigger the latter when the
  ;; former is inserted.  Think of it like the basis of a simplistic
  ;; templating system.
  (require 'prot-abbrev)
  (prot-emacs-abbrev global-abbrev-table
    "metime" #'prot-abbrev-current-time
    "medate" #'prot-abbrev-current-date
    "mejitsi" #'prot-abbrev-jitsi-link
    ";time" #'prot-abbrev-current-time
    ";date" #'prot-abbrev-current-date
    ";jitsi" #'prot-abbrev-jitsi-link)

  (prot-emacs-abbrev text-mode-abbrev-table
    ";update" #'prot-abbrev-update-html)

  ;; Because the *scratch* buffer is produced before we load this, we
  ;; have to explicitly activate the mode there.
  (when-let* ((scratch (get-buffer "*scratch*")))
    (with-current-buffer scratch
      (abbrev-mode 1)))

  ;; By default, abbrev asks for confirmation on whether to use
  ;; `abbrev-file-name' to save abbrevations.  I do not need that, nor
  ;; do I want it.
  (remove-hook 'save-some-buffers-functions #'abbrev--possibly-save))

;;; Corfu (in-buffer completion popup)
(when (and prot-emacs-completion-ui prot-display-graphic-p)
  (prot-emacs-configure
    (prot-emacs-install corfu)

    (setq corfu-preview-current nil)
    (setq corfu-min-width 20)

    (setq corfu-popupinfo-delay '(1.25 . 0.5))
    (corfu-popupinfo-mode 1) ; shows documentation after `corfu-popupinfo-delay'

    (global-corfu-mode 1)

    ;; I also have (setq tab-always-indent 'complete) for TAB to complete
    ;; when it does not need to perform an indentation change.
    (define-key corfu-map (kbd "<tab>") #'corfu-complete)

    ;; Sort by input history (no need to modify `corfu-sort-function').
    (with-eval-after-load 'savehist
      (corfu-history-mode 1)
      (add-to-list 'savehist-additional-variables 'corfu-history))))

;;; Enhanced minibuffer commands (consult.el)
(when prot-emacs-completion-extras
  (prot-emacs-configure
    (prot-emacs-install consult)

    (prot-emacs-keybind global-map
      "M-g M-g" #'consult-goto-line
      "M-s M-b" #'consult-buffer
      "M-s M-f" #'consult-find
      "M-s M-g" #'consult-grep
      "M-s M-h" #'consult-history
      "M-s M-i" #'consult-imenu
      "M-s M-l" #'consult-line
      "M-s M-m" #'consult-mark
      "M-s M-y" #'consult-yank-pop
      "M-s M-s" #'consult-outline)

    (setq consult-line-numbers-widen t)
    ;; (setq completion-in-region-function #'consult-completion-in-region)
    (setq consult-async-min-input 3)
    (setq consult-async-input-debounce 0.5)
    (setq consult-async-input-throttle 0.8)
    (setq consult-narrow-key nil)
    (setq consult-find-args
          (concat "find . -not ( "
                  "-path */.git* -prune "
                  "-or -path */.cache* -prune )"))
    (setq consult-preview-key 'any)
    (setq consult-project-function nil) ; always work from the current directory (use `cd' to switch directory)

    ;; see my `pulsar' package: <https://protesilaos.com/emacs/pulsar>
    (setq consult-after-jump-hook nil) ; reset it to avoid conflicts with my function
    (prot-emacs-hook
      consult-after-jump-hook
      (pulsar-recenter-top pulsar-reveal-entry)
      nil
      pulsar)))

;;; Extended minibuffer actions and more (embark.el)
(when prot-emacs-completion-extras
  (prot-emacs-configure
    (prot-emacs-install embark)

    (add-hook 'embark-collect-mode-hook #'prot-common-truncate-lines-silently)

    (prot-emacs-keybind minibuffer-local-map
      "C-c C-c" #'embark-collect
      "C-c C-e" #'embark-export)

    ;; Needed for correct exporting while using Embark with Consult commands.
    (prot-emacs-install embark-consult)

    (with-eval-after-load 'consult
      (require 'embark-consult))))

;;; Detailed completion annotations (marginalia.el)
(prot-emacs-configure
  (prot-emacs-install marginalia)
  (setq marginalia-max-relative-age 0) ; absolute time
  (marginalia-mode 1))

;;; The minibuffer user interface (mct, vertico, or none)
(when prot-emacs-completion-ui
  (require
   (pcase prot-emacs-completion-ui
     ('mct 'prot-emacs-mct)
     ('vertico 'prot-emacs-vertico))))

(provide 'prot-emacs-completion)
