;;; General minibuffer settings
(use-package minibuffer
  :ensure nil
  :config
;;;; Completion styles
  (setq completion-styles '(basic substring initials flex orderless)) ; also see `completion-category-overrides'
  (setq completion-pcm-leading-wildcard t) ; Emacs 31: make `partial-completion' behave like `substring'

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
          (bookmark (styles . (basic substring)))
          (library (styles . (basic substring)))
          (embark-keybinding (styles . (basic substring)))
          (imenu (styles . (basic substring orderless)))
          (consult-location (styles . (basic substring orderless)))
          (kill-ring (styles . (emacs22 orderless)))
          (eglot (styles . (emacs22 substring orderless))))))

;;; Orderless completion style (and prot-orderless.el)
(use-package orderless
  :ensure t
  :demand t
  :after minibuffer
  :config
  ;; Remember to check my `completion-styles' and the
  ;; `completion-category-overrides'.
  (setq orderless-matching-styles '(orderless-prefixes orderless-regexp))

  ;; SPC should never complete: use it for `orderless' groups.
  ;; The `?' is a regexp construct.
  :bind ( :map minibuffer-local-completion-map
          ("SPC" . nil)
          ("?" . nil)))

(use-package prot-orderless
  :ensure nil
  :config
  (setq orderless-style-dispatchers
        '(prot-orderless-literal
          prot-orderless-file-ext
          prot-orderless-beg-or-end)))

(setq completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq-default case-fold-search t)   ; For general regexp
(setq read-file-name-completion-ignore-case t)

(use-package mb-depth
  :ensure nil
  :hook (after-init . minibuffer-depth-indicate-mode)
  :config
  (setq read-minibuffer-restore-windows nil) ; Emacs 28
  (setq enable-recursive-minibuffers t))

(use-package minibuf-eldef
  :ensure nil
  :hook (after-init . minibuffer-electric-default-mode)
  :config
  (setq minibuffer-default-prompt-format " [%s]")) ; Emacs 29

(use-package rfn-eshadow
  :ensure nil
  :hook (minibuffer-setup . cursor-intangible-mode)
  :config
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

  ;; MCT has a variant of this built-in.
  (unless (eq prot-emacs-completion-ui 'mct)
    ;; Add prompt indicator to `completing-read-multiple'.  We display
    ;; [`completing-read-multiple': <separator>], e.g.,
    ;; [`completing-read-multiple': ,] if the separator is a comma.  This
    ;; is adapted from the README of the `vertico' package by Daniel
    ;; Mendler.  I made some small tweaks to propertize the segments of
    ;; the prompt.
    (defun crm-indicator (args)
      (cons (format "[`completing-read-multiple': %s]  %s"
                    (propertize
                     (replace-regexp-in-string
                      "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                      crm-separator)
                     'face 'error)
                    (car args))
            (cdr args)))

    (advice-add #'completing-read-multiple :filter-args #'crm-indicator))

  (file-name-shadow-mode 1))

(use-package minibuffer
  :ensure nil
  :demand t
  :config
  (setq completions-format 'one-column)
  (setq completion-show-help nil)
  (setq completion-auto-help 'always)
  (setq completion-auto-select nil)
  (setq completions-detailed t)
  (setq completion-show-inline-help nil)
  (setq completions-max-height 6)
  (setq completions-header-format (propertize "%s candidates:\n" 'face 'bold-italic))
  (setq completions-highlight-face 'completions-highlight)
  (setq minibuffer-completion-auto-choose t)
  (setq minibuffer-visible-completions t) ; Emacs 30
  (setq completions-sort 'historical)

  (unless prot-emacs-completion-ui
    (prot-emacs-keybind minibuffer-local-completion-map
      "<up>" #'minibuffer-previous-line-completion
      "<down>" #'minibuffer-next-line-completion)

    (add-hook 'completion-list-mode-hook #'prot-common-truncate-lines-silently)))

;;;; `savehist' (minibuffer and related histories)
(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :config
  (setq savehist-file (locate-user-emacs-file "savehist"))
  (setq history-length 100)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t)
  (add-to-list 'savehist-additional-variables 'kill-ring))

(use-package dabbrev
  :ensure nil
  :commands (dabbrev-expand dabbrev-completion)
  :config
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
  (setq dabbrev-ignored-buffer-modes
        '(archive-mode image-mode docview-mode pdf-view-mode)))

;;;; `abbrev' (Abbreviations, else Abbrevs)
(use-package abbrev
  :ensure nil
  ;; message-mode derives from text-mode, so we don't need a separate
  ;; hook for it.
  :hook ((text-mode prog-mode git-commit-mode) . abbrev-mode)
  :config
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
      ";dev" "{{{development-version}}}")
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
  (when-let ((scratch (get-buffer "*scratch*")))
    (with-current-buffer scratch
      (abbrev-mode 1)))

  ;; By default, abbrev asks for confirmation on whether to use
  ;; `abbrev-file-name' to save abbrevations.  I do not need that, nor
  ;; do I want it.
  (remove-hook 'save-some-buffers-functions #'abbrev--possibly-save))

;;; Corfu (in-buffer completion popup)
(use-package corfu
  :ensure t
  :hook (after-init . global-corfu-mode)
  ;; I also have (setq tab-always-indent 'complete) for TAB to complete
  ;; when it does not need to perform an indentation change.
  :bind (:map corfu-map ("<tab>" . corfu-complete))
  :config
  (setq corfu-preview-current nil)
  (setq corfu-min-width 20)

  (setq corfu-popupinfo-delay '(1.25 . 0.5))
  (corfu-popupinfo-mode 1) ; shows documentation after `corfu-popupinfo-delay'

  ;; Sort by input history (no need to modify `corfu-sort-function').
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history)))

;;; Enhanced minibuffer commands (consult.el)
(when prot-emacs-completion-extras
  (use-package consult
    :ensure t
    :hook (completion-list-mode . consult-preview-at-point-mode)
    :bind
    ( :map global-map
      ("M-g M-g" . consult-goto-line)
      ("M-K" . consult-keep-lines) ; M-S-k is similar to M-S-5 (M-%)
      ("M-F" . consult-focus-lines) ; same principle
      ("M-s M-b" . consult-buffer)
      ("M-s M-f" . consult-find)
      ("M-s M-g" . consult-grep)
      ("M-s M-h" . consult-history)
      ("M-s M-i" . consult-imenu)
      ("M-s M-l" . consult-line)
      ("M-s M-m" . consult-mark)
      ("M-s M-y" . consult-yank-pop)
      ("M-s M-s" . consult-outline)
      :map consult-narrow-map
      ("?" . consult-narrow-help))
    :config
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

    (add-to-list 'consult-mode-histories '(vc-git-log-edit-mode . log-edit-comment-ring))

    (require 'consult-imenu) ; the `imenu' extension is in its own file

    (with-eval-after-load 'pulsar
      ;; see my `pulsar' package: <https://protesilaos.com/emacs/pulsar>
      (setq consult-after-jump-hook nil) ; reset it to avoid conflicts with my function
      (dolist (fn '(pulsar-recenter-top pulsar-reveal-entry))
        (add-hook 'consult-after-jump-hook fn)))))

;;; Extended minibuffer actions and more (embark.el and prot-embark.el)
(when prot-emacs-completion-extras
  (use-package embark
    :ensure t
    :defer 1
    :config
    (setq embark-confirm-act-all nil)
    ;; The prot-embark.el has an advice to further simplify the
    ;; minimal indicator.  It shows cycling, which I never want to see
    ;; or do.
    (setq embark-mixed-indicator-both nil)
    (setq embark-mixed-indicator-delay 1.0)
    (setq embark-indicators '(embark-mixed-indicator embark-highlight-indicator))
    (setq embark-verbose-indicator-nested nil) ; I think I don't have them, but I do not want them either
    (setq embark-verbose-indicator-buffer-sections '(bindings))
    (setq embark-verbose-indicator-excluded-actions
          '(embark-cycle embark-act-all embark-collect embark-export embark-insert))

    ;; I never cycle and want to disable the damn thing.  Normally, a
    ;; nil value disables a key binding but here that value is
    ;; interpreted as the binding for `embark-act'.  So I just add
    ;; some obscure key that I do not have.  I absolutely do not want
    ;; to cycle!
    (setq embark-cycle-key "<XF86Travel>")

    ;; I do not want `embark-org' and am not sure what is loading it.
    ;; So I just unsert all the keymaps... This is the nuclear option
    ;; but here we are.
    (with-eval-after-load 'embark-org
      (defvar prot/embark-org-keymaps
        '(embark-org-table-cell-map
          embark-org-table-map
          embark-org-link-copy-map
          embark-org-link-map
          embark-org-src-block-map
          embark-org-item-map
          embark-org-plain-list-map
          embark-org-export-in-place-map)
        "List of Embark keymaps for Org.")

      ;; Reset `prot/embark-org-keymaps'.
      (seq-do
       (lambda (keymap)
         (set keymap (make-sparse-keymap)))
       prot/embark-org-keymaps)))

  ;; I define my own keymaps because I only use a few functions in a
  ;; limited number of contexts.
  (use-package prot-embark
    :ensure nil
    :after embark
    :bind
    ( :map global-map
      ("C-," . prot-embark-act-no-quit)
      ("C-." . prot-embark-act-quit)
      :map embark-collect-mode-map
      ("C-," . prot-embark-act-no-quit)
      ("C-." . prot-embark-act-quit)
      :map minibuffer-local-filename-completion-map
      ("C-," . prot-embark-act-no-quit)
      ("C-." . prot-embark-act-quit))
    :config
    (setq embark-keymap-alist
          '((buffer prot-embark-buffer-map)
            (command prot-embark-command-map)
            (expression prot-embark-expression-map)
            (file prot-embark-file-map)
            (function prot-embark-function-map)
            (identifier prot-embark-identifier-map)
            (package prot-embark-package-map)
            (region prot-embark-region-map)
            (symbol prot-embark-symbol-map)
            (url prot-embark-url-map)
            (variable prot-embark-variable-map)
            (t embark-general-map))))

  ;; Needed for correct exporting while using Embark with Consult
  ;; commands.
  (use-package embark-consult
    :ensure t
    :after (embark consult)))

;;; Detailed completion annotations (marginalia.el)
(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode)
  :config
  (setq marginalia-max-relative-age 0)) ; absolute time

;;;; Custom completion annotations
(use-package prot-marginalia
  :ensure nil
  :after marginalia
  :config
  (setq marginalia-annotator-registry
        '((bookmark prot-marginalia-bookmark)
          (buffer prot-marginalia-buffer)
          (command marginalia-annotate-command)
          (function prot-marginalia-symbol)
          (symbol prot-marginalia-symbol)
          (variable prot-marginalia-symbol)
          (face marginalia-annotate-face)
          (imenu marginalia-annotate-imenu)
          (package prot-marginalia-package)
          (unicode-name marginalia-annotate-char))))

;;; The minibuffer user interface (mct, vertico, or none)
(when prot-emacs-completion-ui
  (require
   (pcase prot-emacs-completion-ui
     ('mct 'prot-emacs-mct)
     ('vertico 'prot-emacs-vertico))))

(provide 'prot-emacs-completion)
