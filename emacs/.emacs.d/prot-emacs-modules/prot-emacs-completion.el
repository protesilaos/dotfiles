;;; General minibuffer settings
(prot-emacs-configure
;;;; Completion styles
  (setq completion-styles '(basic substring initials partial-completion flex)) ; also see `completion-category-overrides'
  (setq completion-pcm-leading-wildcard nil) ; Emacs 31
  (with-eval-after-load 'orderless
    (setq completion-styles (append completion-styles '(orderless)))))

;;;; Completion category overrides
(prot-emacs-configure
  ;; Reset all the per-category defaults so that (i) we use the
  ;; standard `completion-styles' and (ii) can specify our own styles
  ;; in the `completion-category-overrides' without having to
  ;; explicitly override everything.
  (setq completion-category-defaults nil)

  (prot-minibuffer-missing-categories-mode 1)

  ;; NOTE 2025-12-02: The `eager-display' and `eager-update' are part of Emacs 31.
  (let* ((eager-update-properties '((eager-display . nil)
                                    (eager-update . t)))
         (eager-update-properties-no-sort (append eager-update-properties (list (cons 'display-sort-function #'identity)))))
    (setq completion-category-overrides
          `((file . ((styles . (partial-completion))
                     (eager-display . nil)
                     (eager-update . t)
                     (group-function . ,#'prot-minibuffer-file-group)
                     (affixation-function . ,#'prot-minibuffer-file-affixate)
                     (display-sort-function . ,#'prot-minibuffer-file-sort)))
            (bookmark . (,@eager-update-properties
                         (affixation-function . ,#'prot-minibuffer-bookmark-affixate)))
            (project-file . (,@eager-update-properties
                             (group-function . ,#'prot-minibuffer-file-group)
                             (affixation-function . ,#'prot-minibuffer-file-affixate)))
            (prot-minibuffer-library . (,@eager-update-properties
                                        (annotation-function . ,#'prot-minibuffer-library-annotate)
                                        (display-sort-function . ,#'prot-minibuffer-library-sort)))
            (symbol-help . (,@eager-update-properties
                            (display-sort-function . ,#'prot-minibuffer-symbol-sort)))
            (buffer . (,@eager-update-properties
                       (affixation-function . ,#'prot-minibuffer-buffer-affixate)))
            (command . ((affixation-function . nil)
                        (annotation-function . ,#'prot-minibuffer-command-annotate)))
            (denote-file . ,eager-update-properties)
            (prot-minibuffer-emoji . ,eager-update-properties)
            (theme . ,eager-update-properties)
            (unicode-name . ,eager-update-properties)
            (imenu . ,eager-update-properties-no-sort)
            (consult-location . ,eager-update-properties-no-sort)
            (prot-minibuffer-kill-ring . ((eager-display . t)
                                          (eager-update . t)
                                          (display-sort-function . identity)))))))

;;; Orderless completion style (and prot-orderless.el)
(when prot-emacs-completion-extras
  (prot-emacs-configure
    (prot-emacs-install orderless)
    (require 'orderless)
    ;; Remember to check my `completion-styles' and the
    ;; `completion-category-overrides'.
    (setq orderless-matching-styles '(orderless-prefixes orderless-regexp))
    (setq orderless-smart-case nil)

    ;; SPC should never complete: use it for `orderless' groups.
    ;; The `?' is a regexp construct.
    (prot-emacs-keybind minibuffer-local-completion-map
      "SPC" nil
      "?" nil)))

(setq completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq-default case-fold-search t)   ; For general regexp
(setq read-file-name-completion-ignore-case t)
(setq minibuffer-history-case-insensitive-variables t)

(prot-emacs-configure
  (setq read-minibuffer-restore-windows nil)
  (setq enable-recursive-minibuffers t) ; Emacs 28
  (minibuffer-depth-indicate-mode 1))

(prot-emacs-configure
  (setq minibuffer-default-prompt-format " [%s]") ; Emacs 29
  (minibuffer-electric-default-mode 1))

(prot-emacs-configure
  (setq resize-mini-windows t)
  (setq read-answer-short t) ; also check `use-short-answers' for Emacs28
  (setq echo-keystrokes 0.25)
  (setq kill-ring-max 60) ; Keep it small

  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  ;; Do not allow the cursor to move inside the minibuffer prompt.  I
  ;; got this from the documentation of Daniel Mendler's Vertico
  ;; package: <https://github.com/minad/vertico>.
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))

  (setq crm-prompt (format "%s %%p" (propertize "[%d]" 'face 'shadow))) ; Emacs 31

  (file-name-shadow-mode 1))

(prot-emacs-configure
  (prot-emacs-hook
    (completion-list-mode-hook minibuffer-setup-hook)
    prot-common-truncate-lines-silently)

  (unless prot-emacs-completion-ui
    (prot-minibuffer-completions-mode 1)

    (prot-emacs-keybind completion-list-mode-map
      "h" #'prot-minibuffer-completions-describe-at-point ; "Help" mnemonic
      "c" #'prot-minibuffer-choose-completion-no-exit ; "Choose" mnemonic
      "TAB" #'prot-minibuffer-choose-completion-dwim
      "RET" #'prot-minibuffer-choose-completion-exit)))

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
(when (and (eq prot-emacs-completion-ui 'vertico)
           prot-emacs-completion-extras
           prot-display-graphic-p)
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

(prot-emacs-comment
  (prot-emacs-configure
    (setq completion-preview-exact-match-only nil)
    (setq completion-preview-commands '(self-insert-command
                                        insert-char
                                        analyze-text-conversion
                                        completion-preview-insert-word))
    (setq completion-preview-minimum-symbol-length 4)
    (setq completion-preview-idle-delay 0.3)
    (setq completion-preview-ignore-case t)
    (setq completion-preview-sort-function #'identity)

    (add-hook 'prog-mode-hook #'completion-preview-mode)

    (with-eval-after-load 'completion-preview
      (prot-emacs-keybind completion-preview-active-mode-map
        "M-n" #'completion-preview-next-candidate
        "M-p" #'completion-preview-prev-candidate
        "<tab>" #'completion-preview-complete))))

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
(when prot-emacs-completion-extras
  (prot-emacs-configure
    (prot-emacs-install marginalia)
    (setq marginalia-max-relative-age 0) ; absolute time
    (marginalia-mode 1)))

;;; The minibuffer user interface (mct, vertico, or none)
(when prot-emacs-completion-ui
  (require
   (pcase prot-emacs-completion-ui
     ('mct 'prot-emacs-mct)
     ('vertico 'prot-emacs-vertico))))

(provide 'prot-emacs-completion)
