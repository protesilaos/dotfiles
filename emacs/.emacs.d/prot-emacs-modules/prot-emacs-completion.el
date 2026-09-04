;;; General minibuffer settings
(prot-emacs-configure
;;;; Completion styles
  (setq completion-styles '(basic substring initials partial-completion flex)) ; also see `completion-category-overrides'
  (setq completion-flex-nospace t)
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
  (let* ((eager-both '((eager-display . t)
                       (eager-update . t)))
         (eager-both-no-sort (append eager-both (list (cons 'display-sort-function #'identity))))
         (eager-update-only '((eager-display . nil)
                              (eager-update . t)))
         (eager-update-only-no-sort (append eager-update-only (list (cons 'display-sort-function #'identity)))))
    (setq completion-category-overrides
          `((file . (,@eager-update-only
                     (styles . (partial-completion))
                     (group-function . ,#'prot-minibuffer-file-group)
                     (affixation-function . ,#'prot-minibuffer-file-affixate)
                     (display-sort-function . ,#'prot-minibuffer-file-sort)))
            (buffer . (,@eager-update-only
                       (affixation-function . ,#'prot-minibuffer-buffer-affixate)))
            (project-file . (,@eager-update-only
                             (group-function . ,#'prot-minibuffer-file-group)
                             (affixation-function . ,#'prot-minibuffer-file-affixate)))
            (prot-minibuffer-library . (,@eager-update-only
                                        (annotation-function . ,#'prot-minibuffer-library-annotate)
                                        (display-sort-function . ,#'prot-minibuffer-library-sort)))
            (bookmark . (,@eager-update-only
                         (affixation-function . ,#'prot-minibuffer-bookmark-affixate)))
            (command . (,@eager-update-only
                        (affixation-function . nil) ; so that the `annotation-function' can take effect
                        (annotation-function . ,#'prot-minibuffer-command-annotate)))
            (prot-minibuffer-input-method . (,@eager-update-only
                                             (group-function . ,#'prot-minibuffer-input-method-group)
                                             (display-sort-function . ,#'prot-minibuffer-input-method-sort)))
            (symbol-help . ,eager-update-only)
            (denote-file . ,eager-update-only)
            (cape-abbrev . ,eager-update-only)
            (cape-dabbrev . ,eager-update-only)
            (cape-dict . ,eager-update-only)
            (cape-emoji . ,eager-update-only)
            (cape-history . ,eager-update-only)
            (cape-keyword . ,eager-update-only)
            (cape-super . ,eager-update-only)
            (tmr-timer . ,eager-both)
            (prot-minibuffer-emoji . ,eager-update-only)
            (theme . ,eager-update-only)
            (unicode-name . ,eager-update-only)
            (prot-minibuffer-pass . ,eager-update-only)
            (imenu . ,eager-update-only-no-sort)
            (consult-location . ,eager-update-only-no-sort)
            (prot-minibuffer-kill-ring . ,eager-update-only-no-sort)))))

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

  (setq completions-group-format
        (concat
         (propertize (make-string 20 ? ) 'face 'completions-group-separator)
         (propertize " %s " 'face 'completions-group-title)
         (propertize " " 'face 'completions-group-separator 'display '(space :align-to right))))

  (unless prot-emacs-completion-ui
    (prot-minibuffer-completions-mode 1)

    (prot-emacs-keybind completion-in-region-mode-map
      "M-n" #'minibuffer-next-completion
      "M-p" #'minibuffer-previous-completion)

    (prot-emacs-keybind minibuffer-local-completion-map
      "<down>" #'minibuffer-next-line-completion
      "<up>" #'minibuffer-previous-line-completion
      "C-h C-h" #'prot-minibuffer-completions-describe-at-point ; overrides `help-for-help'
      "C-<tab>" #'prot-minibuffer-choose-completion-no-exit
      "RET" #'prot-minibuffer-choose-completion-exit)

    (prot-emacs-keybind completion-list-mode-map
      "C-h C-h" #'prot-minibuffer-completions-describe-at-point ; overrides `help-for-help'
      "C-<tab>" #'prot-minibuffer-choose-completion-no-exit
      "RET" #'prot-minibuffer-choose-completion-dwim)))

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
  (require 'prot-abbrev)

  (setq only-global-abbrevs nil)

  (define-key global-map (kbd "C-x a u") #'unexpand-abbrev)

  ;; Because the *scratch* buffer is produced before we load this, we
  ;; have to explicitly activate the mode there.
  (when-let* ((scratch (get-buffer "*scratch*")))
    (with-current-buffer scratch
      (abbrev-mode 1)))

  ;; By default, abbrev asks for confirmation on whether to use
  ;; `abbrev-file-name' to save abbrevations.  I do not need that, nor
  ;; do I want it.
  (remove-hook 'save-some-buffers-functions #'abbrev--possibly-save)

  (prot-emacs-hook
    ( text-mode-hook prog-mode-hook git-commit-mode-hook
      vc-git-log-edit-mode-hook message-mode-hook)
    abbrev-mode)

  (prot-emacs-abbrev global-abbrev-table
    ";web"   "https://protesilaos.com"
    ";git"   "https://github.com/protesilaos"
    ";hub"   "https://github.com/protesilaos"
    ";clone" "git@github.com/protesilaos/"
    ";lab"   "https://gitlab.com/protesilaos"
    ";time"  #'prot-abbrev-current-time
    ";date"  #'prot-abbrev-current-date
    ";jitsi" #'prot-abbrev-jitsi-link)

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
    "voila"          "voilà"
    "youtube"        "YouTube"
    ";up"            "🙃"
    ";uni"           "🦄"
    ";laugh"         "🤣"
    ";smile"         "😀"
    ";update"        #'prot-abbrev-update-html)

  (with-eval-after-load 'org
    (prot-emacs-abbrev org-mode-abbrev-table
      ";dev" "{{{development-version}}}"
      ";key" #'prot-abbrev-org-macro-key
      ";cmd" #'prot-abbrev-org-macro-key-command))

  (with-eval-after-load 'message
    (prot-emacs-abbrev message-mode-abbrev-table
      "bestregards"  "Best regards,\nProtesilaos (or simply \"Prot\")"
      "allthebest"   "All the best,\nProtesilaos (or simply \"Prot\")"
      "niceday"      "Have a nice day,\nProtesilaos (or simply \"Prot\")"
      "abest"        "All the best,\nProt"
      "bregards"     "Best regards,\nProt"
      "nday"         "Have a nice day,\nProt"))

  (with-eval-after-load 'markdown-mode
    (prot-emacs-abbrev markdown-mode-abbrev-table
      ";vlog" "---
title: \"#\"
excerpt: \"#\"
layout: vlog
mediaid: \"#\"
---
"
      ";post" "---
title: \"#\"
excerpt: \"#\"
---
"
      ";poem" "---
title: \"#\"
excerpt: \"Just read the poem. No further comment.\"
---
"))

  (with-eval-after-load 'vc-git
    (prot-emacs-abbrev vc-git-log-edit-mode-abbrev-table
    ";update" #'prot-abbrev-update-html)))

;;; Corfu (in-buffer completion popup)
(when (or (eq prot-emacs-completion-in-buffer 'corfu)
          (eq prot-emacs-completion-in-buffer 'corfu-completion-preview))
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

(unless (eq prot-emacs-completion-in-buffer 'company)
  (prot-emacs-configure
    (prot-emacs-install cape)

    (defun prot/cape-super-set-local (capfs &optional individual-capfs)
      "Set `completion-at-point-functions' to current value plus CAPFS.
Treat CAPFS and the default value as a super CAPF.  Then append the
INDIVIDUAL-CAPFS to the list."
      (let* ((all-for-super (append completion-at-point-functions capfs))
             (all-minus-global (delq t all-for-super))
             (cape-super (apply #'cape-capf-super all-minus-global)))
        (setq-local completion-at-point-functions (append (list cape-super) individual-capfs (list t)))))

    (defun prot/cape-prog-setup ()
      "Set up Cape for programming."
      (prot/cape-super-set-local '(cape-dabbrev cape-abbrev) '(cape-file)))

    (add-hook 'prog-mode-hook #'prot/cape-prog-setup)

    (defun prot/cape-text-setup ()
      "Set up Cape for prose."
      (prot/cape-super-set-local '(cape-dict cape-dabbrev cape-abbrev cape-emoji) '(cape-file)))

    (setq completion-at-point-functions '(cape-dabbrev cape-file))

    (add-hook 'text-mode-hook #'prot/cape-text-setup)))

(when (eq prot-emacs-completion-in-buffer 'company)
  (prot-emacs-configure
    (prot-emacs-install company)
    (prot-emacs-install company-statistics)

    ;; NOTE 2026-07-10: with the latest version of Company, (setq
    ;; tab-always-indent 'complete) is supported out-of-the-box, which
    ;; means that I can finally disable the automatic popup feature.
    (setq company-idle-delay nil)

    (setq company-tooltip-limit 6)
    (setq company-tooltip-minimum-width 25)
    (setq company-tooltip-align-annotations t)
    (setq company-dabbrev-code-completion-styles t) ; use the `completion-styles'

    (setq company-transformers '(company-sort-by-occurrence
                                 company-sort-by-backend-importance
                                 company-sort-prefer-same-case-prefix))

    (setq company-frontends '(company-childframe-unless-just-one-frontend
                              company-preview-if-just-one-frontend
                              company-echo-metadata-frontend))

    (setq company-backends '((company-capf company-dabbrev) company-files))

    (define-advice company-show-doc-buffer (:around (&rest args) prot)
      (let ((help-window-select nil)
            (display-buffer-alist nil)
            (display-buffer-overriding-action
             '((display-buffer-reuse-mode-window display-buffer-below-selected)
               (window-height . 0.3)
               (post-command-select-window . nil))))
        (apply args)))

    (defun prot/company-prog-backends ()
      "Set `company-backends' locally to my preferred value for `prog-mode' and derivatives."
      (setq-local company-backends '((company-capf company-dabbrev company-abbrev) company-files)))

    (defun prot/company-text-backends ()
      "Set `company-backends' locally to my preferred value for `text-mode' and derivatives."
      (setq-local company-backends '((company-dabbrev company-abbrev company-ispell company-capf) company-files)))

    (prot-emacs-hook (prog-mode-hook comint-mode-hook) prot/company-prog-backends)
    (prot-emacs-hook (prog-mode-hook text-mode-hook comint-mode-hook) company-mode)
    (add-hook 'text-mode-hook #'prot/company-text-backends)

    (with-eval-after-load 'company
      (prot-emacs-keybind company-active-map
        "C-s" #'company-filter-candidates
        "<escape>" #'company-abort
        "<tab>" #'company-complete-selection
        "<up>" #'company-select-previous-or-abort
        "<down>" #'company-select-next-or-abort)
      (company-statistics-mode 1))))

(when (or (eq prot-emacs-completion-in-buffer 'completion-preview)
          (eq prot-emacs-completion-in-buffer 'corfu-completion-preview))
  (prot-emacs-configure
    (setq completion-preview-exact-match-only nil)
    (setq completion-preview-minimum-symbol-length 2)
    (setq completion-preview-idle-delay nil)
    (setq completion-preview-ignore-case t)

    (prot-emacs-hook
      (prog-mode-hook log-edit-mode-hook git-commit-mode-hook comint-mode-hook)
      completion-preview-mode)

    (with-eval-after-load 'completion-preview
      (setq completion-preview-commands
            (seq-remove
             (lambda (command)
               (memq command '(delete-backward-char backward-delete-char-untabify)))
             completion-preview-commands))
      (prot-emacs-keybind completion-preview-active-mode-map
        "M-i" #'completion-preview-insert-word
        "M-n" #'completion-preview-next-candidate
        "M-p" #'completion-preview-prev-candidate
        "M-<return>" #'completion-preview-insert
        ;; With TAB we effectively defer to another frontend to show
        ;; more completion candidates at once.
        "<tab>" #'completion-preview-complete))

    (with-eval-after-load 'org
      (add-to-list 'completion-preview-commands #'org-self-insert-command))))

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
