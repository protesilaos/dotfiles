;;; General minibuffer settings
(prot-emacs-configure
;;;; Completion styles
  (setq completion-styles '(basic substring)) ; also see `completion-category-overrides'
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

  ;; Add some missing completion categories to let me configure the
  ;; relevant prompts via the `completion-category-overrides'.
  (define-advice read-from-kill-ring (:around (&rest args) prot)
    (let ((completion-extra-properties (list :category 'kill-ring)))
      (apply args)))

  (define-advice read-library-name (:around (&rest args) prot)
    (let ((completion-extra-properties (list :category 'library)))
      (apply args)))

  (define-advice emoji--read-emoji (:around (&rest args) prot)
    (let ((completion-extra-properties (list :category 'emoji)))
      (apply args)))

  ;; NOTE 2025-12-02: The `eager-display' and `eager-update' are part of Emacs 31.
  (setq completion-category-overrides
        `(,@(mapcar
             (lambda (category)
               (cons category
                     '((styles . (partial-completion))
                       (eager-display . nil)
                       (eager-update . t))))
             '(file bookmark symbol-help))
          (emoji . ((styles . (orderless))
                    (eager-display . t)
                    (eager-update . t)))
          (unicode-name . ((styles . (orderless))
                           (eager-display . nil)
                           (eager-update . t)))
          ,@(mapcar
             (lambda (category)
               (cons category
                     '((styles . (basic substring orderless))
                       (eager-display . t)
                       (eager-update . t))))
             '(buffer project-file eglot kill-ring theme consult-location imenu embark-keybinding library)))))

;;; Orderless completion style (and prot-orderless.el)
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
  (add-hook 'minibuffer-setup-hook #'prot-common-truncate-lines-silently)

  (unless prot-emacs-completion-ui
    (setq completion-show-help nil)
    (setq completion-show-inline-help nil)
    (setq completions-detailed t)
    (setq completions-format 'one-column)
    ;; (setq completions-header-format (propertize "%s candidates:\n" 'face 'bold-italic))
    (setq completions-header-format "")
    (setq completions-highlight-face 'completions-highlight)
    (setq completions-max-height 10)
    (setq completions-sort 'historical)
    (setq completion-auto-help 'always)
    (setq completion-auto-select 'second-tab)
    ;; (setq minibuffer-visible-completions t) ; Emacs 30

    ;; These two are for Emacs 31.  The value they have now means that
    ;; each completion category will have its own behaviour based on
    ;; what I am setting in the `completion-category-overrides'.
    (setq completion-eager-display 'auto)
    (setq completion-eager-update 'auto)

    (defun prot/completions-tweak-style ()
      "Tweak the style of the Completions buffer."
      ;; (setq-local mode-line-format "*Completions*)
      (setq-local cursor-in-non-selected-windows nil)
      (when (and completions-header-format
                 (not (string-blank-p completions-header-format)))
        (setq-local display-line-numbers-offset -1))
      (display-line-numbers-mode 1))

    (prot-emacs-hook
      completion-list-mode-hook
      (prot/completions-tweak-style prot-common-truncate-lines-silently))

    (defun prot/quit-completions ()
      "Always quit the Completions window."
      (when-let* ((window (get-buffer-window "*Completions*")))
        (quit-window nil window)))

    (add-hook 'minibuffer-exit-hook #'prot/quit-completions)

    (defun prot/choose-completion-no-exit ()
      "Call `choose-completion' without exiting the minibuffer.
Also see `prot/choose-completion-exit' and `prot/choose-completion-dwim'."
      (interactive)
      (choose-completion nil :no-exit :no-quit)
      (switch-to-minibuffer))

    (defun prot/choose-completion-exit ()
      "Call `choose-completion' and exit the minibuffer.
Also see `prot/choose-completion-no-exit' and `prot/choose-completion-dwim'."
      (interactive)
      (choose-completion nil :no-exit)
      (exit-minibuffer))

    (defun prot/choose-completion-dwim ()
      "Call `choose-completion' that exits only on a unique match.
If the match is not unique, then complete up to the largest common
prefix or, anyhow, continue with the completion (e.g. in `find-file'
switch into the directory and then show the files therein).

Also see `prot/choose-completion-no-exit' and `prot/choose-completion-exit'."
      (interactive)
      (choose-completion nil :no-exit :no-quit)
      (switch-to-minibuffer)
      (minibuffer-completion-help)
      (unless (get-buffer-window "*Completions*")
        (exit-minibuffer)))

    (define-advice minibuffer-completion-help (:around (&rest args) prot)
      "Make `minibuffer-completion-help' display *Completions* in a side window.
Make the window be at slot 0, such that the *Help* buffer produced by
`prot/completions-describe-at-point' is to its right."
      (let ((display-buffer-overriding-action
             '((display-buffer-reuse-mode-window display-buffer-in-side-window)
               (side . bottom)
               (slot . 0))))
        (apply args)))

    (defun prot/completions-describe-at-point (symbol)
      "Describe SYMBOL at point inside the *Completions* buffer.
Place the *Help* buffer in a side window, situated to the right of the
*Completions* buffer.  Make the window have the `prot-minibuffer-help'
property, such that it can be found by `prot/completions-close-help'."
      (interactive (list (intern-soft (thing-at-point 'symbol))))
      (unless (derived-mode-p 'completion-list-mode)
        (user-error "Can only do this from the *Completions* buffer"))
      (when symbol
        (let ((help-window-select nil)
              (display-buffer-overriding-action
               '((display-buffer-reuse-mode-window display-buffer-in-side-window)
                 (slot . 1) ;  next to `prot/minibuffer-completion-help'
                 (window-parameters . ((prot-minibuffer-help . t))))))
          (describe-symbol symbol))))

    (defun prot/completions-close-help ()
      "Close the window that has a `'prot-minibuffer-help' parameter."
      (when-let* ((help (seq-find
                         (lambda (window)
                           (window-parameter window 'prot-minibuffer-help))
                         (window-list))))
        (delete-window help)))

    (add-hook 'minibuffer-exit-hook #'prot/completions-close-help)

    (prot-emacs-keybind completion-list-mode-map
      "h" #'prot/completions-describe-at-point ; "Help" mnemonic
      "c" #'prot/choose-completion-no-exit ; "Choose" mnemonic
      "TAB" #'prot/choose-completion-dwim
      "RET" #'prot/choose-completion-exit)))

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
(when (and prot-emacs-completion-extras prot-display-graphic-p)
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
