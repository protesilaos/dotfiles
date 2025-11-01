;;;; Tabs, indentation, and the TAB key
(prot-emacs-configure
  (setq tab-always-indent 'complete)
  (setq tab-first-completion 'word-or-paren-or-punct) ; Emacs 27
  (setq-default tab-width 4
                indent-tabs-mode nil))

;;;; Emacs Lisp major mode
(prot-emacs-configure
  (with-eval-after-load 'elisp-mode
    (prot-emacs-keybind emacs-lisp-mode-map
      "C-c C-b" nil  ; I do not want to byte compile the buffer
      "C-c C-f" nil) ; .. nor the file
    (prot-emacs-keybind lisp-interaction-mode-map
      "C-c C-b" nil
      "C-c C-f" nil)

    ;; Both of these are from Emacs 31.
    (setq elisp-eldoc-funcall-with-docstring-length 'short)
    (setq elisp-eldoc-docstring-length-limit 1000)

    (require 'prot-elisp)

    (prot-emacs-keybind emacs-lisp-mode-map
      "C-j" #'prot-elisp-eval-and-print-last-sexp  ; overrides `electric-newline-and-maybe-indent'
      "C-c C-p" #'prot-elisp-pp-macroexpand-last-sexp)
    (prot-emacs-keybind lisp-interaction-mode-map
      "C-j" #'prot-elisp-eval-and-print-last-sexp ; overrides `eval-print-last-sexp'
      "C-c C-p" #'prot-elisp-pp-macroexpand-last-sexp)))

;;;; Disable "electric" behaviour
(prot-emacs-configure
  (add-hook 'prog-mode-hook #'electric-indent-local-mode)
  (with-eval-after-load 'electric
    ;; I don't like auto indents in Org and related.  They are okay for
    ;; programming.
    (electric-pair-mode -1)
    (electric-quote-mode -1)
    (electric-indent-mode -1)))

;;;; Parentheses (show-paren-mode)
(prot-emacs-configure
  (add-hook 'prog-mode-hook #'show-paren-local-mode)
  (with-eval-after-load 'paren
    (setq show-paren-style 'parenthesis)
    (setq show-paren-when-point-in-periphery nil)
    (setq show-paren-when-point-inside-paren nil)
    (setq show-paren-context-when-offscreen 'overlay))) ; Emacs 29

;;;; Plain text (text-mode)
(use-package text-mode
  :ensure nil
  :mode "\\`\\(README\\|CHANGELOG\\|COPYING\\|LICENSE\\)\\'"
  :hook
  ((text-mode . turn-on-auto-fill)
   (prog-mode . (lambda () (setq-local sentence-end-double-space t))))
  :config
  (setq sentence-end-double-space nil)
  (setq sentence-end-without-period nil)
  (setq colon-double-space nil)
  (setq use-hard-newlines nil)
  (setq adaptive-fill-mode t))

;;;; Arch Linux and AUR package scripts (sh-mode)
(use-package sh-script
  :ensure nil
  :mode ("PKGBUILD" . sh-mode))

;;;; SystemD and other configuration files (conf-mode)
(use-package conf-mode
  :ensure nil
  :mode ("\\`dircolors\\'" "\\.\\(service\\|timer\\)\\'" "dunstrc"))

;;;; Eldoc (Emacs live documentation feedback)
(use-package eldoc
  :ensure nil
  :hook (prog-mode . eldoc-mode)
  :config
  (setq eldoc-idle-delay 1.0)
  (setq eldoc-message-function #'message)) ; don't use mode line for M-x eval-expression, etc.

;;;; Eglot (built-in client for the language server protocol)
(use-package eglot
  :ensure nil
  :functions (eglot-ensure)
  :commands (eglot)
  :config
  (setq eglot-sync-connect nil)
  (setq eglot-autoshutdown t))

;;;; Handle performance for very long lines (so-long.el)
(use-package so-long
  :ensure nil
  :hook (after-init . global-so-long-mode))

;;; Markdown (markdown-mode)
(use-package markdown-mode
  :ensure t
  :defer t
  :config
  (setq markdown-fontify-code-blocks-natively t))

;;; csv-mode
(use-package csv-mode
  :ensure t
  :commands (csv-align-mode))

;;; SXHKDRC mode (one of my many packages)
(use-package sxhkdrc-mode
  :ensure t
  ;; By default, it only applies to the sxhkdrc file, but I have other
  ;; relevant entries as well.  I separate my keys into different
  ;; modules and load only what I need.
  :mode "sxhkdrc_.*")

;;; Flyspell and prot-spell.el (spell check)
(use-package flyspell
  :ensure nil
  :bind
  ( :map flyspell-mode-map
    ("C-;" . nil)
    :map flyspell-mouse-map
    ("<mouse-3>" . flyspell-correct-word)
    :map ctl-x-x-map
    ("s" . flyspell-mode)) ; C-x x s
  :config
  (setq flyspell-issue-message-flag nil)
  (setq flyspell-issue-welcome-flag nil)
  (setq ispell-program-name "aspell")
  (setq ispell-dictionary "en_GB"))

(use-package prot-spell
  :ensure nil
  :bind
  (("M-$" . prot-spell-spell-dwim)
   ("C-M-$" . prot-spell-change-dictionary)
   ("M-i" . prot-spell-spell-dwim) ; override `tab-to-tab-stop'
   ("C-M-i" . prot-spell-change-dictionary)) ; override `complete-symbol'
  :config
  (setq prot-spell-dictionaries
        '(("EN English" . "en")
          ("EL Ελληνικά" . "el")
          ("FR Français" . "fr")
          ("ES Espanõl" . "es")))

  ;; Also check prot-spell.el for what I am doing with
  ;; `prot-spell-ispell-display-buffer'.  Then refer to the
  ;; `display-buffer-alist' for the relevant entry.
  (setq ispell-choices-buffer "*ispell-top-choices*"))

;;; Flymake
(use-package flymake
  :ensure nil
  :preface
  (defvar prot/flymake-mode-projects-path
    (file-name-as-directory (expand-file-name "Projects" "~/Git/"))
    "Path to my Git projects.")

  (defun prot/flymake-mode-lexical-binding ()
    (when lexical-binding
      (flymake-mode 1)))

  (defun prot/flymake-mode-in-my-projects ()
    (when-let* ((file (buffer-file-name))
                ((string-prefix-p prot/flymake-mode-projects-path (expand-file-name file)))
                ((not (file-directory-p file)))
                ((file-regular-p file)))
      (add-hook 'find-file-hook #'prot/flymake-mode-lexical-binding nil t)))

  (add-hook 'emacs-lisp-mode-hook #'prot/flymake-mode-in-my-projects)
  :bind
  ( :map ctl-x-x-map
    ("m" . flymake-mode) ; C-x x m
    :map flymake-mode-map
    ("C-c ! s" . flymake-start)
    ("C-c ! d" . flymake-show-buffer-diagnostics) ; Emacs28
    ("C-c ! D" . flymake-show-project-diagnostics) ; Emacs28
    ("C-c ! n" . flymake-goto-next-error)
    ("C-c ! p" . flymake-goto-prev-error))
  :config
  (setq flymake-fringe-indicator-position 'left-fringe)
  (setq flymake-suppress-zero-counters t)
  (setq flymake-no-changes-timeout nil)
  (setq flymake-start-on-flymake-mode t)
  (setq flymake-start-on-save-buffer t)
  (setq flymake-proc-compilation-prevents-syntax-check t)
  (setq flymake-wrap-around nil)
  (setq flymake-mode-line-format
        '("" flymake-mode-line-exception flymake-mode-line-counters))
  ;; NOTE 2023-07-03: `prot-modeline.el' actually defines the counters
  ;; itself and ignores this.
  (setq flymake-mode-line-counter-format
        '("" flymake-mode-line-error-counter
          flymake-mode-line-warning-counter
          flymake-mode-line-note-counter ""))
  (setq flymake-show-diagnostics-at-end-of-line nil)) ; Emacs 30

;;; Elisp packaging requirements
(use-package package-lint-flymake
  :ensure t
  :after flymake
  :config
  (add-hook 'flymake-diagnostic-functions #'package-lint-flymake))

;;; General configurations for prose/writing

;;;; `outline' (`outline-mode' and `outline-minor-mode')
(use-package outline
  :ensure nil
  :bind
  ("<f10>" . outline-minor-mode)
  :config
  (setq outline-minor-mode-highlight nil) ; emacs28
  (setq outline-minor-mode-cycle t) ; emacs28
  (setq outline-minor-mode-use-buttons nil) ; emacs29---bless you for the nil option!
  (setq outline-minor-mode-use-margins nil)) ; as above

;;;; `dictionary'
(use-package dictionary
  :ensure nil
  :bind ("C-c d" . dictionary-search)
  :config
  (setq dictionary-server "dict.org"
        dictionary-default-popup-strategy "lev" ; read doc string
        dictionary-create-buttons nil
        dictionary-use-single-buffer t))

;;; aLtCaPs
;; Read the manual: <https://protesilaos.com/emacs/altcaps>.
(use-package altcaps
  :ensure t
  :bind
  ("C-x C-a" . altcaps-dwim)
  :config
  ;; Force letter casing for certain characters (for legibility).
  (setq altcaps-force-character-casing
        '(;; Greek theta
          (?θ . downcase))))

;;; Denote (simple note-taking and file-naming)

;; Read the manual: <https://protesilaos.com/emacs/denote>.  This does
;; not include all the useful features of Denote.  I have a separate
;; private setup for those, as I need to test everything is in order.
(use-package denote
  :ensure t
  :hook
  ;; If you use Markdown or plain text files you want to fontify links
  ;; upon visiting the file (Org renders links as buttons right away).
  ((text-mode . denote-fontify-links-mode-maybe)
   ;; Highlight Denote file names in Dired buffers.  Below is the
   ;; generic approach, which is great if you rename files Denote-style
   ;; in lots of places as I do.
   ;;
   ;; If you only want the `denote-dired-mode' in select directories,
   ;; then modify the variable `denote-dired-directories' and use the
   ;; following instead:
   ;;
   ;;  (dired-mode . denote-dired-mode-in-directories)
   (dired-mode . denote-dired-mode))
  :bind
  ;; Denote DOES NOT define any key bindings.  This is for the user to
  ;; decide.  Here I only have a subset of what Denote offers.
  ( :map global-map
    ("C-c n n" . denote)
    ("C-c n N" . denote-type)
    ("C-c n d" . denote-sort-dired)
    ;; Note that `denote-rename-file' can work from any context, not
    ;; just Dired buffers.  That is why we bind it here to the
    ;; `global-map'.
    ;;
    ;; Also see `denote-rename-file-using-front-matter' further below.
    ("C-c n r" . denote-rename-file)
    ;; If you intend to use Denote with a variety of file types, it is
    ;; easier to bind the link-related commands to the `global-map', as
    ;; shown here.  Otherwise follow the same pattern for
    ;; `org-mode-map', `markdown-mode-map', and/or `text-mode-map'.
    :map text-mode-map
    ("C-c n i" . denote-link) ; "insert" mnemonic
    ("C-c n I" . denote-add-links)
    ("C-c n b" . denote-backlinks)
    ;; Also see `denote-rename-file' further above.
    ("C-c n R" . denote-rename-file-using-front-matter)
    ;; Key bindings specifically for Dired.
    :map dired-mode-map
    ("C-c C-d C-i" . denote-dired-link-marked-notes)
    ("C-c C-d C-r" . denote-dired-rename-marked-files)
    ("C-c C-d C-k" . denote-dired-rename-marked-files-with-keywords)
    ("C-c C-d C-f" . denote-dired-rename-marked-files-using-front-matter))
  :config
  ;; Remember to check the doc strings of those variables.
  (setq denote-directory (expand-file-name "~/Documents/notes/"))
  (setq denote-file-type 'text) ; Org is the default file type

  ;; If you want to have a "controlled vocabulary" of keywords,
  ;; meaning that you only use a predefined set of them, then you want
  ;; `denote-infer-keywords' to be nil and `denote-known-keywords' to
  ;; have the keywords you need.
  (setq denote-known-keywords '("emacs" "philosophy" "politics"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-excluded-directories-regexp nil)
  (setq denote-date-format nil) ; read its doc string
  (setq denote-rename-confirmations nil) ; CAREFUL with this if you are not familiar with Denote!
  (setq denote-backlinks-show-context nil)
  (setq denote-buffer-name-prefix "[D] ") ; to identify all Denote buffers
  (setq denote-rename-buffer-format "%D")

  (denote-rename-buffer-mode 1)

  ;; ;; EXPERIMENT 2025-04-25: This is not the default order, though
  ;; ;; Denote supports any order for its file name components.
  ;; (setq denote-file-name-components-order '(identifier signature keywords title))

  (defun prot/denote-rename-all-to-reorder-components ()
    "Call `denote-dired-rename-files' without any prompts.
In other words, preserve the value of each Denote file name component.

Use this command if you want to modify the user option
`denote-file-name-components-order' and then want your files to
retroactively follow that order."
    (interactive)
    (let ((denote-prompts nil))
      (call-interactively 'denote-dired-rename-files))))

;;;; Integrate Consult with Denote

(when prot-emacs-completion-extras
  (use-package consult-denote
    :ensure t
    :bind
    (("C-c n f" . consult-denote-find)
     ("C-c n g" . consult-denote-grep))
    :config
    (consult-denote-mode 1)))

;;;; Denote Org extras (denote-org)

(use-package denote-org
  :ensure t
  :commands
  ( denote-org-link-to-heading
    denote-org-backlinks-for-heading

    denote-org-extract-org-subtree

    denote-org-convert-links-to-file-type
    denote-org-convert-links-to-denote-type

    denote-org-dblock-insert-files
    denote-org-dblock-insert-links
    denote-org-dblock-insert-backlinks
    denote-org-dblock-insert-missing-links
    denote-org-dblock-insert-files-as-headings))

;;;; Denote Sequence notes or folgezettel (denote-sequence)

(use-package denote-sequence
  :ensure t
  :bind
  ( :map global-map
    ;; Here we make "C-c n s" a prefix for all "[n]otes with [s]equence".
    ;; This is just for demonstration purposes: use the key bindings
    ;; that work for you.  Also check the commands:
    ;;
    ;; - `denote-sequence-new-parent'
    ;; - `denote-sequence-new-sibling'
    ;; - `denote-sequence-new-child'
    ;; - `denote-sequence-new-child-of-current'
    ;; - `denote-sequence-new-sibling-of-current'
    ("C-c n s s" . denote-sequence)
    ("C-c n s f" . denote-sequence-find)
    ("C-c n s l" . denote-sequence-link)
    ("C-c n s d" . denote-sequence-dired)
    ("C-c n s r" . denote-sequence-reparent)
    ("C-c n s c" . denote-sequence-convert))
  :config
  ;; The default sequence scheme is `numeric'.
  (setq denote-sequence-scheme 'alphanumeric))

;;;; Denote Markdown extras (denote-markdown)

(use-package denote-markdown
  :ensure t
  :commands ( denote-markdown-convert-links-to-file-paths
              denote-markdown-convert-links-to-denote-type
              denote-markdown-convert-links-to-obsidian-type
              denote-markdown-convert-obsidian-links-to-denote-type ))

;;;; Denote Journal extras (denote-journal)

(use-package denote-journal
  :ensure t
  :commands ( denote-journal-new-entry
              denote-journal-new-or-existing-entry
              denote-journal-link-or-create-entry
              prot/denote-journal-new-or-existing-entry )
  :bind ("C-c n j" . prot/denote-journal-new-or-existing-entry)
  :hook (calendar-mode . denote-journal-calendar-mode)
  :config
  ;; Use the "journal" subdirectory of the `denote-directory'.  Set this
  ;; to nil to use the `denote-directory' instead.
  (setq denote-journal-directory (expand-file-name "journal" denote-directory))
  ;; Default keyword for new journal entries.  It can also be a list of strings.
  (setq denote-journal-keyword "journal")
  ;; Read the doc string of `denote-journal-title-format'.
  (setq denote-journal-title-format 'day-date-month-year)

  (defun prot/denote-journal-new-or-existing-entry ()
    "EXPERIMENTAL Like `denote-journal-new-or-existing-entry' but with no front matter."
    (interactive)
    (cl-letf (((symbol-function #'denote--format-front-matter) (lambda (&rest _) ""))
              (denote-file-type 'text)
              (denote-journal-title-format ""))
      (let* ((internal-date (current-time))
             (files (denote-journal--entry-today internal-date)))
        (if files
            (find-file (denote-journal-select-file-prompt files))
          (call-interactively 'denote-journal-new-entry))))))

;;;; Denote Silo extras (denote-silo)

(use-package denote-silo
  :ensure t
  ;; Bind these commands to key bindings of your choice.
  :commands ( denote-silo-create-note
              denote-silo-open-or-create
              denote-silo-select-silo-then-command
              denote-silo-dired
              denote-silo-cd )
  :config
  ;; Add your silos to this list.  By default, it only includes the
  ;; value of the variable `denote-directory'.
  (setq denote-silo-directories
        (list denote-directory
              "~/Documents/books/"
              "~/Documents/denote-test-silo/")))

;;; Custom extensions for "focus mode" (logos.el)
;; Read the manual: <https://protesilaos.com/emacs/logos>.
(use-package olivetti
  :ensure t
  :commands (olivetti-mode)
  :config
  (setq-default olivetti-body-width 0.7)
  (setq olivetti-minimum-body-width 80)
  (setq olivetti-recall-visual-line-mode-entry-state t))

(use-package logos
  :ensure t
  :bind
  (("C-x n n" . logos-narrow-dwim)
   ("C-x ]" . logos-forward-page-dwim)
   ("C-x [" . logos-backward-page-dwim)
   ;; I don't think I ever saw a package bind M-] or M-[...
   ("M-]" . logos-forward-page-dwim)
   ("M-[" . logos-backward-page-dwim)
   ("<f9>" . logos-focus-mode))
  :config
  (setq logos-outlines-are-pages t)
  (setq logos-outline-regexp-alist
        `((emacs-lisp-mode . ,(format "\\(^;;;+ \\|%s\\)" logos-page-delimiter))
          (org-mode . ,(format "\\(^\\*+ +\\|^-\\{5\\}$\\|%s\\)" logos-page-delimiter))
          (markdown-mode . ,(format "\\(^\\#+ +\\|^[*-]\\{5\\}$\\|^\\* \\* \\*$\\|%s\\)" logos-page-delimiter))
          (conf-toml-mode . "^\\[")))

  ;; These apply when `logos-focus-mode' is enabled.  Their value is
  ;; buffer-local.
  (setq-default logos-hide-mode-line t)
  (setq-default logos-hide-header-line t)
  (setq-default logos-hide-buffer-boundaries t)
  (setq-default logos-hide-fringe t)
  (setq-default logos-variable-pitch t) ; see my `fontaine' configurations
  (setq-default logos-buffer-read-only nil)
  (setq-default logos-scroll-lock nil)
  (setq-default logos-olivetti t)

  (add-hook 'enable-theme-functions #'logos-update-fringe-in-buffers)

;;;; Extra tweaks
  ;; place point at the top when changing pages, but not in `prog-mode'
  (defun prot/logos--recenter-top ()
    "Use `recenter' to reposition the view at the top."
    (unless (derived-mode-p 'prog-mode)
      (recenter 1))) ; Use 0 for the absolute top

  (add-hook 'logos-page-motion-hook #'prot/logos--recenter-top))

(provide 'prot-emacs-langs)
