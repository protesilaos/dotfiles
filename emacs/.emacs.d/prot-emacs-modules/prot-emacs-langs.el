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

    ;; All of these are from Emacs 31.
    (setq elisp-eldoc-funcall-with-docstring-length 'short)
    (setq elisp-eldoc-docstring-length-limit 1000)
    (set-default-toplevel-value 'lexical-binding t) ; Emacs 31

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
(prot-emacs-configure
  (add-to-list 'auto-mode-alist '("\\`\\(README\\|CHANGELOG\\|COPYING\\|LICENSE\\)\\'" . text-mode))

  (add-hook 'text-mode-hook #'turn-on-auto-fill)
  (add-hook 'prog-mode-hook (lambda () (setq-local sentence-end-double-space t)))

  (with-eval-after-load 'text-mode
    (setq sentence-end-double-space nil)
    (setq sentence-end-without-period nil)
    (setq colon-double-space nil)
    (setq use-hard-newlines nil)
    (setq adaptive-fill-mode t)))

(prot-emacs-configure
  ;; Arch Linux and AUR package scripts (sh-mode)
  (add-to-list 'auto-mode-alist '("PKGBUILD" . sh-mode))
  ;; SystemD and other configuration files (conf-mode)
  (add-to-list 'auto-mode-alist '("\\`dircolors\\'" "\\.\\(service\\|timer\\)\\'" "dunstrc")))

;;;; Eldoc (Emacs live documentation feedback)
(prot-emacs-configure
  (add-hook 'prog-mode-hook #'eldoc-mode)
  (setq eldoc-idle-delay 1.0)
  (setq eldoc-message-function #'message)) ; don't use mode line for M-x eval-expression, etc.

;;;; Eglot (built-in client for the language server protocol)
(prot-emacs-configure
  (setq eglot-sync-connect nil)
  (setq eglot-autoshutdown t))

;;;; Handle performance for very long lines (so-long.el)
(prot-emacs-configure
  (global-so-long-mode 1))

;;; Markdown (markdown-mode)
(prot-emacs-configure
  (prot-emacs-install markdown-mode)
  (setq markdown-fontify-code-blocks-natively t))

;;; csv-mode
(prot-emacs-configure
  (prot-emacs-install csv-mode))

;;; SXHKDRC mode (one of my many packages)
(prot-emacs-configure
  (prot-emacs-install sxhkdrc-mode)
  ;; By default, it only applies to the sxhkdrc file, but I have other
  ;; relevant entries as well.  I separate my keys into different
  ;; modules and load only what I need.
  (add-to-list 'auto-mode-alist '("sxhkdrc_.*" . sxhkdrc-mode)))

;;; Flyspell and prot-spell.el (spell check)
(prot-emacs-configure
  (prot-emacs-autoload
    (prot-spell-spell-dwim
     prot-spell-change-dictionary
     prot-spell-spell-dwim
     prot-spell-change-dictionary)
    "prot-spell")

  (prot-emacs-keybind global-map
    "M-$" #'prot-spell-spell-dwim
    "C-M-$" #'prot-spell-change-dictionary
    "M-i" #'prot-spell-spell-dwim ; override `tab-to-tab-stop
    "C-M-i" #'prot-spell-change-dictionary) ; override `complete-symbol'

  (with-eval-after-load 'flyspell
    (define-key flyspell-mode-map (kbd "C-;") nil)
    (define-key flyspell-mouse-map (kbd "<mouse-3>") #'flyspell-correct-word)
    (define-key ctl-x-x-map (kbd "s") #'flyspell-mode) ; C-x x s

    (setq flyspell-issue-message-flag nil)
    (setq flyspell-issue-welcome-flag nil)
    (setq ispell-program-name "aspell")
    (setq ispell-dictionary "en_GB")

    (setq prot-spell-dictionaries
          '(("EN English" . "en")
            ("EL Ελληνικά" . "el")
            ("FR Français" . "fr")
            ("ES Espanõl" . "es")))

    ;; Also check prot-spell.el for what I am doing with
    ;; `prot-spell-ispell-display-buffer'.  Then refer to the
    ;; `display-buffer-alist' for the relevant entry.
    (setq ispell-choices-buffer "*ispell-top-choices*")))

;;; Flymake
(prot-emacs-configure
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

  (define-key ctl-x-x-map (kbd "m") #'flymake-mode) ; C-x x m

  (with-eval-after-load 'flymake
    (prot-emacs-keybind flymake-mode-map
      "C-c ! s" #'flymake-start
      "C-c ! d" #'flymake-show-buffer-diagnostics ; Emacs28
      "C-c ! D" #'flymake-show-project-diagnostics ; Emacs28
      "C-c ! n" #'flymake-goto-next-error
      "C-c ! p" #'flymake-goto-prev-error)

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
    (setq flymake-show-diagnostics-at-end-of-line nil)) ; Emacs 31

;;; Elisp packaging requirements
  (prot-emacs-install package-lint-flymake)
  (add-hook 'flymake-diagnostic-functions #'package-lint-flymake))

;;; General configurations for prose/writing

;;;; `outline' (`outline-mode' and `outline-minor-mode')
(prot-emacs-configure
  (define-key global-map (kbd "<f10>") #'outline-minor-mode)
  (setq outline-minor-mode-highlight nil) ; emacs28
  (setq outline-minor-mode-cycle t) ; emacs28
  (setq outline-minor-mode-use-buttons nil) ; emacs29---bless you for the nil option!
  (setq outline-minor-mode-use-margins nil)) ; as above

;;;; `dictionary'
(prot-emacs-configure
  (define-key global-map (kbd "C-c d") #'dictionary-search)
  (setq dictionary-server "dict.org")
  (setq dictionary-default-popup-strategy "lev") ; read doc string
  (setq dictionary-create-buttons nil)
  (setq dictionary-use-single-buffer t))

;;; aLtCaPs
;; Read the manual: <https://protesilaos.com/emacs/altcaps>.
(prot-emacs-configure
  (prot-emacs-install altcaps)
  (define-key global-map (kbd "C-x C-a") #'altcaps-dwim)
  ;; Force letter casing for certain characters (for legibility).
  (setq altcaps-force-character-casing
        '(;; Greek theta
          (?θ . downcase))))

;;; Denote (simple note-taking and file-naming)

;; Read the manual: <https://protesilaos.com/emacs/denote>.  This does
;; not include all the useful features of Denote.  I have a separate
;; private setup for those, as I need to test everything is in order.
(prot-emacs-configure
  (prot-emacs-install denote)
  (add-hook 'text-mode-hook #'denote-fontify-links-mode-maybe)
  (add-hook 'dired-mode-hook #'denote-dired-mode)

  (prot-emacs-keybind global-map
    "C-c n n" #'denote
    "C-c n N" #'denote-type
    "C-c n d" #'denote-sort-dired
    "C-c n r" #'denote-rename-file)

  (with-eval-after-load 'text-mode
    (prot-emacs-keybind text-mode-map
      "C-c n i" #'denote-link ; "insert" mnemonic
      "C-c n I" #'denote-add-links
      "C-c n b" #'denote-backlinks
      "C-c n R" #'denote-rename-file-using-front-matter))

  (with-eval-after-load 'dired
    (prot-emacs-keybind dired-mode-map
      "C-c C-d C-i" #'denote-dired-link-marked-notes
      "C-c C-d C-r" #'denote-dired-rename-marked-files
      "C-c C-d C-k" #'denote-dired-rename-marked-files-with-keywords
      "C-c C-d C-f" #'denote-dired-rename-marked-files-using-front-matter))

  (with-eval-after-load 'denote
    (setq denote-directory (expand-file-name "~/Documents/notes/"))
    (setq denote-file-type 'text) ; Org is the default file type

    (setq denote-known-keywords '("emacs" "philosophy" "politics"))
    (setq denote-infer-keywords t)
    (setq denote-sort-keywords t)
    (setq denote-excluded-directories-regexp nil)
    (setq denote-date-format nil)
    (setq denote-rename-confirmations nil) ; CAREFUL with this if you are not familiar with Denote!
    (setq denote-backlinks-show-context nil)
    (setq denote-buffer-name-prefix "[D] ")
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
        (call-interactively 'denote-dired-rename-files)))))

;;;; Integrate Consult with Denote

(when prot-emacs-completion-extras
  (prot-emacs-configure
    (prot-emacs-install consult-denote)
    (prot-emacs-keybind global-map
      "C-c n f" #'consult-denote-find
      "C-c n g" #'consult-denote-grep)
    (with-eval-after-load 'denote
      (consult-denote-mode 1))))

;;;; Denote Org extras (denote-org)
(prot-emacs-configure
  (prot-emacs-install denote-org))

;;;; Denote Sequence notes or folgezettel (denote-sequence)
(prot-emacs-configure
  (prot-emacs-install denote-sequence)
  (prot-emacs-keybind global-map
    "C-c n s s" #'denote-sequence
    "C-c n s f" #'denote-sequence-find
    "C-c n s l" #'denote-sequence-link
    "C-c n s d" #'denote-sequence-dired
    "C-c n s r" #'denote-sequence-reparent
    "C-c n s c" #'denote-sequence-convert)
  (setq denote-sequence-scheme 'alphanumeric))

;;;; Denote Markdown extras (denote-markdown)
(prot-emacs-configure
  (prot-emacs-install denote-markdown))

;;;; Denote Journal extras (denote-journal)
(prot-emacs-configure
  (prot-emacs-install denote-journal)

  (add-hook 'calendar-mode-hook #'denote-journal-calendar-mode)

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
          (call-interactively 'denote-journal-new-entry)))))

  (define-key global-map (kbd "C-c n j") #'prot/denote-journal-new-or-existing-entry)

  (with-eval-after-load 'denote
    (setq denote-journal-directory (expand-file-name "journal" denote-directory))
    (setq denote-journal-keyword "journal")
    (setq denote-journal-title-format 'day-date-month-year)))

;;;; Denote Silo extras (denote-silo)
(prot-emacs-configure
  (prot-emacs-install denote-silo)
  (with-eval-after-load 'denote
    (setq denote-silo-directories
          (append (denote-directories)
                  (list "~/Documents/books/" "~/Documents/denote-test-silo/")))))

;;; Custom extensions for "focus mode" (logos.el)
;; Read the manual: <https://protesilaos.com/emacs/logos>.
(prot-emacs-configure
  (prot-emacs-install olivetti)
  (setq-default olivetti-body-width 0.7)
  (setq olivetti-minimum-body-width 80)
  (setq olivetti-recall-visual-line-mode-entry-state t)

  (prot-emacs-install logos)

  (prot-emacs-keybind global-map
    "C-x n n" #'logos-narrow-dwim
    "C-x ]" #'logos-forward-page-dwim
    "C-x [" #'logos-backward-page-dwim
    "M-]" #'logos-forward-page-dwim
    "M-[" #'logos-backward-page-dwim
    "<f9>" #'logos-focus-mode)

  (with-eval-after-load 'logos
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
    (defun prot/logos-recenter-top ()
      "Use `recenter' to reposition the view at the top."
      (unless (derived-mode-p 'prog-mode)
        (recenter 1))) ; Use 0 for the absolute top

    (add-hook 'logos-page-motion-hook #'prot/logos-recenter-top)))

(provide 'prot-emacs-langs)
