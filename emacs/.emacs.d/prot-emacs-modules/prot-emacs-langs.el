;;; Paragraphs and fill-mode
(setq sentence-end-double-space t)
(setq sentence-end-without-period nil)
(setq colon-double-space nil)
(setq use-hard-newlines nil)
(setq adaptive-fill-mode t)
(add-hook 'text-mode-hook #'turn-on-auto-fill)

;;; eglot (lsp client built into Emacs 29+)
(prot-emacs-builtin-package 'eglot)

;;; Plain text (text-mode)
(prot-emacs-builtin-package 'text-mode
  (add-to-list 'auto-mode-alist '("\\(README\\|CHANGELOG\\|COPYING\\|LICENSE\\)\\'" . text-mode)))

;;; Markdown (markdown-mode)
(prot-emacs-elpa-package 'markdown-mode
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (setq markdown-fontify-code-blocks-natively t))

;;; YAML (yaml-mode)
(prot-emacs-elpa-package 'yaml-mode
  (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode)))

;;; Shell scripts (sh-mode)
(prot-emacs-builtin-package 'sh-script
  (add-to-list 'auto-mode-alist '("PKGBUILD" . sh-mode)))

;;; SXHKDRC mode (one of my many packages)
(prot-emacs-elpa-package 'sxhkdrc-mode
  ;; By default, it only applies to the sxhkdrc file, but I have other
  ;; relevant entries as well.  I separate my keys into different
  ;; modules and load only what I need.
  (add-to-list 'auto-mode-alist '("sxhkdrc_.*" . sxhkdrc-mode)))

;;; JavaScript and extras
;; I build Emacs from source with support for tree-sitter.  Since I
;; use Arch Linux, I also install the `typescript-language-server' and
;; `aur/tree-sitter-javascript-git' packages (still not sure which
;; ones I require).  I learnt about the language server by inspecting
;; the value of `eglot-server-programs'.
(prot-emacs-builtin-package 'js
  ;; Enables word motions inside camelCase words.
  (dolist (hook '(js-mode-hook js-ts-mode-hook js-comint-mode-hook))
    (add-hook hook #'subword-mode)))

(prot-emacs-elpa-package 'js-comint
  (setopt js-comint-prompt "🦄> ")

  (defun prot/js-comint-eval-keys ()
    "Bind local keys for JavaScript evaluation."
    (local-set-key (kbd "C-c C-z") #'js-comint-repl)
    ;; These are analogous to what we have for Elisp
    (local-set-key (kbd "C-x C-e") #'js-comint-send-last-sexp)
    (local-set-key (kbd "C-C C-e") #'js-comint-send-buffer))

  (dolist (hook '(js-mode-hook js-ts-mode-hook))
    (add-hook hook #'prot/js-comint-eval-keys)))

;;; HTML and related (sgml-mode and mhtml-mode)
(prot-emacs-builtin-package 'sgml-mode
  (require 'mhtml-mode)
  (setq mhtml-tag-relative-indent t)

  (setq html-tag-face-alist
        '(("b" bold)
          ("big" bold)
          ("blink" secondary-selection)
          ("cite" italic)
          ("em" italic)
          ("h1" bold)
          ("h2" bold)
          ("h3" bold)
          ("h4" bold)
          ("h5" bold)
          ("h6" bold)
          ("i" italic)
          ("rev" . underline) ; What is this?  Adding `underline' to spot it and decide afterwards
          ("s" . underline) ; Same as above
          ("small" . default)
          ("strong" . bold)
          ("title" bold)
          ("tt" . default)
          ("u" . underline)
          ("var" . italic)))

  (defun prot/html-special-variables ()
    "Define buffer-local values for HTML development.
These constitute deviations from my normal preferences.  I need
this arrangement for code I write that is not mine and thus is
not necessarily aligned with my opinions.

NOTE: I will expand this as needs arise."
    (setq-local browse-url-browser-function 'browse-url-default-browser)
    (electric-indent-local-mode 1))

  (defvar prot/html-browsers '(firefox chromium epiphany)
    "List of browsers for use in `prot/html-select-browser'.")

  (defvar prot/html--select-browser-history '()
    "Minibuffer history of `prot/html-select-browser'.")

  (defun prot/html-select-browser (browser)
    "Pick BROWSER for local `browse-url-browser-function'.
BROWSER is a symbol among `prot/html-browsers'.  Any other value
falls back to `browse-url-default-browser'.

Do this to change what happens when clicking on a link.  It is
useful for testing an HTML document."
    (require 'browse-url)
    (interactive
     (list (intern (completing-read "Set new default browser: "
                                    prot/html-browsers nil t nil
                                    'prot/html--select-browser-history))))
    (setq-local browse-url-browser-function
                (pcase browser
                  ('firefox 'browse-url-firefox)
                  ('chromium 'browse-url-chromium)
                  ('epiphany 'browse-url-epiphany)
                  (_ 'browse-url-default-browser))))

  ;; The underlying `sgml-mode' is derived from `text-mode'.  I
  ;; normally want `auto-fill-mode' for "text", though to me HTML and
  ;; friends should be counted as "code".
  (dolist (hook '(sgml-mode-hook html-mode-hook mhtml-mode-hook))
    (add-hook hook #'turn-off-auto-fill)
    (add-hook hook #'prot/html-special-variables))

  (let ((map html-mode-map))
    (define-key map (kbd "M-o") nil) ; send `facemenu-keymap' into oblivion; thanks!
    (define-key map (kbd "C-c C-s") #'prot/html-select-browser)
    (define-key map (kbd "C-c C-v") #'html-autoview-mode)))

;;; CSS (css-mode, scss-mode)
(prot-emacs-builtin-package 'css-mode
  (add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))
  (add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
  (setq css-indent-offset 2)
  (setq css-fontify-colors nil)

  (defun prot/scss-to-css ()
    "Convert current SCSS file to CSS."
    (interactive)
    (unless (executable-find "sassc")
      (user-error "No `sassc' executable found; aborting"))
    (let* ((path (buffer-file-name))
           (name (file-name-nondirectory path))
           (name-no-ext (file-name-base path)))
      (if (buffer-modified-p)
          (user-error "Save changes before converting SCSS to CSS")
        (shell-command (format "sassc -t expanded %s %s.css" name name-no-ext)))))

  (define-key scss-mode-map (kbd "C-c C-e") #'prot/scss-to-css)) ; "export" mnemonic

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
  (setq show-paren-context-when-offscreen 'overlay) ; Emacs 29
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
  (define-key flyspell-mode-map (kbd "C-;") nil)
  (define-key ctl-x-x-map "s" #'flyspell-mode)) ; C-x x s

(prot-emacs-builtin-package 'prot-spell
  (setq prot-spell-dictionaries
        '(("EN English" . "en")
          ("EL Ελληνικά" . "el")
          ("FR Français" . "fr")
          ("ES Espanõl" . "es")))

  ;; Also check prot-spell.el for what I am doing with
  ;; `prot-spell-ispell-display-buffer'.  Then refer to the
  ;; `display-buffer-alist' for the relevant entry.

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

  (defvar prot/flymake-mode-projects-path
    (file-name-as-directory (expand-file-name "Projects" "~/Git/"))
    "Path to my Git projects.")

  (defun prot/flymake-mode-lexical-binding ()
    (when lexical-binding
      (flymake-mode 1)))

  (defun prot/flymake-mode-in-my-projects ()
    (when-let* ((file (buffer-file-name))
                ((string-prefix-p prot/flymake-mode-projects-path
                                  (expand-file-name file)))
                ((not (file-directory-p file)))
                ((file-regular-p file)))
      (add-hook 'find-file-hook #'prot/flymake-mode-lexical-binding nil t)))

  (add-hook 'emacs-lisp-mode-hook #'prot/flymake-mode-in-my-projects)

  (define-key ctl-x-x-map "m" #'flymake-mode) ; C-x x m
  (let ((map flymake-mode-map))
    (define-key map (kbd "C-c ! s") #'flymake-start)
    (define-key map (kbd "C-c ! d") #'flymake-show-buffer-diagnostics) ; Emacs28
    (define-key map (kbd "C-c ! D") #'flymake-show-project-diagnostics) ; Emacs28
    (define-key map (kbd "C-c ! n") #'flymake-goto-next-error)
    (define-key map (kbd "C-c ! p") #'flymake-goto-prev-error)))

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

;;; Handle performance for very long lines (so-long.el)
(prot-emacs-builtin-package 'so-long
  (global-so-long-mode 1))

(provide 'prot-emacs-langs)
