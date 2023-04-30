;;; Plain text (text-mode)
(prot-emacs-package text-mode
  (:delay 5)
  (setq sentence-end-double-space t)
  (setq sentence-end-without-period nil)
  (setq colon-double-space nil)
  (setq use-hard-newlines nil)
  (setq adaptive-fill-mode t)

  (add-hook 'text-mode-hook #'turn-on-auto-fill)

  (add-to-list 'auto-mode-alist '("\\(README\\|CHANGELOG\\|COPYING\\|LICENSE\\)\\'" . text-mode)))

;;; Markdown (markdown-mode)
(prot-emacs-package markdown-mode
  (:install t)
  (:delay 5)
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (setq markdown-fontify-code-blocks-natively t))

;;; Shell scripts (sh-mode)
(prot-emacs-package sh-script
  (:delay 5)
  (add-to-list 'auto-mode-alist '("PKGBUILD" . sh-mode)))

;;; SXHKDRC mode (one of my many packages)
(prot-emacs-package sxhkdrc-mode
  (:install t)
  (:delay 5)
  ;; By default, it only applies to the sxhkdrc file, but I have other
  ;; relevant entries as well.  I separate my keys into different
  ;; modules and load only what I need.
  (add-to-list 'auto-mode-alist '("sxhkdrc_.*" . sxhkdrc-mode)))

;;; Comments (newcomment.el and prot-comment.el)
(prot-emacs-package newcomment
  (:delay 5)
  (setq comment-empty-lines t)
  (setq comment-fill-column nil)
  (setq comment-multi-line t)
  (setq comment-style 'multi-line)
  (prot-emacs-keybind global-map
    "C-:" #'comment-kill ; C-S-;
    "M-;" #'comment-indent))

(prot-emacs-package prot-comment
  (:delay 5)
  (setq prot-comment-comment-keywords
        '("TODO" "NOTE" "XXX" "REVIEW" "FIXME"))
  (setq prot-comment-timestamp-format-concise "%F")
  (setq prot-comment-timestamp-format-verbose "%F %T %z")
  (prot-emacs-keybind global-map
    "C-;" #'prot-comment-comment-dwim
    "C-x C-;" #'prot-comment-timestamp-keyword))

;;; Configure 'electric' behaviour
(prot-emacs-package electric
  (:delay 5)
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
(prot-emacs-package paren
  (:delay 5)
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
(prot-emacs-package flyspell
  (:delay 10)
  (setq flyspell-issue-message-flag nil)
  (setq flyspell-issue-welcome-flag nil)
  (setq ispell-program-name "aspell")
  (setq ispell-dictionary "en_GB")
  (define-key flyspell-mode-map (kbd "C-;") nil)
  (define-key ctl-x-x-map "s" #'flyspell-mode)) ; C-x x s

(prot-emacs-package prot-spell
  (:delay 10)
  (setq prot-spell-dictionaries
        '(("EN English" . "en")
          ("EL Ελληνικά" . "el")
          ("FR Français" . "fr")
          ("ES Espanõl" . "es")))

  (setq ispell-choices-buffer "*ispell-top-choices*") ; see my `display-buffer-alist'

  ;; Also check prot-spell.el for what I am doing with
  ;; `prot-spell-ispell-display-buffer'.  Then refer to the
  ;; `display-buffer-alist' for the relevant entry.

  (prot-emacs-keybind global-map
    "M-$" #'prot-spell-spell-dwim
    "C-M-$" #'prot-spell-change-dictionary))

;;; Flymake
(prot-emacs-package flymake
  (:delay 10)
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
  (prot-emacs-keybind flymake-mode-map
    "C-c ! s" #'flymake-start
    "C-c ! d" #'flymake-show-buffer-diagnostics ; Emacs28
    "C-c ! D" #'flymake-show-project-diagnostics ; Emacs28
    "C-c ! n" #'flymake-goto-next-error
    "C-c ! p" #'flymake-goto-prev-error))

;;; Elisp packaging requirements
(prot-emacs-package package-lint-flymake
  (:install t)
  (:delay 10)
  (add-hook 'flymake-diagnostic-functions #'package-lint-flymake))

;;; Eldoc (elisp live documentation feedback)
(prot-emacs-package eldoc
  (:delay 5)
  (global-eldoc-mode 1))

;;; Handle performance for very long lines (so-long.el)
(prot-emacs-package so-long
  (:delay 10)
  (global-so-long-mode 1))

(provide 'prot-emacs-langs)
