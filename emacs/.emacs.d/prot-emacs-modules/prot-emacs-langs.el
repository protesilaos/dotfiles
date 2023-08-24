;;; General language/editing settings
(prot-emacs-configure
  (:delay 5)
;;;; Tabs, indentation, and the TAB key
  (setq-default tab-always-indent 'complete
                tab-first-completion 'word-or-paren-or-punct ; Emacs 27
                tab-width 4
                indent-tabs-mode nil)

;;;; Disable "electric" behaviour
  (electric-pair-mode -1)
  (electric-quote-mode -1)
  ;; I don't like auto indents in Org and related.  They are okay for
  ;; programming.
  (electric-indent-mode -1)
  (add-hook 'prog-mode-hook #'electric-indent-local-mode)

;;;; Parentheses (show-paren-mode)
  (setq show-paren-style 'parenthesis)
  (setq show-paren-when-point-in-periphery nil)
  (setq show-paren-when-point-inside-paren nil)
  (setq show-paren-context-when-offscreen 'overlay) ; Emacs 29
  (add-hook 'after-init-hook #'show-paren-mode)

;;;; Plain text (text-mode)
  (setq sentence-end-double-space t)
  (setq sentence-end-without-period nil)
  (setq colon-double-space nil)
  (setq use-hard-newlines nil)
  (setq adaptive-fill-mode t)

  (add-hook 'text-mode-hook #'turn-on-auto-fill)

  (add-to-list 'auto-mode-alist '("\\(README\\|CHANGELOG\\|COPYING\\|LICENSE\\)\\'" . text-mode))

;;;; Arch Linux and AUR package scripts (sh-mode)
  (add-to-list 'auto-mode-alist '("PKGBUILD" . sh-mode))

;;;; SystemD and other configuration files (conf-mode)
  (add-to-list 'auto-mode-alist '("\\.\\(service\\|timer\\)\\'" . conf-mode))
  (add-to-list 'auto-mode-alist '("dircolors" . conf-mode))

;;;; Eldoc (elisp live documentation feedback)
  (setq eldoc-message-function #'message) ; don't use mode line for M-x eval-expression, etc.
  (global-eldoc-mode 1)

;;;; Handle performance for very long lines (so-long.el)
  (global-so-long-mode 1))

;;; Markdown (markdown-mode)
(prot-emacs-package markdown-mode
  (:install t)
  (:delay 5)
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (setq markdown-fontify-code-blocks-natively t))


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

;;; Jinx (highly performant spell checker)
(prot-emacs-package jinx
  (:install t)
  (:delay 10)
  (setq jinx-languages "en_GB el_GR fr_FR es_ES pt_PT-preao")
  (setq jinx-include-modes '(text-mode prog-mode))
  (setq jinx-include-faces
        '((prog-mode font-lock-doc-face)
          (conf-mode font-lock-comment-face)))
  (setq jinx-exclude-regexps
        '((t "[A-Z]+\\>"
             "\\<[[:upper:]][[:lower:]]+\\>"
             "\\w*?[0-9\.'\"-]\\w*"
             "[a-z]+://\\S-+"
             "<?[-+_.~a-zA-Z][-+_.~:a-zA-Z0-9]*@[-.a-zA-Z0-9]+>?")))

  (global-jinx-mode 1)

  (define-key ctl-x-x-map "j" #'jinx-mode) ; C-x x j

  (prot-emacs-keybind global-map
    "M-$" #'jinx-correct
    "C-M-$" #'jinx-languages))

;;; Flyspell and prot-spell.el (spell check)
;; (prot-emacs-package flyspell
;;   (:delay 10)
;;   (setq flyspell-issue-message-flag nil)
;;   (setq flyspell-issue-welcome-flag nil)
;;   (setq ispell-program-name "aspell")
;;   (setq ispell-dictionary "en_GB")
;;   (define-key flyspell-mode-map (kbd "C-;") nil)
;;   (define-key ctl-x-x-map "s" #'flyspell-mode)) ; C-x x s
;;
;; (prot-emacs-package prot-spell
;;   (:delay 10)
;;   (setq prot-spell-dictionaries
;;         '(("EN English" . "en")
;;           ("EL Ελληνικά" . "el")
;;           ("FR Français" . "fr")
;;           ("ES Espanõl" . "es")))
;;
;;   (setq ispell-choices-buffer "*ispell-top-choices*") ; see my `display-buffer-alist'
;;
;;   ;; Also check prot-spell.el for what I am doing with
;;   ;; `prot-spell-ispell-display-buffer'.  Then refer to the
;;   ;; `display-buffer-alist' for the relevant entry.
;;
;;   (prot-emacs-keybind global-map
;;     "M-$" #'prot-spell-spell-dwim
;;     "C-M-$" #'prot-spell-change-dictionary))

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
  ;; NOTE 2023-07-03: `prot-modeline.el' actually defines the counters
  ;; itself and ignores this.
  (setq flymake-mode-line-counter-format
        '("" flymake-mode-line-error-counter
          flymake-mode-line-warning-counter
          flymake-mode-line-note-counter ""))
  (setq flymake-show-diagnostics-at-end-of-line nil) ; Emacs 30

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

(provide 'prot-emacs-langs)
