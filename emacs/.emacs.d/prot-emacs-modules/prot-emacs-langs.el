;;; General language/editing settings
(prot-emacs-configure
  (:delay 2)

;;;; Tabs, indentation, and the TAB key
  (setq tab-always-indent 'complete)
  (setq tab-first-completion 'word-or-paren-or-punct) ; Emacs 27
  (setq-default tab-width 4
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
  (setq sentence-end-double-space nil)

  (defun prot/prog-mode-sentence-end-double-space ()
    "Set `sentence-end-double-space' to non-nil in the current buffer.
Meant to be added to `prog-mode-hook'."
    (setq-local sentence-end-double-space t))

  (add-hook 'emacs-lisp-mode-hook #'prot/prog-mode-sentence-end-double-space)

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

;;;; Eldoc (Emacs live documentation feedback)
  (setq eldoc-message-function #'message) ; don't use mode line for M-x eval-expression, etc.
  (add-hook 'prog-mode-hook #'eldoc-mode)

;;;; Eglot (built-in client for the language server protocol)
  (setq eglot-sync-connect nil)
  (setq eglot-autoshutdown t)

;;;; Handle performance for very long lines (so-long.el)
  (global-so-long-mode 1))

;;; Markdown (markdown-mode)
(prot-emacs-package markdown-mode
  (:install t)
  (:delay 5)
  (setq markdown-fontify-code-blocks-natively t))

;;; SXHKDRC mode (one of my many packages)
(prot-emacs-package sxhkdrc-mode
  (:install t)
  (:delay 5)
  ;; By default, it only applies to the sxhkdrc file, but I have other
  ;; relevant entries as well.  I separate my keys into different
  ;; modules and load only what I need.
  (add-to-list 'auto-mode-alist '("sxhkdrc_.*" . sxhkdrc-mode)))

;; FIXME 2023-08-28: The laptop I use now that I do not have
;; electricity/Internet at home does not build the Jinx C module.  I
;; am not sure what is happening.  Commenting out for the time being,
;; as I need to have Emacs up and running.

;;; Jinx (highly performant spell checker)
;; (if (executable-find "enchant-2")
;;   (prot-emacs-package jinx
;;     (:install t)
;;     (:delay 10)
;;     (setq jinx-languages "en_GB el_GR fr_FR es_ES pt_PT-preao")
;;     (setq jinx-include-modes '(text-mode prog-mode))
;;     (setq jinx-include-faces
;;           '((prog-mode font-lock-doc-face)
;;             (conf-mode font-lock-comment-face)))
;;     (setq jinx-exclude-regexps
;;           '((t "[A-Z]+\\>"
;;                "\\<[[:upper:]][[:lower:]]+\\>"
;;                "\\w*?[0-9\.'\"-]\\w*"
;;                "[a-z]+://\\S-+"
;;                "<?[-+_.~a-zA-Z][-+_.~:a-zA-Z0-9]*@[-.a-zA-Z0-9]+>?")))
;;
;;     (define-key ctl-x-x-map "j" #'jinx-mode) ; C-x x j
;;
;;     (prot-emacs-keybind global-map
;;       "M-$" jinx-correct
;;       "C-M-$" jinx-languages))
;;   ;; I would use an `error' but I do not want it to interrupt startup.
;;   (message "libenchant is not available"))

;;; Flyspell and prot-spell.el (spell check)
;; See FIXME for `jinx'.
(prot-emacs-package flyspell
  (:delay 30)
  (setq flyspell-issue-message-flag nil)
  (setq flyspell-issue-welcome-flag nil)
  (setq ispell-program-name "aspell")
  (setq ispell-dictionary "en_GB")
  (define-key flyspell-mode-map (kbd "C-;") nil)
  (define-key ctl-x-x-map "s" flyspell-mode)) ; C-x x s

(prot-emacs-package prot-spell
  (:delay 30)
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
  (:delay 30)
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
  (setq flymake-show-diagnostics-at-end-of-line nil) ; Emacs 30

  (defvar prot/flymake-mode-projects-path
    (file-name-as-directory (expand-file-name "Projects" "~/Git/"))
    "Path to my Git projects.")

  (defun prot/flymake-mode-lexical-binding ()
    (when lexical-binding
      (flymake-mode 1)))

  (defun prot/flymake-mode-in-my-projects ()
    (when-let ((file (buffer-file-name))
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
  (:delay 30)
  (add-hook 'flymake-diagnostic-functions #'package-lint-flymake))

;;; General configurations for prose/writing
(prot-emacs-configure
;;;; `outline' (`outline-mode' and `outline-minor-mode')
  (:delay 10)
  (setq outline-minor-mode-highlight nil) ; emacs28
  (setq outline-minor-mode-cycle t) ; emacs28
  (setq outline-minor-mode-use-buttons nil) ; emacs29---bless you for the nil option!
  (setq outline-minor-mode-use-margins nil) ; as above
  (define-key global-map (kbd "<f10>") #'outline-minor-mode)

;;;; `dictionary'
  (setq dictionary-server "dict.org"
        dictionary-default-popup-strategy "lev" ; read doc string
        dictionary-create-buttons nil
        dictionary-use-single-buffer t)
  (define-key global-map (kbd "C-c d") #'dictionary-search))

;;; aLtCaPs
;; Read the manual: <https://protesilaos.com/emacs/altcaps>.
(prot-emacs-package altcaps
  (:install t)
  (:delay 60)
  ;; Force letter casing for certain characters (for legibility).
  (setq altcaps-force-character-casing
        '(;; Greek theta
          (?θ . downcase)))

  ;; The available commands: `altcaps-word', `altcaps-region',
  ;; `altcaps-dwim'.
  (define-key global-map (kbd "C-x C-a") #'altcaps-dwim))

;;; Denote (simple note-taking and file-naming)
;; Read the manual: <https://protesilaos.com/emacs/denote>.
(prot-emacs-package denote
  (:install t)
  (:delay 5)
  ;; Remember to check the doc strings of those variables.
  (setq denote-directory (expand-file-name "~/Documents/notes/"))
  (setq denote-file-type 'text) ; Org is the default, set others here like I do
  (setq denote-file-name-letter-casing
        '((signature . verbatim)
          (title . downcase)
          (keywords . verbatim)
          (t , verbatim)))
  ;; If you want to have a "controlled vocabulary" of keywords,
  ;; meaning that you only use a predefined set of them, then you want
  ;; `denote-infer-keywords' to be nil and `denote-known-keywords' to
  ;; have the keywords you need.
  (setq denote-known-keywords '("emacs" "philosophy" "politics" "economics"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-excluded-directories-regexp nil)
  (setq denote-date-format nil) ; read its doc string
  (setq denote-rename-no-confirm t)
  (setq denote-backlinks-show-context nil)
  (setq denote-rename-buffer-format "[D] %t")

  ;; Automatically rename Denote buffers when opening them so that
  ;; instead of their long file name they have a literal "[D]"
  ;; followed by the file's title.  Read the doc string of
  ;; `denote-rename-buffer-format' for how to modify this.
  (denote-rename-buffer-mode 1)

  ;; If you use Markdown or plain text files you want to buttonise
  ;; existing buttons upon visiting the file (Org renders links as
  ;; buttons right away).
  (add-hook 'find-file-hook #'denote-link-buttonize-buffer)

  ;; Highlight Denote file names in Dired buffers.  Below is the
  ;; generic approach, which is great if you rename files Denote-style
  ;; in lots of places as I do:
  (add-hook 'dired-mode-hook #'denote-dired-mode)
  ;;
  ;; OR if you only want the `denote-dired-mode' in select
  ;; directories, then modify the variable `denote-dired-directories'
  ;; and use the following instead:
  ;;
  ;;  (add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)

  (require 'denote-journal-extras)
  (setq denote-journal-extras-directory nil) ; use the `denote-directory'
  (setq denote-journal-extras-title-format nil) ; always prompt for title
  (setq denote-journal-extras-keyword "journal")

  ;; Denote DOES NOT define any key bindings.  This is for the user to
  ;; decide.  For example:
  (prot-emacs-keybind global-map
    "C-c n n" #'denote
    "C-c n N" #'denote-type
    "C-c n d" #'denote-date
    "C-c n z" #'denote-signature ; "zettelkasten" mnemonic
    "C-c n s" #'denote-subdirectory
    "C-c n o" #'denote-sort-dired ; "order" mnemonic
    "C-c n j" #'denote-journal-extras-new-entry
    "C-c n J" #'denote-journal-extras-new-or-existing-entry
    ;; Note that `denote-rename-file' can work from any context, not
    ;; just Dired buffers.  That is why we bind it here to the
    ;; `global-map'.
    ;;
    ;; Also see `denote-rename-file-using-front-matter' further below.
    "C-c n r" #'denote-rename-file)

  ;; If you intend to use Denote with a variety of file types, it is
  ;; easier to bind the link-related commands to the `global-map', as
  ;; shown here.  Otherwise follow the same pattern for
  ;; `org-mode-map', `markdown-mode-map', and/or `text-mode-map'.
  (prot-emacs-keybind text-mode-map
    "C-c n i" #'denote-link ; "insert" mnemonic
    "C-c n I" #'denote-add-links
    "C-c n b" #'denote-backlinks
    "C-c n f f" #'denote-find-link
    "C-c n f b" #'denote-find-backlink
    ;; Also see `denote-rename-file' further above.
    "C-c n R" #'denote-rename-file-using-front-matter)

  ;; I do not bind the Org dynamic blocks, but they are useful:
  ;;
  ;; - `denote-org-dblock-insert-links'
  ;; - `denote-org-dblock-insert-backlinks'
  ;; - `denote-org-dblock-insert-files'

  ;; Key bindings specifically for Dired.
  (prot-emacs-keybind dired-mode-map
    "C-c C-d C-i" #'denote-link-dired-marked-notes
    "C-c C-d C-r" #'denote-dired-rename-marked-files
    "C-c C-d C-k" #'denote-dired-rename-marked-files-with-keywords
    "C-c C-d C-f" #'denote-dired-rename-marked-files-using-front-matter)

  (with-eval-after-load 'org-capture
    (setq denote-org-capture-specifiers "%l\n%i\n%?")
    (add-to-list 'org-capture-templates
                 '("n" "New note (with denote.el)" plain
                   (file denote-last-path)
                   #'denote-org-capture
                   :no-save t
                   :immediate-finish nil
                   :kill-buffer t
                   :jump-to-captured t))

    ;; This prompts for TITLE, KEYWORDS, and SUBDIRECTORY
    (add-to-list 'org-capture-templates
                 '("N" "New note with prompts (with denote.el)" plain
                   (file denote-last-path)
                   (function
                    (lambda ()
                      (denote-org-capture-with-prompts :title :keywords :signature)))
                   :no-save t
                   :immediate-finish nil
                   :kill-buffer t
                   :jump-to-captured t))))

;;; Custom extensions for "focus mode" (logos.el)
;; Read the manual: <https://protesilaos.com/emacs/logos>.
(prot-emacs-package olivetti
  (:install t)
  (:delay 10)
  (setq olivetti-body-width 0.7)
  (setq olivetti-minimum-body-width 80)
  (setq olivetti-recall-visual-line-mode-entry-state t))

(prot-emacs-package logos
  (:install t)
  (:delay 10)
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

  ;; I don't need to do `with-eval-after-load' for the `modus-themes' as
  ;; I always load them before other relevant potentially packages.
  (add-hook 'modus-themes-post-load-theme-hook #'logos-update-fringe-in-buffers)

  (prot-emacs-keybind global-map
    "C-x n n" #'logos-narrow-dwim
    "C-x ]" #'logos-forward-page-dwim
    "C-x [" #'logos-backward-page-dwim
    ;; I don't think I ever saw a package bind M-] or M-[...
    "M-]" #'logos-forward-page-dwim
    "M-[" #'logos-backward-page-dwim
    "<f9>" #'logos-focus-mode)

;;;; Extra tweaks
  ;; place point at the top when changing pages, but not in `prog-mode'
  (defun prot/logos--recenter-top ()
    "Use `recenter' to reposition the view at the top."
    (unless (derived-mode-p 'prog-mode)
      (recenter 1))) ; Use 0 for the absolute top

  (add-hook 'logos-page-motion-hook #'prot/logos--recenter-top))

(provide 'prot-emacs-langs)
