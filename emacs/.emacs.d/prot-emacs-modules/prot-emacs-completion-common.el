;;; Orderless completion style (and prot-orderless.el)
(prot-emacs-elpa-package 'orderless
  (setq orderless-component-separator " +")
  ;; Remember to check my `completion-styles' and the
  ;; `completion-category-overrides'.
  (setq orderless-matching-styles
        '(orderless-prefixes orderless-flex orderless-regexp))

  ;; SPC should never complete: use it for `orderless' groups.
  ;; The `?' is a regexp construct.
  (let ((map minibuffer-local-completion-map))
    (define-key map (kbd "SPC") nil)
    (define-key map (kbd "?") nil)))

(prot-emacs-builtin-package 'prot-orderless
  (setq orderless-style-dispatchers
        '(prot-orderless-literal
          prot-orderless-file-ext
          prot-orderless-beg-or-end)))

;;; Corfu (in-buffer completion popup)
(prot-emacs-elpa-package 'corfu
  (global-corfu-mode 1)

  (corfu-popupinfo-mode 1) ; shows documentation after `corfu-popupinfo-delay'

  (define-key corfu-map (kbd "<tab>") #'corfu-complete)

  ;; Adapted from Corfu's manual.
  (defun contrib/corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if MCT or Vertico is not active.
Useful for prompts such as `eval-expression' and `shell-command'."
    (unless (or (bound-and-true-p vertico--input)
                (bound-and-true-p mct--active))
      (corfu-mode 1)))

  (add-hook 'minibuffer-setup-hook #'contrib/corfu-enable-always-in-minibuffer 1))

;;; CAPE (extra completion-at-point backends)
(prot-emacs-elpa-package 'cape
  (setq cape-dabbrev-min-length 3)
  (setq cape-symbol-wrapper
        '((org-mode ?~ ?~)
          (markdown-mode ?` ?`)
          (log-edit-mode ?' ?')
          (message-mode ?' ?')))
  (dolist (backend '( cape-symbol cape-keyword cape-file cape-history cape-dabbrev))
    (add-to-list 'completion-at-point-functions backend)))

;;; Minibuffer configurations
(prot-emacs-builtin-package 'minibuffer
  (setq completion-styles '(emacs22 orderless)) ; also see `completion-category-overrides'
  (setq completion-category-defaults nil)

  ;; A list of known completion categories:
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
  ;;
  ;; From the `consult' package:
  ;;
  ;; - `consult-grep'
  ;; - `consult-isearch'
  ;; - `consult-kmacro'
  ;; - `consult-location'
  ;;
  ;; From the `embark' package:
  ;;
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
          (project-file (styles . (basic substring partial-completion orderless)))
          (imenu (styles . (emacs22 substring orderless)))
          (kill-ring (styles . (emacs22 substring orderless)))
          (consult-location (styles . (emacs22 substring orderless)))
          (eglot (styles . (emacs22 substring orderless)))))

  (setq completion-ignore-case t)
  (setq read-buffer-completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)
  (setq-default case-fold-search t)   ; For general regexp

  (setq enable-recursive-minibuffers t)
  ;; Allow Emacs to resize mini windows, otherwise this does not work:
  ;;   (setq org-use-fast-todo-selection 'expert)
  (setq resize-mini-windows t)
  (setq minibuffer-eldef-shorten-default t)

  (setq read-answer-short t) ; also check `use-short-answers' for Emacs28
  (setq echo-keystrokes 0.25)
  (setq kill-ring-max 60)               ; Keep it small

  ;; Do not allow the cursor to move inside the minibuffer prompt.  I
  ;; got this from the documentation of Daniel Mendler's Vertico
  ;; package: <https://github.com/minad/vertico>.
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))

  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Add prompt indicator to `completing-read-multiple'.  We display
  ;; [CRM<separator>], e.g., [CRM,] if the separator is a comma.  This
  ;; is copied from the README of the `vertico' package.  I made some
  ;; small tweaks to propertize the segments of the prompt.
  (defun crm-indicator (args)
    (cons (format "[`crm-separator': %s]  %s"
                  (propertize
                   (replace-regexp-in-string
                    "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                    crm-separator)
                   'face 'error)
                  (car args))
          (cdr args)))

  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  (file-name-shadow-mode 1)
  (minibuffer-depth-indicate-mode 1)
  (minibuffer-electric-default-mode 1))

(prot-emacs-builtin-package 'savehist
  (setq savehist-file (locate-user-emacs-file "savehist"))
  (setq history-length 500)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t)
  (savehist-mode 1))

;;; Completion for recent files and directories (prot-recentf.el)
(prot-emacs-builtin-package 'recentf
  (setq recentf-save-file (locate-user-emacs-file "recentf"))
  (setq recentf-max-saved-items 200)
  (setq recentf-exclude '(".gz" ".xz" ".zip" ".gpg" ".asc"
                          "/ssh:" "/sudo:"
                          "~/.local" "~/.config" "~/.cache"
                          "/etc" "/usr" "/tmp"
                          "~/.emacs.d"
                          "~/Downloads" "~/Pictures"))
  (recentf-mode 1))

(prot-emacs-builtin-package 'prot-recentf
  (add-to-list 'recentf-keep 'prot-recentf-keep-predicate)
  (let ((map global-map))
    (define-key map (kbd "C-x C-r") #'prot-recentf-recent-files-or-dirs)))

;;; Dabbrev (dynamic word completion)
(prot-emacs-builtin-package 'dabbrev
  (setq dabbrev-abbrev-char-regexp "\\sw\\|\\s_")
  (setq dabbrev-abbrev-skip-leading-regexp "[$*/=~']")
  (setq dabbrev-backward-only nil)
  (setq dabbrev-case-distinction 'case-replace)
  (setq dabbrev-case-fold-search nil)
  (setq dabbrev-case-replace 'case-replace)
  (setq dabbrev-check-other-buffers t)
  (setq dabbrev-eliminate-newlines t)
  (setq dabbrev-upcase-means-case-search t)
  (let ((map global-map))
    (define-key map (kbd "M-/") #'dabbrev-expand)
    (define-key map (kbd "C-x M-/") #'dabbrev-completion)))

;;; Abbreviations or Abbrevs
(prot-emacs-builtin-package 'abbrev
  (setq abbrev-file-name (locate-user-emacs-file "abbrevs"))
  (setq only-global-abbrevs nil)

  (let ((table global-abbrev-table))
    (define-abbrev table "meweb" "https://protesilaos.com")
    (define-abbrev table "megit" "https://git.sr.ht/~protesilaos")
    (define-abbrev table "mehub" "https://github.com/protesilaos")
    (define-abbrev table "melab" "https://gitlab.com/protesilaos"))

  (let ((table text-mode-abbrev-table))
    (define-abbrev table "asciidoc" "AsciiDoc")
    (define-abbrev table "auctex" "AUCTeX")
    (define-abbrev table "cliche" "cliché")
    (define-abbrev table "clojurescript" "ClojureScript")
    (define-abbrev table "emacsconf" "EmacsConf")
    (define-abbrev table "github" "GitHub")
    (define-abbrev table "gitlab" "GitLab")
    (define-abbrev table "javascript" "JavaScript")
    (define-abbrev table "latex" "LaTeX")
    (define-abbrev table "libreplanet" "LibrePlanet")
    (define-abbrev table "linkedin" "LinkedIn")
    (define-abbrev table "paypal" "PayPal")
    (define-abbrev table "sourcehut" "SourceHut")
    (define-abbrev table "texmacs" "TeXmacs")
    (define-abbrev table "typescript" "TypeScript")
    (define-abbrev table "visavis" "vis-à-vis")
    (define-abbrev table "youtube" "YouTube"))

  (with-eval-after-load 'message
    (let ((table message-mode-abbrev-table)
          (name "Protesilaos (or simply \"Prot\")"))
      (define-abbrev table "bestregards" (format "Best regards,\n%s" name))
      (define-abbrev table "allthebest" (format "All the best,\n%s" name))
      (define-abbrev table "abest" "All the best,\nProt")
      (define-abbrev table "bregards" "Best regards,\nProt")))

  (let ((map global-map))
    (define-key map (kbd "C-x a e") #'expand-abbrev) ; default, just here for visibility
    (define-key map (kbd "C-x a u") #'unexpand-abbrev))

  ;; message-mode derives from text-mode, so we don't need a separate
  ;; hook for it.
  (dolist (hook '(text-mode-hook prog-mode-hook git-commit-mode-hook))
    (add-hook hook #'abbrev-mode)))

;;; Enhanced minibuffer commands (consult.el)
(prot-emacs-elpa-package 'consult
  (setq consult-line-numbers-widen t)
  ;; (setq completion-in-region-function #'consult-completion-in-region)
  (setq consult-async-min-input 3)
  (setq consult-async-input-debounce 0.5)
  (setq consult-async-input-throttle 0.8)
  (setq consult-narrow-key ">")
  (setq register-preview-delay 0.8
        register-preview-function #'consult-register-format)
  (setq consult-find-args "find . -not ( -path */.git* -prune )")
  (setq consult-preview-key 'any)

  (add-to-list 'consult-mode-histories '(vc-git-log-edit-mode . log-edit-comment-ring))

  (add-hook 'completion-list-mode-hook #'consult-preview-at-point-mode)

  (require 'consult-imenu) ; the `imenu' extension is in its own file

  (let ((map global-map))
    (define-key map [remap goto-line] #'consult-goto-line)
    (define-key map (kbd "M-K") #'consult-keep-lines) ; M-S-k is similar to M-S-5 (M-%)
    (define-key map (kbd "M-F") #'consult-focus-lines) ; same principle
    (define-key map (kbd "M-s M-b") #'consult-buffer)
    (define-key map (kbd "M-s M-f") #'consult-find)
    (define-key map (kbd "M-s M-g") #'consult-grep)
    (define-key map (kbd "M-s M-h") #'consult-history)
    (define-key map (kbd "M-s M-i") #'consult-imenu)
    (define-key map (kbd "M-s M-l") #'consult-line)
    (define-key map (kbd "M-s M-m") #'consult-mark)
    (define-key map (kbd "M-s M-s") #'consult-outline))
  (define-key consult-narrow-map (kbd "?") #'consult-narrow-help)

  (with-eval-after-load 'pulsar
    ;; see my `pulsar' package: <https://protesilaos.com/emacs/pulsar>
    (setq consult-after-jump-hook nil) ; reset it to avoid conflicts with my function
    (dolist (fn '(pulsar-recenter-top pulsar-reveal-entry))
      (add-hook 'consult-after-jump-hook fn))))

;;; Extended minibuffer actions and more (embark.el and prot-embark.el)
(prot-emacs-elpa-package 'embark
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; (setq prefix-help-command #'describe-prefix-bindings) ; the default of the above
  (setq embark-quit-after-action t)
  ;; I never cycle and want to disable the key.  Normally, a nil value
  ;; disables a key binding but here that value is interpreted as the
  ;; binding for `embark-act'.
  (setq embark-cycle-key "")
  (setq embark-confirm-act-all nil)
  (setq embark-mixed-indicator-both nil)
  (setq embark-mixed-indicator-delay 1.0)
  (setq embark-indicators '(embark-mixed-indicator embark-highlight-indicator))
  (setq embark-verbose-indicator-nested nil) ; I think I don't have them, but I do not want them either
  (setq embark-verbose-indicator-buffer-sections '(bindings))
  (setq embark-verbose-indicator-excluded-actions
        '(embark-act-all embark-collect embark-export embark-insert))
  
  ;; The minimal indicator shows cycling options, but I have no use
  ;; for those.  I want it to be silent.
  (defun prot/embark-no-minimal-indicator ())
  (advice-add #'embark-minimal-indicator :override #'prot/embark-no-minimal-indicator)

  (dolist (map (list global-map embark-collect-mode-map minibuffer-local-filename-completion-map))
    (define-key map (kbd "C-,") #'embark-act))

  ;; NOTE 2023-03-15: I am working on making my Embark buffers easier
  ;; to read.  I am removing keys I do not use.  What follows is a
  ;; drastic measure.  I still need to test that it works as intended.
  (seq-do
   (lambda (cell)
     (let* ((keymap (cdr-safe cell))
	    (map (if (listp keymap) (car keymap) keymap)))
       (set map (make-sparse-keymap))))
   embark-keymap-alist)

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
     prot/embark-org-keymaps)

    ;; embark-org.el adds to the `embark-region-map', which I do not
    ;; want to.
    (define-key embark-region-map (kbd "M") #'embark-org-copy-as-markdown))
  
  (let ((map embark-general-map))
    (define-key map (kbd "i") #'embark-insert)
    (define-key map (kbd "w") #'embark-copy-as-kill)
    (define-key map (kbd "E") #'embark-export)
    (define-key map (kbd "S") #'embark-collect)
    (define-key map (kbd "A") #'embark-act-all)
    (define-key map (kbd "DEL") #'delete-region))

  ;; TODO 2023-03-14: `embark-url-map' for mpv
  (let ((map embark-url-map))
    (set-keymap-parent map embark-general-map)
    (define-key map (kbd "b") #'browse-url)
    (define-key map (kbd "d") #'embark-download-url)
    (define-key map (kbd "e") #'eww))

  (let ((map embark-buffer-map))
    (set-keymap-parent map embark-general-map)
    (define-key map (kbd "k") #'kill-buffer)
    (define-key map (kbd "r") #'embark-rename-buffer)
    (define-key map (kbd "e") #'ediff-buffers))

  (let ((map embark-file-map))
    (set-keymap-parent map embark-general-map)
    (define-key map (kbd "f") #'find-file)
    (define-key map (kbd "j") #'embark-dired-jump)
    (define-key map (kbd "e") #'ediff-files))

  (let ((map embark-identifier-map))
    (set-keymap-parent map embark-general-map)
    (define-key map (kbd "h") #'display-local-help)
    (define-key map (kbd ".") #'xref-find-definitions)
    (define-key map (kbd "o") #'occur))

  (set-keymap-parent embark-defun-map embark-expression-map)

  (let ((map embark-command-map))
    (set-keymap-parent map embark-general-map)
    (define-key map (kbd "h") #'describe-command)
    (define-key map (kbd ".") #'embark-find-definition))

  (let ((map embark-expression-map))
    (set-keymap-parent map embark-general-map)
    (define-key map (kbd "e") #'pp-eval-expression)
    (define-key map (kbd "m") #'pp-macroexpand-expression))

  (let ((map embark-function-map))
    (set-keymap-parent map embark-general-map)
    (define-key map (kbd "h") #'describe-function)
    (define-key map (kbd ".") #'embark-find-definition))

  (let ((map embark-package-map))
    (set-keymap-parent map embark-general-map)
    (define-key map (kbd "h") #'describe-package)
    (define-key map (kbd "i") #'package-install)
    (define-key map (kbd "d") #'package-delete)
    (define-key map (kbd "r") #'package-reinstall)
    (define-key map (kbd "u") #'embark-browse-package-url)
    (define-key map (kbd "w") #'embark-save-package-url))

  (let ((map embark-symbol-map))
    (set-keymap-parent map embark-general-map)
    (define-key map (kbd "h") #'describe-symbol)
    (define-key map (kbd ".") #'embark-find-definition))

  (let ((map embark-variable-map))
    (set-keymap-parent map embark-general-map)
    (define-key map (kbd "h") #'describe-variable)
    (define-key map (kbd ".") #'embark-find-definition))

  (let ((map embark-region-map))
    (set-keymap-parent map embark-general-map)
    (define-key map (kbd "a") #'align-regexp)
    (define-key map (kbd "d") #'delete-duplicate-lines)
    (define-key map (kbd "f") #'flush-lines)
    (define-key map (kbd "i") #'epa-import-keys-region)
    (define-key map (kbd "r") #'repunctuate-sentences)
    (define-key map (kbd "s") #'sort-lines)
    (define-key map (kbd "u") #'untabify)))

;; Needed for correct exporting while using Embark with Consult
;; commands.
(prot-emacs-elpa-package 'embark-consult)

;;; Template-based in-buffer completion (tempel.el)
;; NOTE 2023-01-12: Check the `templates' file that I distribute with
;; my Emacs files as part of my dotfiles:
;; <https://git.sr.ht/~protesilaos/dotfiles>.
(prot-emacs-elpa-package 'tempel

  (setq tempel-path (expand-file-name "tempel-templates" user-emacs-directory))

  ;; Setup completion at point
  (defun contrib/tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook 'contrib/tempel-setup-capf))

  (let ((map global-map))
    (define-key map (kbd "M-+") #'tempel-complete) ; Alternative: `tempel-expand'
    (define-key map (kbd "M-*") #'tempel-insert))
  (let ((map tempel-map))
    (define-key map (kbd "RET") #'tempel-done)
    (define-key map (kbd "C-p") #'tempel-previous)
    (define-key map (kbd "C-n") #'tempel-next)))

(provide 'prot-emacs-completion-common)
