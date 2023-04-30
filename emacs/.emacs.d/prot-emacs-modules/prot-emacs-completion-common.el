;;; Orderless completion style (and prot-orderless.el)
(prot-emacs-package orderless
  (:install t)
  (:delay 5)
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

(prot-emacs-package prot-orderless
  (setq orderless-style-dispatchers
        '(prot-orderless-literal
          prot-orderless-file-ext
          prot-orderless-beg-or-end)))

;;; Corfu (in-buffer completion popup)
(prot-emacs-package corfu
  (:install t)
  (:delay 5)
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
(prot-emacs-package cape
  (:install t)
  (:delay 5)
  (setq cape-dabbrev-min-length 3)
  (setq cape-symbol-wrapper
        '((org-mode ?~ ?~)
          (markdown-mode ?` ?`)
          (log-edit-mode ?' ?')
          (message-mode ?' ?')))
  (dolist (backend '( cape-symbol cape-keyword cape-file cape-history cape-dabbrev))
    (add-to-list 'completion-at-point-functions backend)))

;;; Minibuffer configurations
(prot-emacs-package minibuffer
  (setq completion-styles '(emacs22 substring orderless)) ; also see `completion-category-overrides'
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

(prot-emacs-package savehist
  (:delay 5)
  (setq savehist-file (locate-user-emacs-file "savehist"))
  (setq history-length 500)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t)
  (setq savehist-additional-variables '(register-alist kill-ring))
  (savehist-mode 1))

;;; Dabbrev (dynamic word completion)
(prot-emacs-package dabbrev
  (:delay 5)
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
(prot-emacs-package abbrev
  (:delay 5)
  (setq abbrev-file-name (locate-user-emacs-file "abbrevs"))
  (setq only-global-abbrevs nil)

  (prot-emacs-abbrev global-abbrev-table
    "meweb" "https://protesilaos.com"
    "megit" "https://git.sr.ht/~protesilaos"
    "mehub" "https://github.com/protesilaos"
    "melab" "https://gitlab.com/protesilaos")

  (prot-emacs-abbrev text-mode-abbrev-table
    "asciidoc"       "AsciiDoc"
    "auctex"         "AUCTeX"
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
    "youtube"        "YouTube")

  (with-eval-after-load 'message
    (prot-emacs-abbrev message-mode-abbrev-table
      "bestregards"  "Best regards,\nProtesilaos (or simply \"Prot\")"
      "allthebest"   "All the best,\nProtesilaos (or simply \"Prot\")"
      "abest"        "All the best,\nProt"
      "bregards"     "Best regards,\nProt"))

  (let ((map global-map))
    (define-key map (kbd "C-x a e") #'expand-abbrev) ; default, just here for visibility
    (define-key map (kbd "C-x a u") #'unexpand-abbrev))

  ;; message-mode derives from text-mode, so we don't need a separate
  ;; hook for it.
  (dolist (hook '(text-mode-hook prog-mode-hook git-commit-mode-hook))
    (add-hook hook #'abbrev-mode)))

;;; Enhanced minibuffer commands (consult.el)
(prot-emacs-package consult
  (:install t)
  (:delay 5)
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

  (prot-emacs-keybind global-map
    "M-g M-g" #'consult-goto-line
    "M-K" #'consult-keep-lines ; M-S-k is similar to M-S-5 (M-%)
    "M-F" #'consult-focus-lines ; same principle
    "M-s M-b" #'consult-buffer
    "M-s M-f" #'consult-find
    "M-s M-g" #'consult-grep
    "M-s M-h" #'consult-history
    "M-s M-i" #'consult-imenu
    "M-s M-l" #'consult-line
    "M-s M-m" #'consult-mark
    "M-s M-y" #'consult-yank-pop
    "M-s M-s" #'consult-outline)
  (define-key consult-narrow-map (kbd "?") #'consult-narrow-help)

  (with-eval-after-load 'pulsar
    ;; see my `pulsar' package: <https://protesilaos.com/emacs/pulsar>
    (setq consult-after-jump-hook nil) ; reset it to avoid conflicts with my function
    (dolist (fn '(pulsar-recenter-top pulsar-reveal-entry))
      (add-hook 'consult-after-jump-hook fn))))

;;; Extended minibuffer actions and more (embark.el and prot-embark.el)
(prot-emacs-package embark
  (:install t)
  (:delay 5)
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; (setq prefix-help-command #'describe-prefix-bindings) ; the default of the above

  (setq embark-confirm-act-all nil)
  (setq embark-mixed-indicator-both nil)
  (setq embark-mixed-indicator-delay 1.0)
  (setq embark-indicators '(embark-mixed-indicator embark-highlight-indicator))
  (setq embark-verbose-indicator-nested nil) ; I think I don't have them, but I do not want them either
  (setq embark-verbose-indicator-buffer-sections '(bindings))
  (setq embark-verbose-indicator-excluded-actions
        '(embark-cycle embark-act-all embark-collect embark-export embark-insert))

  ;; I never cycle and want to disable the key.  Normally, a nil value
  ;; disables a key binding but here that value is interpreted as the
  ;; binding for `embark-act'.  So I just add some obscure key that I
  ;; do not have.  I absolutely do not want to cycle by accident!
  (setq embark-cycle-key "<XF86Travel>")

  ;; The minimal indicator shows cycling options, but I have no use
  ;; for those.  I want it to be silent.
  (defun prot/embark-no-minimal-indicator ())
  (advice-add #'embark-minimal-indicator :override #'prot/embark-no-minimal-indicator)

  (defun prot/embark-act-no-quit ()
    "Call `embark-act' but do not quit after the action."
    (interactive)
    (let ((embark-quit-after-action nil))
      (call-interactively #'embark-act)))

  (defun prot/embark-act-quit ()
    "Call `embark-act' and quit after the action."
    (interactive)
    (let ((embark-quit-after-action t))
      (call-interactively #'embark-act)))

  (dolist (map (list global-map embark-collect-mode-map minibuffer-local-filename-completion-map))
    (define-key map (kbd "C-,") #'prot/embark-act-no-quit)
    (define-key map (kbd "C-.") #'prot/embark-act-quit))

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

  (prot-emacs-keybind embark-general-map
    "i" #'embark-insert
    "w" #'embark-copy-as-kill
    "E" #'embark-export
    "S" #'embark-collect
    "A" #'embark-act-all
    "DEL" #'delete-region)

  ;; TODO 2023-03-14: `embark-url-map' for mpv
  (prot-emacs-keybind embark-url-map
    "b" #'browse-url
    "d" #'embark-download-url
    "e" #'eww)

  (prot-emacs-keybind embark-buffer-map
    "k" #'prot-simple-kill-buffer
    "o" #'switch-to-buffer-other-window
    "r" #'embark-rename-buffer
    "e" #'ediff-buffers)

  (prot-emacs-keybind embark-file-map
    "f" #'find-file
    "j" #'embark-dired-jump
    "c" #'copy-file
    "e" #'ediff-files)

  (prot-emacs-keybind embark-identifier-map
    "h" #'display-local-help
    "." #'xref-find-definitions
    "o" #'occur)

  (prot-emacs-keybind embark-command-map
    "h" #'describe-command
    "." #'embark-find-definition)

  (prot-emacs-keybind embark-expression-map
    "e" #'pp-eval-expression
    "m" #'pp-macroexpand-expression)

  (prot-emacs-keybind embark-function-map
    "h" #'describe-function
    "." #'embark-find-definition)

  (prot-emacs-keybind embark-package-map
    "h" #'describe-package
    "i" #'package-install
    "d" #'package-delete
    "r" #'package-reinstall
    "u" #'embark-browse-package-url
    "w" #'embark-save-package-url)

  (prot-emacs-keybind embark-symbol-map
    "h" #'describe-symbol
    "." #'embark-find-definition)

  (prot-emacs-keybind embark-variable-map
    "h" #'describe-variable
    "." #'embark-find-definition)

  (prot-emacs-keybind embark-region-map
    "a" #'align-regexp
    "d" #'delete-duplicate-lines
    "f" #'flush-lines
    "i" #'epa-import-keys-region
    "r" #'repunctuate-sentences
    "s" #'sort-lines
    "u" #'untabify)

  ;; FIXME 2023-04-13: Why `embark-defun-map' has `embark-expression-map' as parent?
  (set-keymap-parent embark-defun-map embark-expression-map)

  (dolist (map '( embark-url-map embark-buffer-map embark-file-map
                  embark-identifier-map embark-command-map embark-expression-map
                  embark-function-map embark-package-map embark-symbol-map
                  embark-variable-map embark-region-map))
    (set-keymap-parent (symbol-value map) embark-general-map)))

;; Needed for correct exporting while using Embark with Consult
;; commands.
(prot-emacs-package embark-consult (:install t) (:delay 5))

;;; Template-based in-buffer completion (tempel.el)
;; NOTE 2023-01-12: Check the `templates' file that I distribute with
;; my Emacs files as part of my dotfiles:
;; <https://git.sr.ht/~protesilaos/dotfiles>.
(prot-emacs-package tempel
  (:install t)
  (:delay 5)
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

  (prot-emacs-keybind global-map
    "M-+" #'tempel-complete ; Alternative: `tempel-expand'
    "M-*" #'tempel-insert)
  (prot-emacs-keybind tempel-map
    "RET" #'tempel-done
    "C-p" #'tempel-previous
    "C-n" #'tempel-next
    "<tab>" #'tempel-next
    "<backtab>" #'tempel-previous
    "C-S-<iso-lefttab>" #'tempel-previous))

(provide 'prot-emacs-completion-common)
