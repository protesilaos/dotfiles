;;; Isearch, occur, grep, and extras (prot-search.el)
(prot-emacs-builtin-package 'isearch
  (setq search-highlight t)
  (setq search-whitespace-regexp ".*?")
  (setq isearch-lax-whitespace t)
  (setq isearch-regexp-lax-whitespace nil)
  (setq isearch-lazy-highlight t)
  ;; All of the following variables were introduced in Emacs 27.1.
  (setq isearch-lazy-count t)
  (setq lazy-count-prefix-format nil)
  (setq lazy-count-suffix-format " (%s/%s)")
  (setq isearch-yank-on-move 'shift)
  (setq isearch-allow-scroll 'unlimited)
  ;; These variables are from Emacs 28
  (setq isearch-repeat-on-direction-change t)
  (setq lazy-highlight-initial-delay 0.5)
  (setq lazy-highlight-no-delay-length 3)
  (setq isearch-wrap-pause t)

  (define-key minibuffer-local-isearch-map (kbd "M-/") #'isearch-complete-edit)
  (let ((map isearch-mode-map))
    (define-key map (kbd "C-g") #'isearch-cancel) ; instead of `isearch-abort'
    (define-key map (kbd "M-/") #'isearch-complete)))

(prot-emacs-builtin-package 'replace
  (setq list-matching-lines-jump-to-current-line nil)
  (add-hook 'occur-mode-hook #'hl-line-mode)
  (add-hook 'occur-mode-hook #'prot-common-truncate-lines-silently) ; from `prot-common.el'
  (define-key occur-mode-map (kbd "t") #'toggle-truncate-lines))

(prot-emacs-builtin-package 'grep)

(prot-emacs-builtin-package 'prot-search
  (setq prot-search-outline-regexp-alist
        '((emacs-lisp-mode . "^\\((\\|;;;+ \\)")
          (org-mode . "^\\(\\*+ +\\|#\\+[Tt][Ii][Tt][Ll][Ee]:\\)")
          (conf-toml-mode . "^\\[")
          (markdown-mode . "^#+ +")))
  (setq prot-search-todo-keywords
        (concat "TODO\\|FIXME\\|NOTE\\|REVIEW\\|XXX\\|KLUDGE"
                "\\|HACK\\|WARN\\|WARNING\\|DEPRECATED\\|BUG"))

  (let ((map global-map))
    (define-key map (kbd "M-s %") #'prot-search-isearch-replace-symbol)
    (define-key map (kbd "M-s M-%") #'prot-search-replace-markup) ; see `prot-search-markup-replacements'
    (define-key map (kbd "M-s M-<") #'prot-search-isearch-beginning-of-buffer)
    (define-key map (kbd "M-s M->") #'prot-search-isearch-end-of-buffer)
    (define-key map (kbd "M-s g") #'prot-search-grep)
    (define-key map (kbd "M-s u") #'prot-search-occur-urls)
    (define-key map (kbd "M-s t") #'prot-search-occur-todo-keywords)
    (define-key map (kbd "M-s M-t") #'prot-search-grep-todo-keywords) ; With C-u it runs `prot-search-git-grep-todo-keywords'
    (define-key map (kbd "M-s M-o") #'prot-search-occur-outline)
    (define-key map (kbd "M-s M-u") #'prot-search-occur-browse-url))
  (let ((map isearch-mode-map))
    (define-key map (kbd "<up>") #'prot-search-isearch-repeat-backward)
    (define-key map (kbd "<down>") #'prot-search-isearch-repeat-forward)
    (define-key map (kbd "<backspace>") #'prot-search-isearch-abort-dwim)
    (define-key map (kbd "<C-return>") #'prot-search-isearch-other-end)))

;;; Test regular expressions (re-builder)
(prot-emacs-builtin-package 're-builder
  (setq reb-re-syntax 'read))

;;; wgrep (writable grep)
(prot-emacs-elpa-package 'wgrep
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-change-readonly-file t)
  (let ((map grep-mode-map))
    (define-key map (kbd "e") #'wgrep-change-to-wgrep-mode)
    (define-key map (kbd "C-x C-q") #'wgrep-change-to-wgrep-mode)
    (define-key map (kbd "C-c C-c") #'wgrep-finish-edit)))

;;; Cross-references (xref.el)
(prot-emacs-builtin-package 'xref
  ;; All those have been changed for Emacs 28
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read) ; for M-.
  (setq xref-show-xrefs-function #'xref-show-definitions-buffer) ; for grep and the like
  (setq xref-file-name-display 'project-relative)
  (setq xref-search-program 'grep))

;;; Built-in bookmarking framework (bookmark.el and prot-bookmark.el)
(prot-emacs-builtin-package 'bookmark
  (setq bookmark-use-annotations nil)
  (setq bookmark-automatically-show-annotations t)
  (setq bookmark-set-fringe-mark t) ; Emacs28

  (add-hook 'bookmark-bmenu-mode-hook #'hl-line-mode))

(prot-emacs-builtin-package 'prot-bookmark
  (prot-bookmark-extra-keywords 1))

(provide 'prot-emacs-search)
