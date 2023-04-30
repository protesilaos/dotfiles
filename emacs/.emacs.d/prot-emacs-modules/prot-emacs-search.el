;;; Isearch, occur, grep, and extras (prot-search.el)
(prot-emacs-package isearch
  (:delay 5)
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

(prot-emacs-package replace
  (:delay 5)
  (setq list-matching-lines-jump-to-current-line nil)
  (add-hook 'occur-mode-hook #'hl-line-mode)
  (add-hook 'occur-mode-hook #'prot-common-truncate-lines-silently) ; from `prot-common.el'
  (define-key occur-mode-map (kbd "t") #'toggle-truncate-lines))

(prot-emacs-package grep (:delay 5))

(prot-emacs-package prot-search
  (:delay 5)
  (setq prot-search-outline-regexp-alist
        '((emacs-lisp-mode . "^\\((\\|;;;+ \\)")
          (org-mode . "^\\(\\*+ +\\|#\\+[Tt][Ii][Tt][Ll][Ee]:\\)")
          (conf-toml-mode . "^\\[")
          (markdown-mode . "^#+ +")))
  (setq prot-search-todo-keywords
        (concat "TODO\\|FIXME\\|NOTE\\|REVIEW\\|XXX\\|KLUDGE"
                "\\|HACK\\|WARN\\|WARNING\\|DEPRECATED\\|BUG"))

  (prot-emacs-keybind global-map
    "M-s M-%" #'prot-search-replace-markup ; see `prot-search-markup-replacements'
    "M-s M-<" #'prot-search-isearch-beginning-of-buffer
    "M-s M->" #'prot-search-isearch-end-of-buffer
    "M-s g" #'prot-search-grep
    "M-s u" #'prot-search-occur-urls
    "M-s t" #'prot-search-occur-todo-keywords
    "M-s M-t" #'prot-search-grep-todo-keywords ; With C-u it runs `prot-search-git-grep-todo-keywords'
    "M-s M-o" #'prot-search-occur-outline
    "M-s M-u" #'prot-search-occur-browse-url)
  (prot-emacs-keybind isearch-mode-map
    "<up>" #'prot-search-isearch-repeat-backward
    "<down>" #'prot-search-isearch-repeat-forward
    "<backspace>" #'prot-search-isearch-abort-dwim
    "<C-return>" #'prot-search-isearch-other-end))

;;; Test regular expressions (re-builder)
(prot-emacs-package re-builder
  (:delay 5)
  (setq reb-re-syntax 'read))

;;; wgrep (writable grep)
(prot-emacs-package wgrep
  (:install t)
  (:delay 5)
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-change-readonly-file t)
  (prot-emacs-keybind grep-mode-map
    "e" #'wgrep-change-to-wgrep-mode
    "C-x C-q" #'wgrep-change-to-wgrep-mode
    "C-c C-c" #'wgrep-finish-edit))

;;; Cross-references (xref.el)
(prot-emacs-package xref
  (:delay 5)
  ;; All those have been changed for Emacs 28
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read) ; for M-.
  (setq xref-show-xrefs-function #'xref-show-definitions-buffer) ; for grep and the like
  (setq xref-file-name-display 'project-relative)
  (setq xref-search-program
        (cond
         ((or (executable-find "ripgrep") (executable-find "rg")) 'ripgrep)
         ((executable-find "ugrep") 'ugrep)
         (t 'grep))))

;;; Built-in bookmarking framework (bookmark.el)
(prot-emacs-package bookmark
  (:delay 5)
  (setq bookmark-use-annotations nil)
  (setq bookmark-automatically-show-annotations t)
  (setq bookmark-fringe-mark nil) ; Emacs 29 to hide bookmark fringe icon

  (add-hook 'bookmark-bmenu-mode-hook #'hl-line-mode)

  (defun prot/bookmark-save-no-prompt (&rest _)
    "Run `bookmark-save' without prompts.

The intent of this function is to be added as an :after advice to
`bookmark-set-internal'.  Concretely, this means that when
`bookmark-set-internal' is called, this function is called right
afterwards.  We set this up because there is no hook after
setting a bookmark and we want to automatically save bookmarks at
that point."
    (funcall 'bookmark-save))

  (advice-add 'bookmark-set-internal :after 'prot/bookmark-save-no-prompt))

(provide 'prot-emacs-search)
