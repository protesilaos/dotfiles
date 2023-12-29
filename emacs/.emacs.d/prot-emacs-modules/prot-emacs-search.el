;;; Isearch, occur, grep, and extras (prot-search.el)
(prot-emacs-package isearch
  (:delay 5)
  (setq search-whitespace-regexp ".*?" ; one `setq' here to make it obvious they are a bundle
        isearch-lax-whitespace t
        isearch-regexp-lax-whitespace nil)

  (setq search-highlight t)
  (setq isearch-lazy-highlight t)
  (setq lazy-highlight-initial-delay 0.5)
  (setq lazy-highlight-no-delay-length 4)

  (setq isearch-lazy-count t)
  (setq lazy-count-prefix-format "(%s/%s) ")
  (setq lazy-count-suffix-format nil)

  (setq isearch-repeat-on-direction-change t)
  (setq isearch-wrap-pause t) ; `no-ding' makes keyboard macros never quit

  (setq list-matching-lines-jump-to-current-line nil) ; do not jump to current line in `*occur*' buffers
  (add-hook 'occur-mode-hook #'hl-line-mode)
  (add-hook 'occur-mode-hook #'prot-common-truncate-lines-silently) ; from `prot-common.el'

  (define-key minibuffer-local-isearch-map (kbd "M-/") #'isearch-complete-edit)
  (define-key occur-mode-map (kbd "t") #'toggle-truncate-lines)

  (prot-emacs-keybind isearch-mode-map
    "C-g" #'isearch-cancel ; instead of `isearch-abort'
    "M-/" #'isearch-complete))

(prot-emacs-package prot-search
  (:delay 5)
  (setq prot-search-outline-regexp-alist
        '((emacs-lisp-mode . "^\\((\\|;;;+ \\)")
          (org-mode . "^\\(\\*+ +\\|#\\+[Tt][Ii][Tt][Ll][Ee]:\\)")
          (outline-mode . "^\\*+ +")
          (emacs-news-view-mode . "^\\*+ +")
          (conf-toml-mode . "^\\[")
          (markdown-mode . "^#+ +")))
  (setq prot-search-todo-keywords
        (concat "TODO\\|FIXME\\|NOTE\\|REVIEW\\|XXX\\|KLUDGE"
                "\\|HACK\\|WARN\\|WARNING\\|DEPRECATED\\|BUG"))

  (with-eval-after-load 'pulsar
    (add-hook 'prot-search-outline-hook #'pulsar-recenter-center)
    (add-hook 'prot-search-outline-hook #'pulsar-reveal-entry))

  (prot-emacs-keybind global-map
    "M-s M-%" #'prot-search-replace-markup ; see `prot-search-markup-replacements'
    "M-s M-<" #'prot-search-isearch-beginning-of-buffer
    "M-s M->" #'prot-search-isearch-end-of-buffer
    "M-s g" #'prot-search-grep
    "M-s u" #'prot-search-occur-urls
    "M-s t" #'prot-search-occur-todo-keywords
    "M-s M-t" #'prot-search-grep-todo-keywords ; With C-u it runs `prot-search-git-grep-todo-keywords'
    "M-s M-T" #'prot-search-git-grep-todo-keywords
    "M-s M-s" #'prot-search-outline
    "M-s M-o" #'prot-search-occur-outline
    "M-s M-u" #'prot-search-occur-browse-url)
  (prot-emacs-keybind isearch-mode-map
    "<up>" #'prot-search-isearch-repeat-backward
    "<down>" #'prot-search-isearch-repeat-forward
    "<backspace>" #'prot-search-isearch-abort-dwim
    "<C-return>" #'prot-search-isearch-other-end))

;;; grep and xref
(prot-emacs-configure
  (:delay 5)
  (setq reb-re-syntax 'read)

  ;; All those have been changed for Emacs 28
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read) ; for M-.
  (setq xref-show-xrefs-function #'xref-show-definitions-buffer) ; for grep and the like
  (setq xref-file-name-display 'project-relative)
  (setq grep-save-buffers nil)
  (setq grep-use-headings t) ; Emacs 30

  (let ((executable (or (executable-find "rg") "grep"))
        (rgp (string-match-p "rg" grep-program)))
    (setq grep-program executable)
    (setq grep-template
          (if rgp
              "/usr/bin/rg -nH --null -e <R> <F>"
            "/usr/bin/grep <X> <C> -nH --null -e <R> <F>"))
    (setq xref-search-program (if rgp 'ripgrep 'grep))))

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

(provide 'prot-emacs-search)
