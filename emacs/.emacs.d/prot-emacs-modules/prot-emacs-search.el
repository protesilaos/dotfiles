;;; Isearch, occur, grep, and extras (prot-search.el)
(prot-emacs-configure
 (setq search-whitespace-regexp ".*?")
 (setq isearch-lax-whitespace t)
 (setq isearch-regexp-lax-whitespace nil))

(prot-emacs-configure
  (setq search-highlight t)
  (setq isearch-lazy-highlight t)
  (setq lazy-highlight-initial-delay 0.5)
  (setq lazy-highlight-no-delay-length 4))

(prot-emacs-configure
  (setq isearch-lazy-count t)
  (setq lazy-count-prefix-format "(%s/%s) ")
  (setq lazy-count-suffix-format nil))

(prot-emacs-configure
  (setq isearch-wrap-pause t) ; `no-ding' makes keyboard macros never quit
  (setq isearch-repeat-on-direction-change t))

(prot-emacs-configure
 (setq list-matching-lines-jump-to-current-line nil) ; do not jump to current line in `*occur*' buffers
 (prot-emacs-hook occur-mode-hook (prot-common-truncate-lines-silently hl-line-mode)))

(prot-emacs-configure
 (define-key global-map (kbd "C-.") #'isearch-forward-symbol-at-point) ; easier than M-s . // I also have `prot-simple-mark-sexp' on C-,
 (define-key minibuffer-local-isearch-map (kbd "M-/") #'isearch-complete-edit)
 (define-key occur-mode-map (kbd "t") #'toggle-truncate-lines)
 (prot-emacs-keybind isearch-mode-map
   "C-g" #'isearch-cancel ; instead of `isearch-abort'
   "M-/" #'isearch-complete))

(prot-emacs-configure
 (require 'prot-search)
 (prot-emacs-keybind global-map
   "M-s M-%" #'prot-search-replace-markup ; see `prot-search-markup-replacements'
   "M-s M-<" #'prot-search-isearch-beginning-of-buffer
   "M-s M->" #'prot-search-isearch-end-of-buffer
   "M-s g" #'prot-search-grep
   "M-s u" #'prot-search-occur-urls
   "M-s t" #'prot-search-occur-todo-keywords
   "M-s M-t" #'prot-search-grep-todo-keywords ; With C-u it runs `prot-search-git-grep-todo-keywords'
   "M-s M-T" #'prot-search-git-grep-todo-keywords
   "M-s s" #'prot-search-outline
   "M-s M-o" #'prot-search-occur-outline
   "M-s M-u" #'prot-search-occur-browse-url)
 (prot-emacs-keybind isearch-mode-map
   "<up>" #'prot-search-isearch-repeat-backward
   "<down>" #'prot-search-isearch-repeat-forward
   "<backspace>" #'prot-search-isearch-abort-dwim
   "<C-return>" #'prot-search-isearch-other-end)
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

 (prot-emacs-hook
   prot-search-outline-hook
   (pulsar-recenter-center pulsar-reveal-entry)
   nil
   pulsar))

;;; grep and xref
(prot-emacs-configure
  (setq reb-re-syntax 'read)

  (let ((ripgrep (or (executable-find "rg") (executable-find "ripgrep"))))
    ;; All those have been changed for Emacs 28
    (setq xref-show-definitions-function #'xref-show-definitions-completing-read) ; for M-.
    (setq xref-show-xrefs-function #'xref-show-definitions-buffer) ; for grep and the like
    (setq xref-file-name-display 'project-relative)
    (setq xref-search-program (if ripgrep 'ripgrep 'grep))

    (setq grep-save-buffers nil)
    (setq grep-use-headings nil) ; Emacs 30

    (setq grep-program (or ripgrep (executable-find "grep")))
    (setq grep-template
          (if ripgrep
              "/usr/bin/rg -nH --null -e <R> <F>"
            "/usr/bin/grep <X> <C> -nH --null -e <R> <F>")))

  (add-hook 'grep-mode #'prot-common-truncate-lines-silently))

;;; wgrep (writable grep)
;; See the `grep-edit-mode' for the new built-in feature.
(unless (>= emacs-major-version 31)
  (prot-emacs-configure
    (prot-emacs-install wgrep)
    (with-eval-after-load 'grep
      (prot-emacs-keybind grep-mode-map
        "e" #'wgrep-change-to-wgrep-mode
        "C-x C-q" #'wgrep-change-to-wgrep-mode
        "C-c C-c" #'wgrep-finish-edit)
      (setq wgrep-auto-save-buffer t)
      (setq wgrep-change-readonly-file t))))

(provide 'prot-emacs-search)
