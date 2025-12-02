;;; Icons
(when prot-display-graphic-p
  (prot-emacs-configure
    (prot-emacs-install nerd-icons)

    (prot-emacs-install nerd-icons-completion)

    (nerd-icons-completion-mode 1)

    ;; By default, icons are shown in all sorts of completion prompts.
    ;; When those have different kinds of candidates, like files and
    ;; folders, the icons are helpful.  If all the candidates have the
    ;; same icon though, I prefer not to see any icon.
    (setq nerd-icons-completion-category-icons nil)

    (prot-emacs-install nerd-icons-corfu)

    (with-eval-after-load 'corfu
      (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

    (prot-emacs-install nerd-icons-dired)

    (add-hook 'dired-mode-hook #'nerd-icons-dired-mode)

    (prot-emacs-install nerd-icons-xref)

    (with-eval-after-load 'xref
      (nerd-icons-xref-mode 1))

    (prot-emacs-install nerd-icons-grep)

    (with-eval-after-load 'grep
      (when grep-use-headings
        (nerd-icons-grep-mode 1)))))

(provide 'prot-emacs-icons)
