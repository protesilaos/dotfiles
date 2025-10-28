;;; Icons
(use-package nerd-icons
  :ensure t
  :defer t) ; will be loaded by one of the following

(use-package nerd-icons-completion
  :ensure t
  :if (display-graphic-p)
  :after marginalia
  ;; FIXME 2024-09-01: For some reason this stopped working because it
  ;; macroexpands to `marginalia-mode' instead of
  ;; `marginalia-mode-hook'.  What is more puzzling is that this does
  ;; not happen in the next :hook...
  ;; :hook (marginalia-mode . nerd-icons-completion-marginalia-setup))
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)

  ;; By default, icons are shown in all sorts of completion prompts.
  ;; When those have different kinds of candidates, like files and
  ;; folders, the icons are helpful.  If all the candidates have the
  ;; same icon though, I prefer not to see any icon.
  (setq nerd-icons-completion-category-icons nil))

(use-package nerd-icons-corfu
  :ensure t
  :if (display-graphic-p)
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
  :ensure t
  :if (display-graphic-p)
  :after nerd-icons
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-xref
  :ensure t
  :if (display-graphic-p)
  :after xref
  :config
  (nerd-icons-xref-mode 1))

(use-package nerd-icons-grep
  :ensure t
  :if (display-graphic-p)
  :after grep
  :config
  (when grep-use-headings
    (nerd-icons-grep-mode 1)))

(provide 'prot-emacs-icons)
