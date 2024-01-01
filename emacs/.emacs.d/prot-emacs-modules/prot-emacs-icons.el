;;; Icons
(prot-emacs-package nerd-icons
  (:install t)
  (:delay 5))

(prot-emacs-package nerd-icons-completion
  (:install t)
  (:delay 5)
  (nerd-icons-completion-marginalia-setup)
  (nerd-icons-completion-mode 1))

(prot-emacs-package nerd-icons-corfu
  (:install t)
  (:delay 5)
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(prot-emacs-package nerd-icons-dired
  (:install t)
  (:delay 5)
  (add-hook 'dired-mode-hook #'nerd-icons-dired-mode))

(provide 'prot-emacs-icons)
