;;; Icons
(prot-emacs-package nerd-icons
  (:install t)
  (:delay 5))

(prot-emacs-package nerd-icons-dired
  (:install t)
  (:delay 5)
  (add-hook 'dired-mode-hook #'nerd-icons-dired-mode))

(provide 'prot-emacs-icons)
