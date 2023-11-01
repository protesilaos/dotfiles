(prot-emacs-package which-key
  (:install t)
  (:delay 1)
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "... ")
  (setq which-key-max-display-columns 3)
  (setq which-key-idle-delay 0.5)
  (setq which-key-idle-secondary-delay 0.25)
  (setq which-key-add-column-padding 5)
  (setq which-key-max-description-length 40)

  (which-key-mode 1))
  
(provide 'prot-emacs-which-key)
