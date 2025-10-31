;;; The Doric themes

(use-package doric-themes
  :ensure t
  :demand t
  :bind
  (("<f5>" . doric-themes-toggle)
   ("C-<f5>" . doric-themes-select)
   ("M-<f5>" . doric-themes-rotate))
  :config
  ;; These are the default values.
  (setq doric-themes-to-toggle '(doric-light doric-dark))
  (setq doric-themes-to-rotate doric-themes-collection)
  (doric-themes-load-random
   (if (prot-emacs-theme-environment-dark-p)
       'dark
     'light)))

;; For testing purposes
(prot-emacs-comment
  (:eval nil)

  (add-to-list 'load-path "~/Git/Projects/doric-themes/")

  (require 'doric-themes)

  (prot-emacs-keybind global-map
    "<f5>" #'doric-themes-toggle
    "C-<f5>" #'doric-themes-select
    "M-<f5>" #'doric-themes-rotate)

  ;; These are the default values.
  (setq doric-themes-to-toggle '(doric-light doric-dark))
  (setq doric-themes-to-rotate doric-themes-collection)
  (doric-themes-load-random
   (if (prot-emacs-theme-environment-dark-p)
       'dark
     'light)))

(provide 'prot-emacs-doric-themes)
