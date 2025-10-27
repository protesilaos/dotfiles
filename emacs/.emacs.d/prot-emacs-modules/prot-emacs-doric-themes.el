;;; The Doric themes

(use-package doric-themes
  :ensure t
  :demand t
  :config
  ;; These are the default values.
  (setq doric-themes-to-toggle '(doric-light doric-dark))
  (setq doric-themes-to-rotate doric-themes-collection)
  (doric-themes-load-random
   (if (prot-emacs-theme-environment-dark-p)
       'dark
     'light))
  :bind
  (("<f5>" . doric-themes-toggle)
   ("C-<f5>" . doric-themes-select)
   ("M-<f5>" . doric-themes-rotate)))

;; For testing purposes
(prot-emacs-comment
  (:eval nil)
  (add-to-list 'load-path "/home/prot/Git/Projects/doric-themes/")

  (require 'doric-themes)

  (setq doric-themes-to-toggle '(doric-light doric-dark))
  (setq doric-themes-to-rotate doric-themes-collection)
  (doric-themes-load-random
   (if (prot-emacs-theme-environment-dark-p)
       'dark
     'light))

  (prot-emacs-keybind global-map
    "<f5>" #'doric-themes-toggle
    "C-<f5>" #'doric-themes-select
    "M-<f5>" #'doric-themes-rotate))

(provide 'prot-emacs-doric-themes)
