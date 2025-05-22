;;; The Doric themes

(use-package doric-themes
  :ensure nil
  :load-path "~/Git/Projects/doric-themes/" ; I will update this when the package is on GNU ELPA
  :demand t
  :config
  ;; These are the default values.
  (setq doric-themes-to-toggle '(doric-light doric-dark))
  (setq doric-themes-to-rotate doric-themes-collection)

  (with-eval-after-load 'lin
    (setq lin-face 'hl-line)
    (lin-global-mode -1)
    (global-hl-line-mode 1))

  (global-display-line-numbers-mode 1)

  (doric-themes-load-random
   (if (prot-emacs-theme-environment-dark-p)
       'dark
     'light))

  :bind
  (("<f5>" . doric-themes-toggle)
   ("C-<f5>" . doric-themes-select)
   ("M-<f5>" . doric-themes-rotate)))

(provide 'prot-emacs-doric-themes)
