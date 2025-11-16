;;; The Doric themes

(prot-emacs-configure
  (prot-emacs-install doric-themes)

  (prot-emacs-keybind global-map
    "<f5>" #'doric-themes-rotate
    "C-<f5>" #'doric-themes-select)

  (doric-themes-load-random
   (if (prot-emacs-gnome-prefers-dark-p)
       'dark
     'light)))

;; For testing purposes
(prot-emacs-comment
  (:eval nil)

  (add-to-list 'load-path "~/Git/Projects/doric-themes/")

  (require 'doric-themes)

  (prot-emacs-keybind global-map
    "<f5>" #'doric-themes-rotate
    "C-<f5>" #'doric-themes-select)

  (doric-themes-load-random
   (if (prot-emacs-gnome-prefers-dark-p)
       'dark
     'light)))

(provide 'prot-emacs-doric-themes)
