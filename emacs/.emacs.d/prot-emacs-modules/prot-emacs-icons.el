;;; Icons
;; NOTE 2025-12-21: Also see the `prot-minibuffer.el' and my
;; `completion-category-overrides' for completion-related icons.
(prot-emacs-configure
  (prot-emacs-autoload (prot-icons-dired-mode prot-icons-xref-mode) "prot-icons")

  (when (memq prot-emacs-load-theme-family '(modus ef standard))
    (defun prot/icons-set-faces ()
      (modus-themes-with-colors
        (custom-set-faces
         `(prot-icons-red ((,c :foreground ,red)))
         `(prot-icons-green ((,c :foreground ,green)))
         `(prot-icons-yellow ((,c :foreground ,yellow)))
         `(prot-icons-blue ((,c :foreground ,blue)))
         `(prot-icons-magenta ((,c :foreground ,magenta)))
         `(prot-icons-cyan ((,c :foreground ,cyan))))))

    (prot/icons-set-faces)

    (add-hook 'modus-themes-after-load-theme-hook #'prot/icons-set-faces))

  (prot-icons-dired-mode 1)
  (prot-icons-xref-mode 1))

(provide 'prot-emacs-icons)
