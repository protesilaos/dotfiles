;;; Icons
;; NOTE 2025-12-21: Also see the `prot-minibuffer.el' and my
;; `completion-category-overrides' for completion-related icons.
(prot-emacs-configure
  (prot-emacs-autoload (prot-icons-dired-mode prot-icons-xref-mode) "prot-icons")

  (when (memq prot-emacs-load-theme-family '(modus ef standard))
    (defun prot/icons-set-faces ()
      (modus-themes-with-colors
        (custom-set-faces
         `(prot-icons-icon ((,c :inherit (bold fixed-pitch) :box (:line-width (1 . -1)) :inverse-video t)))
         `(prot-icons-directory ((,c :inherit bold :foreground ,accent-0)))
         `(prot-icons-gray ((,c :inherit bold :foreground ,fg-dim)))
         `(prot-icons-red ((,c :inherit prot-icons-icon :foreground ,red)))
         `(prot-icons-green ((,c :inherit prot-icons-icon :foreground ,green-warmer)))
         `(prot-icons-yellow ((,c :inherit prot-icons-icon :foreground ,yellow)))
         `(prot-icons-blue ((,c :inherit prot-icons-icon :foreground ,blue-cooler)))
         `(prot-icons-magenta ((,c :inherit prot-icons-icon :foreground ,magenta-cooler)))
         `(prot-icons-cyan ((,c :inherit prot-icons-icon :foreground ,cyan))))))

    (prot/icons-set-faces)

    (add-hook 'modus-themes-after-load-theme-hook #'prot/icons-set-faces))

  (prot-icons-dired-mode 1)
  (prot-icons-xref-mode 1))

(provide 'prot-emacs-icons)
