;;; Icons
;; NOTE 2025-12-21: Also see the `prot-minibuffer.el' and my
;; `completion-category-overrides' for completion-related icons.
(prot-emacs-configure
  (require 'prot-icons)

  (prot-icons-dired-mode 1)
  (prot-icons-xref-mode 1)

  (when prot-emacs-load-theme-family
    (cond
     ((memq prot-emacs-load-theme-family '(modus ef standard))
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
      (add-hook 'modus-themes-after-load-theme-hook #'prot/icons-set-faces))
     ((eq prot-emacs-load-theme-family 'doric)
      (defun prot/icons-set-faces ()
        (doric-themes-with-colors
         (custom-set-faces
          `(prot-icons-icon ((t :inherit (bold fixed-pitch) :box (:line-width (1 . -1)) :inverse-video t)))
          `(prot-icons-directory ((t :inherit bold :foreground ,fg-accent)))
          `(prot-icons-gray ((t :inherit bold :foreground ,fg-shadow-subtle)))
          `(prot-icons-red ((t :inherit prot-icons-icon :foreground ,fg-red)))
          `(prot-icons-green ((t :inherit prot-icons-icon :foreground ,fg-green)))
          `(prot-icons-yellow ((t :inherit prot-icons-icon :foreground ,fg-yellow)))
          `(prot-icons-blue ((t :inherit prot-icons-icon :foreground ,fg-blue)))
          `(prot-icons-magenta ((t :inherit prot-icons-icon :foreground ,fg-magenta)))
          `(prot-icons-cyan ((t :inherit prot-icons-icon :foreground ,fg-cyan))))))
      (add-hook 'doric-themes-after-load-theme-hook #'prot/icons-set-faces)))

    (prot/icons-set-faces)))

(provide 'prot-emacs-icons)
