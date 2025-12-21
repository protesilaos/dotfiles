;;; Icons
;; NOTE 2025-12-21: Also see the `prot-minibuffer.el' and my
;; `completion-category-overrides' for completion-related icons.
(prot-emacs-configure
  (prot-emacs-autoload (prot-icons-dired-mode prot-icons-xref-mode) "prot-icons")

  (prot-icons-dired-mode 1)
  (prot-icons-xref-mode 1))

(provide 'prot-emacs-icons)
