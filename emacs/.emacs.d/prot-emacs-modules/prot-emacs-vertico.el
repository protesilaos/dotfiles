;;; Vertical completion layout (vertico)
(prot-emacs-configure
 (prot-emacs-install vertico)

 (setq vertico-scroll-margin 0)
 (setq vertico-count 5)
 (setq vertico-resize t)
 (setq vertico-cycle t)

 (with-eval-after-load 'rfn-eshadow
   ;; This works with `file-name-shadow-mode' enabled.  When you are in
   ;; a sub-directory and use, say, `find-file' to go to your home '~/'
   ;; or root '/' directory, Vertico will clear the old path to keep
   ;; only your current input.
   (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))

 (vertico-mode 1))

;;; Custom tweaks for vertico (prot-vertico.el)
(prot-emacs-configure
  (require 'prot-vertico)
  (setq vertico-multiform-commands
        `(("consult-\\(.*\\)?\\(find\\|grep\\|ripgrep\\)" ,@prot-vertico-multiform-maximal)))
  (setq vertico-multiform-categories
        `(;; Maximal
          (embark-keybinding ,@prot-vertico-multiform-maximal)
          (multi-category ,@prot-vertico-multiform-maximal)
          (consult-location ,@prot-vertico-multiform-maximal)
          (imenu ,@prot-vertico-multiform-maximal)
          (theme ,@prot-vertico-multiform-maximal)
          (unicode-name ,@prot-vertico-multiform-maximal)
          ;; Minimal
          (file ,@prot-vertico-multiform-minimal
                (vertico-sort-function . vertico-sort-directories-first))
          (t ,@prot-vertico-multiform-minimal)))

  (vertico-multiform-mode 1)

  (prot-emacs-keybind vertico-map
    "<left>" #'backward-char
    "<right>" #'forward-char
    "TAB" #'prot-vertico-private-complete
    "DEL" #'vertico-directory-delete-char
    "M-DEL" #'vertico-directory-delete-word
    "M-," #'vertico-quick-insert
    "M-." #'vertico-quick-exit)
  (prot-emacs-keybind vertico-multiform-map
    "RET" #'prot-vertico-private-exit
    "<return>" #'prot-vertico-private-exit
    "C-n" #'prot-vertico-private-next
    "<down>" #'prot-vertico-private-next
    "C-p" #'prot-vertico-private-previous
    "<up>" #'prot-vertico-private-previous
    "C-l" #'vertico-multiform-vertical))

(provide 'prot-emacs-vertico)
