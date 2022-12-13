;;; The Modus themes

;; NOTE 2022-12-13: I am refactoring the themes for their version 4
;; release.  This is a work-in-progress.

(prot-emacs-vc-package 'modus-themes
  (:url "https://git.sr.ht/~protesilaos/modus-themes" :branch "version-4")
  (setopt modus-themes-mixed-fonts t
          modus-themes-variable-pitch-ui t
          modus-themes-italic-constructs t
          modus-themes-bold-constructs t)

  (if (or (prot-emacs-theme-twm-dark-p)
          (prot-emacs-theme-gesttings-dark-p))
      (modus-themes-load-theme 'modus-vivendi)
    (modus-themes-load-theme 'modus-operandi))

  (define-key global-map (kbd "<f5>") #'modus-themes-toggle))

(provide 'prot-emacs-modus-themes)
