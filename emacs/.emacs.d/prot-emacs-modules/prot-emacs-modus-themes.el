;;; The Modus themes

;; The themes are highly customisable.  Read the manual:
;; <https://protesilaos.com/emacs/modus-themes>.
(prot-emacs-elpa-package 'modus-themes
  (setopt modus-themes-mixed-fonts t
          modus-themes-variable-pitch-ui t
          modus-themes-headings ; read the manual's entry of the doc string
          '((0 . (variable-pitch light 1.9))
            (1 . (variable-pitch light 1.8))
            (2 . (variable-pitch regular 1.7))
            (3 . (variable-pitch regular 1.6))
            (4 . (variable-pitch regular 1.5))
            (5 . (variable-pitch 1.4)) ; absence of weight means `bold'
            (6 . (variable-pitch 1.3))
            (7 . (variable-pitch 1.2))
            (agenda-date . (semilight 1.5))
            (agenda-structure . (variable-pitch light 1.9))
            (t . (variable-pitch 1.1))))

  (if (prot-emacs-theme-environment-dark-p)
      (modus-themes-load-theme 'modus-vivendi)
    (modus-themes-load-theme 'modus-operandi))

  (define-key global-map (kbd "<f5>") #'modus-themes-toggle))

;; NOTE: For testing purposes
;; (progn
;;   (mapc #'disable-theme custom-enabled-themes)
;;
;;   (add-to-list 'load-path "/home/prot/Git/Projects/modus-themes/")
;;
;;   (require 'modus-themes)
;;
;;   (setopt modus-themes-custom-auto-reload t
;;           modus-themes-to-toggle '(modus-operandi modus-vivendi)
;;           ;; modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted)
;;           ;; modus-themes-to-toggle '(modus-operandi-deuteranopia modus-vivendi-deuteranopia)
;;           modus-themes-mixed-fonts t
;;           modus-themes-variable-pitch-ui t
;;           modus-themes-italic-constructs nil
;;           modus-themes-bold-constructs nil
;;           modus-themes-org-blocks nil
;;           modus-themes-completions '((t . (extrabold)))
;;           modus-themes-prompts '(italic extrabold)
;;           modus-themes-region nil
;;           modus-themes-headings nil)
;;
;;   (setq modus-themes-common-palette-overrides nil)
;;
;;   (if (or (prot-emacs-theme-twm-dark-p)
;;           (prot-emacs-theme-gesttings-dark-p))
;;       (modus-themes-load-theme 'modus-vivendi)
;;     (modus-themes-load-theme 'modus-operandi))
;;
;;   ;; Also check `modus-themes-select'.  To list the palette's colours,
;;   ;; use `modus-themes-list-colors', `modus-themes-list-colors-current'.
;;   (define-key global-map (kbd "<f5>") #'modus-themes-toggle))

(provide 'prot-emacs-modus-themes)
