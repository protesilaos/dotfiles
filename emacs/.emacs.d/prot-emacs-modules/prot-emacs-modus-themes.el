;;; The Modus themes

;; The themes are highly customisable.  Read the manual:
;; <https://protesilaos.com/emacs/modus-themes>.
(use-package modus-themes
  :ensure t
  :demand t
  :bind (("<f5>" . modus-themes-toggle)
         ("C-<f5>" . modus-themes-select))
  :config
  (setq modus-themes-custom-auto-reload nil
        modus-themes-to-toggle '(modus-operandi modus-vivendi)
        ;; modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted)
        ;; modus-themes-to-toggle '(modus-operandi-deuteranopia modus-vivendi-deuteranopia)
        ;; modus-themes-to-toggle '(modus-operandi-tritanopia modus-vivendi-tritanopia)
        modus-themes-mixed-fonts t
        modus-themes-variable-pitch-ui t
        modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-completions '((t . (extrabold)))
        modus-themes-prompts '(extrabold)
        modus-themes-headings
        '((agenda-structure . (variable-pitch light 2.2))
          (agenda-date . (variable-pitch regular 1.3))
          (t . (regular 1.15))))

  ;; (setq modus-themes-common-palette-overrides nil)

  (setq modus-themes-common-palette-overrides
        '((cursor blue-intense)
          (keybind green-cooler)
          (comment maroon)
          (bg-paren-match unspecified)
          (fg-paren-match magenta-intense)
          (underline-paren-match magenta-intense)))

  (if (prot-emacs-theme-environment-dark-p)
      (modus-themes-load-theme (cadr modus-themes-to-toggle))
    (modus-themes-load-theme (car modus-themes-to-toggle))))

;; NOTE: For testing purposes
(prot-emacs-comment
  (:eval nil)
  (progn
    (mapc #'disable-theme custom-enabled-themes)

    (add-to-list 'load-path "/home/prot/Git/Projects/modus-themes/")

    (require 'modus-themes)

    (setq modus-themes-custom-auto-reload nil
          modus-themes-to-toggle '(modus-operandi modus-vivendi)
          ;; modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted)
          ;; modus-themes-to-toggle '(modus-operandi-deuteranopia modus-vivendi-deuteranopia)
          ;; modus-themes-to-toggle '(modus-operandi-tritanopia modus-vivendi-tritanopia)
          modus-themes-mixed-fonts t
          modus-themes-variable-pitch-ui nil
          modus-themes-italic-constructs t
          modus-themes-bold-constructs t
          modus-themes-completions '((t . (extrabold)))
          modus-themes-prompts nil
          modus-themes-headings
          '((agenda-structure . (variable-pitch light 2.2))
            (agenda-date . (variable-pitch regular 1.3))
            (t . (regular 1.15))))

    ;; (setq modus-themes-common-palette-overrides nil)

    (setq modus-themes-common-palette-overrides
          `((fringe unspecified)
            ;; (bg-mode-line-active bg-lavender)
            ;; (border-mode-line-active unspecified)
            ;; (border-mode-line-inactive unspecified)
            (bg-line-number-active bg-hl-line)
            (bg-line-number-inactive unspecified)
            (fg-line-number-active fg-main)
            ;; ,@modus-themes-preset-overrides-warmer
            ))

    ;; ;; Make the active mode line have a pseudo 3D effect (this assumes
    ;; ;; you are using the default mode line and not an extra package).
    ;; (custom-set-faces
    ;;  '(mode-line ((t :box (:style unspecified)))))

    (if (prot-emacs-theme-environment-dark-p)
        (modus-themes-load-theme (cadr modus-themes-to-toggle))
      (modus-themes-load-theme (car modus-themes-to-toggle)))

    ;; Also check `modus-themes-select'.  To list the palette's colours,
    ;; use `modus-themes-list-colors', `modus-themes-list-colors-current'.
    (define-key global-map (kbd "<f5>") #'modus-themes-toggle)))

(provide 'prot-emacs-modus-themes)
