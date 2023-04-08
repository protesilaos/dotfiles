;;; The Modus themes

;; The themes are highly customisable.  Read the manual:
;; <https://protesilaos.com/emacs/modus-themes>.
(prot-emacs-package modus-themes
  (:install t)
  (setq modus-themes-custom-auto-reload nil
        modus-themes-to-toggle '(modus-operandi modus-vivendi)
        ;; modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted)
        ;; modus-themes-to-toggle '(modus-operandi-deuteranopia modus-vivendi-deuteranopia)
        modus-themes-mixed-fonts t
        modus-themes-variable-pitch-ui nil
        modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-org-blocks nil
        modus-themes-completions '((t . (extrabold)))
        modus-themes-prompts nil
        modus-themes-headings
        '((agenda-structure . (variable-pitch light 2.2))
          (agenda-date . (variable-pitch regular 1.3))
          (t . (regular 1.15))))

  ;; (setq modus-themes-common-palette-overrides nil)

  (setq modus-themes-common-palette-overrides
        '((cursor magenta-cooler)
          ;; Make the fringe invisible.
          (fringe unspecified)
          ;; Make line numbers less intense and add a shade of cyan
          ;; for the current line number.
          (fg-line-number-inactive "gray50")
          (fg-line-number-active cyan-cooler)
          (bg-line-number-inactive unspecified)
          (bg-line-number-active unspecified)
          ;; Make the current line of `hl-line-mode' a fine shade of
          ;; gray (though also see my `lin' package).
          (bg-hl-line bg-dim)
          ;; Make the region have a cyan-green background with no
          ;; specific foreground (use foreground of underlying text).
          ;; "bg-sage" refers to Salvia officinalis, else the common
          ;; sage.
          (bg-region bg-sage)
          (fg-region unspecified)
          ;; Make matching parentheses a shade of magenta.  It
          ;; complements the region nicely.
          (bg-paren-match bg-magenta-intense)
          ;; Make email citations faint and neutral, reducing the
          ;; default four colors to two; make mail headers cyan-blue.
          (mail-cite-0 fg-dim)
          (mail-cite-1 blue-faint)
          (mail-cite-2 fg-dim)
          (mail-cite-3 blue-faint)
          (mail-part cyan-warmer)
          (mail-recipient blue-warmer)
          (mail-subject magenta-cooler)
          (mail-other cyan-warmer)
          ;; Change dates to a set of more subtle combinations.
          (date-deadline magenta-cooler)
          (date-scheduled magenta)
          (date-weekday fg-main)
          (date-event fg-dim)
          (date-now blue-faint)
          ;; Make tags (Org) less colorful and tables look the same as
          ;; the default foreground.
          (prose-done cyan-cooler)
          (prose-tag fg-dim)
          (prose-table fg-main)
          ;; Make headings less colorful (though I never use deeply
          ;; nested headings).
          (fg-heading-2 blue-faint)
          (fg-heading-3 magenta-faint)
          (fg-heading-4 blue-faint)
          (fg-heading-5 magenta-faint)
          (fg-heading-6 blue-faint)
          (fg-heading-7 magenta-faint)
          (fg-heading-8 blue-faint)
          ;; Make the active mode line a fine shade of lavender
          ;; (purple) and tone down the gray of the inactive mode
          ;; lines.
          (bg-mode-line-active bg-lavender)
          (border-mode-line-active bg-lavender)

          (bg-mode-line-inactive bg-dim)
          (border-mode-line-inactive bg-inactive)
          ;; Make the prompts a shade of magenta (rosy), to fit in
          ;; nicely with the overall blue-cyan-purple style of the
          ;; other overrides.  Add a nuanced background as well.
          (bg-prompt bg-magenta-nuanced)
          (fg-prompt magenta)
          ;; Tweak some more constructs for stylistic constistency.
          (name blue-warmer)
          (identifier magenta-faint)
          (keybind magenta-cooler)
          (accent-0 magenta-cooler)
          (accent-1 cyan-cooler)
          (accent-2 blue-warmer)
          (accent-3 red-cooler)))

  ;; Make the active mode line have a pseudo 3D effect (this assumes
  ;; you are using the default mode line and not an extra package).
  (custom-set-faces
   '(mode-line ((t :box (:style released-button)))))

  (if (prot-emacs-theme-environment-dark-p)
      (modus-themes-load-theme 'modus-vivendi)
    (modus-themes-load-theme 'modus-operandi))

  ;; Also check `modus-themes-select'.  To list the palette's colours,
  ;; use `modus-themes-list-colors', `modus-themes-list-colors-current'.
  (define-key global-map (kbd "<f5>") #'modus-themes-toggle))

;; ;; NOTE: For testing purposes
;; (progn
;;   (mapc #'disable-theme custom-enabled-themes)
;;
;;   (add-to-list 'load-path "/home/prot/Git/Projects/modus-themes/")
;;
;;   (require 'modus-themes)
;;
;;   (setq modus-themes-custom-auto-reload nil
;;         modus-themes-to-toggle '(modus-operandi modus-vivendi)
;;         ;; modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted)
;;         ;; modus-themes-to-toggle '(modus-operandi-deuteranopia modus-vivendi-deuteranopia)
;;         modus-themes-mixed-fonts t
;;         modus-themes-variable-pitch-ui nil
;;         modus-themes-italic-constructs t
;;         modus-themes-bold-constructs nil
;;         modus-themes-org-blocks nil
;;         modus-themes-completions '((t . (extrabold)))
;;         modus-themes-prompts nil
;;         modus-themes-headings
;;         '((agenda-structure . (variable-pitch light 2.2))
;;           (agenda-date . (variable-pitch regular 1.3))
;;           (t . (regular 1.15))))
;;
;;   ;; (setq modus-themes-common-palette-overrides nil)
;;
;;   (setq modus-themes-common-palette-overrides
;;         '((cursor magenta-cooler)
;;           ;; Make the fringe invisible.
;;           (fringe unspecified)
;;           ;; Make line numbers less intense and add a shade of cyan
;;           ;; for the current line number.
;;           (fg-line-number-inactive "gray50")
;;           (fg-line-number-active cyan-cooler)
;;           (bg-line-number-inactive unspecified)
;;           (bg-line-number-active unspecified)
;;           ;; Make the current line of `hl-line-mode' a fine shade of
;;           ;; gray (though also see my `lin' package).
;;           (bg-hl-line bg-dim)
;;           ;; Make the region have a cyan-green background with no
;;           ;; specific foreground (use foreground of underlying text).
;;           ;; "bg-sage" refers to Salvia officinalis, else the common
;;           ;; sage.
;;           (bg-region bg-sage)
;;           (fg-region unspecified)
;;           ;; Make matching parentheses a shade of magenta.  It
;;           ;; complements the region nicely.
;;           (bg-paren-match bg-magenta-intense)
;;           ;; Make email citations faint and neutral, reducing the
;;           ;; default four colors to two; make mail headers cyan-blue.
;;           (mail-cite-0 fg-dim)
;;           (mail-cite-1 blue-faint)
;;           (mail-cite-2 fg-dim)
;;           (mail-cite-3 blue-faint)
;;           (mail-part cyan-warmer)
;;           (mail-recipient blue-warmer)
;;           (mail-subject magenta-cooler)
;;           (mail-other cyan-warmer)
;;           ;; Change dates to a set of more subtle combinations.
;;           (date-deadline magenta-cooler)
;;           (date-scheduled green-cooler)
;;           (date-weekday fg-main)
;;           (date-event fg-dim)
;;           (date-now blue-faint)
;;           ;; Make tags (Org) less colorful and tables look the same as
;;           ;; the default foreground.
;;           (prose-done green-faint)
;;           (prose-tag fg-dim)
;;           (prose-table fg-main)
;;           ;; Make headings less colorful (though I never use deeply
;;           ;; nested headings).
;;           (fg-heading-2 blue-faint)
;;           (fg-heading-3 magenta-faint)
;;           (fg-heading-4 blue-faint)
;;           (fg-heading-5 magenta-faint)
;;           (fg-heading-6 blue-faint)
;;           (fg-heading-7 magenta-faint)
;;           (fg-heading-8 blue-faint)
;;           ;; Make the active mode line a fine shade of lavender
;;           ;; (purple) and tone down the gray of the inactive mode
;;           ;; lines.
;;           (bg-mode-line-active bg-lavender)
;;           (border-mode-line-active bg-lavender)
;;
;;           (bg-mode-line-inactive bg-dim)
;;           (border-mode-line-inactive bg-inactive)
;;           ;; Make the prompts a shade of magenta (rosy), to fit in
;;           ;; nicely with the overall blue-cyan-purple style of the
;;           ;; other overrides.  Add a nuanced background as well.
;;           (bg-prompt bg-magenta-nuanced)
;;           (fg-prompt magenta)
;;           ;; Tweak some more constructs for stylistic constistency.
;;           (name blue-warmer)
;;           (identifier magenta-faint)
;;           (keybind magenta-cooler)))
;;
;;   ;; Make the active mode line have a pseudo 3D effect (this assumes
;;   ;; you are using the default mode line and not an extra package).
;;   (custom-set-faces
;;    '(mode-line ((t :box (:style released-button)))))
;;
;;   (if (prot-emacs-theme-environment-dark-p)
;;       (modus-themes-load-theme 'modus-vivendi)
;;     (modus-themes-load-theme 'modus-operandi))
;;
;;   ;; Also check `modus-themes-select'.  To list the palette's colours,
;;   ;; use `modus-themes-list-colors', `modus-themes-list-colors-current'.
;;   (define-key global-map (kbd "<f5>") #'modus-themes-toggle))

(provide 'prot-emacs-modus-themes)
