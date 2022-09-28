;;; The Ef (εὖ) themes

(prot-emacs-elpa-package 'ef-themes

  (setq ef-themes-variable-pitch-ui t
        ef-themes-mixed-fonts t
        ef-themes-headings ; read the manual's entry of the doc string
        '((0 . (variable-pitch light 1.9))
          (1 . (variable-pitch light 1.8))
          (2 . (variable-pitch regular 1.7))
          (3 . (variable-pitch regular 1.6))
          (4 . (variable-pitch regular 1.5))
          (5 . (variable-pitch 1.4)) ; absence of weight means `bold'
          (6 . (variable-pitch 1.3))
          (7 . (variable-pitch 1.2))
          (t . (variable-pitch 1.1))))

  ;; They are nil by default...
  (setq ef-themes-mixed-fonts t
        ef-themes-variable-pitch-ui t)
  
  ;; The `ef-themes' provide lots of themes.  I want to pick one at
  ;; random when I start Emacs: the `ef-themes-load-random' does just
  ;; that (it can be called interactively as well).  I just check with
  ;; GNOME to determine if the choice should be about a light or a dark
  ;; theme.
  (if (string-match-p
       "dark"
       (shell-command-to-string "gsettings get org.gnome.desktop.interface color-scheme"))
      (ef-themes-load-random 'dark)
    (ef-themes-load-random 'light))

  (define-key global-map (kbd "<f5>") #'ef-themes-select))

(provide 'prot-emacs-ef-themes)
