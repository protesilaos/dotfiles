;;; The Ef (εὖ) themes

;; The themes are customisable.  Read the manual:
;; <https://protesilaos.com/emacs/ef-themes>.
(use-package ef-themes
  :ensure t
  :demand t
  :bind
  (("<f5>" . ef-themes-rotate)
   ("C-<f5>" . ef-themes-select))
  :config
  (setq ef-themes-variable-pitch-ui t
        ef-themes-mixed-fonts t
        ef-themes-rotate ef-themes-items
        ef-themes-headings ; read the manual's entry of the doc string
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

  ;; The `ef-themes' provide lots of themes.  I want to pick one at
  ;; random when I start Emacs: the `ef-themes-load-random' does just
  ;; that (it can be called interactively as well).  I just check with
  ;; my desktop environment to determine if the choice should be about
  ;; a light or a dark theme.  Those functions are in my init.el.
  (if (prot-emacs-theme-environment-dark-p)
      (ef-themes-load-random 'dark)
    (ef-themes-load-random 'light)))

;; NOTE: For testing purposes
(prot-emacs-comment
  (:eval nil)
  (progn
    (mapc #'disable-theme custom-enabled-themes)

    (add-to-list 'load-path "/home/prot/Git/Projects/ef-themes/")

    (require 'ef-themes)
    (load-theme 'ef-arbutus t t)
    (load-theme 'ef-autumn t t)
    (load-theme 'ef-bio t t)
    (load-theme 'ef-cherie t t)
    (load-theme 'ef-cyprus t t)
    (load-theme 'ef-dark t t)
    (load-theme 'ef-day t t)
    (load-theme 'ef-deuteranopia-dark t t)
    (load-theme 'ef-deuteranopia-light t t)
    (load-theme 'ef-duo-dark t t)
    (load-theme 'ef-duo-light t t)
    (load-theme 'ef-eagle t t)
    (load-theme 'ef-frost t t)
    (load-theme 'ef-kassio t t)
    (load-theme 'ef-light t t)
    (load-theme 'ef-melissa-dark t t)
    (load-theme 'ef-melissa-light t t)
    (load-theme 'ef-night t t)
    (load-theme 'ef-owl t t)
    (load-theme 'ef-rosa t t)
    (load-theme 'ef-spring t t)
    (load-theme 'ef-summer t t)
    (load-theme 'ef-symbiosis t t)
    (load-theme 'ef-trio-dark t t)
    (load-theme 'ef-trio-light t t)
    (load-theme 'ef-tritanopia-dark t t)
    (load-theme 'ef-tritanopia-light t t)
    (load-theme 'ef-winter t t)

    (setq ef-themes-headings ; read the manual's entry or the doc string
          '((0 . (variable-pitch light 1.9))
            (1 . (variable-pitch light 1.8))
            (2 . (variable-pitch light 1.7))
            (3 . (variable-pitch semilight 1.6))
            (4 . (variable-pitch semilight 1.5))
            (5 . (variable-pitch regular 1.4))
            (6 . (variable-pitch regular 1.3))
            (7 . (variable-pitch regular 1.2))    ; absence of weight means `bold'
            (agenda-date . (semilight 1.5))
            (agenda-structure . (variable-pitch light 1.9))
            (t . (variable-pitch regular 1.1))))

    ;; They are nil by default...
    (setq ef-themes-mixed-fonts t
          ef-themes-variable-pitch-ui nil)

    (mapcar (lambda (theme)
              (add-to-list
               'custom-theme-load-path
               (concat "/home/prot/Git/Projects/ef-themes/" (symbol-name theme) "-theme.el")))
            (ef-themes--list-enabled-themes))

    (if (prot-emacs-theme-environment-dark-p)
        (ef-themes-load-random 'dark)
      (ef-themes-load-random 'light))

    (define-key global-map (kbd "<f5>") #'ef-themes-rotate)
    (define-key global-map (kbd "C-<f5>") #'ef-themes-select)))

(provide 'prot-emacs-ef-themes)
