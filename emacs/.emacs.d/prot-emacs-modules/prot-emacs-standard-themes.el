;;; The Standard themes

(prot-emacs-configure
  (prot-emacs-install standard-themes)

  (standard-themes-take-over-modus-themes-mode 1)

  (prot-emacs-keybind global-map
    "<f5>" #'modus-themes-rotate
    "C-<f5>" #'modus-themes-select
    "M-<f5>" #'modus-themes-load-random)

  (setq modus-themes-variable-pitch-ui t
        modus-themes-mixed-fonts t
        modus-themes-to-rotate nil ; defaults to the return value of `modus-themes-get-themes'
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

  ;; The `standard-themes' provide lots of themes.  I want to pick one at
  ;; random when I start Emacs: the `modus-themes-load-random' does just
  ;; that (it can be called interactively as well).  I just check with
  ;; my desktop environment to determine if the choice should be about
  ;; a light or a dark theme.  Those functions are in my init.el.
  (if (prot-emacs-gnome-prefers-dark-p)
      (modus-themes-load-random 'dark)
    (modus-themes-load-random 'light)))

;; For testing purposes...
(prot-emacs-comment
  (:eval nil)

  (add-to-list 'load-path "~/Git/Projects/modus-themes/")
  (add-to-list 'load-path "~/Git/Projects/standard-themes/")

  (require 'modus-themes)
  (require 'standard-themes)

  (standard-themes-take-over-modus-themes-mode 1)

  (prot-emacs-keybind global-map
    "<f5>" #'modus-themes-rotate
    "C-<f5>" #'modus-themes-select
    "M-<f5>" #'modus-themes-load-random)

  (if (prot-emacs-gnome-prefers-dark-p)
      (modus-themes-load-random 'dark)
    (modus-themes-load-random 'light)))

(provide 'prot-emacs-standard-themes)
