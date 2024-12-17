;;; The Standard themes

;; The themes are customisable.  Read the manual:
;; <https://protesilaos.com/emacs/standard-themes>.

(use-package standard-themes
  :ensure t
  :demand t
  :bind (("<f5>" . standard-themes-toggle)
         ("M-<f5>" . standard-themes-rotate))
  :config
  (setq standard-themes-bold-constructs t
        standard-themes-italic-constructs t
        standard-themes-mixed-fonts t
        standard-themes-variable-pitch-ui t

        ;; more complex alist to set weight, height, and optional
        ;; `variable-pitch' per heading level (t is for any level not
        ;; specified)
        standard-themes-headings
        '((0 . (variable-pitch light 1.9))
          (1 . (variable-pitch light 1.8))
          (2 . (variable-pitch light 1.7))
          (3 . (variable-pitch semilight 1.6))
          (4 . (variable-pitch semilight 1.5))
          (5 . (variable-pitch 1.4))
          (6 . (variable-pitch 1.3))
          (7 . (variable-pitch 1.2))
          (agenda-date . (1.3))
          (agenda-structure . (variable-pitch light 1.8))
          (t . (variable-pitch 1.1))))

  ;; Load a theme that is consistent with my session's theme.  Those
  ;; functions are defined in my init.el.
  (standard-themes-load-theme
   (if (prot-emacs-theme-environment-dark-p)
       'standard-dark
     'standard-light)))

;; NOTE: For testing purposes
(prot-emacs-comment
  (:eval nil)
  (progn
    (mapc #'disable-theme custom-enabled-themes)

    (add-to-list 'load-path "/home/prot/Git/Projects/standard-themes/")

    (require 'standard-themes)
    (load-theme 'standard-dark t t)
    (load-theme 'standard-light t t)
    (load-theme 'standard-dark-tinted t t)
    (load-theme 'standard-light-tinted t t)

    (setq standard-themes-bold-constructs t
          standard-themes-italic-constructs t
          standard-themes-disable-other-themes t
          standard-themes-mixed-fonts t
          standard-themes-variable-pitch-ui t

          ;; more complex alist to set weight, height, and optional
          ;; `variable-pitch' per heading level (t is for any level not
          ;; specified)
          standard-themes-headings
          '((0 . (variable-pitch light 1.9))
            (1 . (variable-pitch light 1.8))
            (2 . (variable-pitch light 1.7))
            (3 . (variable-pitch semilight 1.6))
            (4 . (variable-pitch semilight 1.5))
            (5 . (variable-pitch 1.4))
            (6 . (variable-pitch 1.3))
            (7 . (variable-pitch 1.2))
            (agenda-date . (1.3))
            (agenda-structure . (variable-pitch light 1.8))
            (t . (variable-pitch 1.1))))

    (mapcar (lambda (theme)
              (add-to-list
               'custom-theme-load-path
               (concat "/home/prot/Git/Projects/standard-themes/" (symbol-name theme) "-theme.el")))
            (standard-themes--list-enabled-themes))

    (standard-themes-load-theme
     (if (prot-emacs-theme-environment-dark-p)
         'standard-dark
       'standard-light))

    (define-key global-map (kbd "<f5>") #'standard-themes-toggle)
    (define-key global-map (kbd "M-<f5>") #'standard-themes-rotate)))

(provide 'prot-emacs-standard-themes)
