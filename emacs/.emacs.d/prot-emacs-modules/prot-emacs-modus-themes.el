;;; The Modus themes

;; The themes are highly customisable.  Read the manual:
;; <https://protesilaos.com/emacs/modus-themes>.
(prot-emacs-configure
  (prot-emacs-install modus-themes)
  ;; Starting with version 5.0.0 of the `modus-themes', other packages
  ;; can be built on top to provide their own "Modus" derivatives.
  ;; For example, this is what I do with my `ef-themes' and
  ;; `standard-themes' (starting with versions 2.0.0 and 3.0.0,
  ;; respectively).
  ;;
  ;; The `modus-themes-include-derivatives-mode' makes all Modus
  ;; commands that act on a theme consider all such derivatives, if
  ;; their respective packages are available and have been loaded.
  ;;
  ;; Note that those packages can even completely take over from the
  ;; Modus themes such that, for example, `modus-themes-rotate' only
  ;; goes through the Ef themes (to this end, the Ef themes provide
  ;; the `ef-themes-take-over-modus-themes-mode' and the Standard
  ;; themes have the `standard-themes-take-over-modus-themes-mode'
  ;; equivalent).
  ;;
  ;; If you only care about the Modus themes, then (i) you do not need
  ;; to enable the `modus-themes-include-derivatives-mode' and (ii) do
  ;; not install and activate those other theme packages.
  (modus-themes-include-derivatives-mode 1)

  (prot-emacs-keybind global-map
    "<f5>" #'modus-themes-rotate
    "C-<f5>" #'modus-themes-select
    "M-<f5>" #'modus-themes-load-random)

  (setq modus-themes-mixed-fonts t
        modus-themes-variable-pitch-ui t
        modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-completions '((t . (bold)))
        modus-themes-prompts '(bold)
        modus-themes-headings
        '((agenda-structure . (variable-pitch light 2.2))
          (agenda-date . (variable-pitch regular 1.3))
          (t . (regular 1.15))))

  (setq modus-themes-common-palette-overrides nil)

  (if (prot-emacs-gnome-prefers-dark-p)
      (modus-themes-load-random 'dark)
    (modus-themes-load-random 'light)))

;; NOTE: For testing purposes
(prot-emacs-comment
  (:eval nil)

  (add-to-list 'load-path "~/Git/Projects/modus-themes/")

  (require 'modus-themes)

  (modus-themes-include-derivatives-mode 1)

  (prot-emacs-keybind global-map
    "<f5>" #'modus-themes-rotate
    "C-<f5>" #'modus-themes-select
    "M-<f5>" #'modus-themes-load-random)

  (if (prot-emacs-gnome-prefers-dark-p)
      (modus-themes-load-random 'dark)
    (modus-themes-load-random 'light)))

(provide 'prot-emacs-modus-themes)
