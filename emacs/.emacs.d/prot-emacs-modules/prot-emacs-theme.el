;;; Theme setup and related

;;;; Load the desired theme module
;; These all reference my packages: `modus-themes', `ef-themes',
;; `doric-themes', `standard-themes'.
(when prot-emacs-load-theme-family
  (require
   (pcase prot-emacs-load-theme-family
     ('modus 'prot-emacs-modus-themes)
     ('ef 'prot-emacs-ef-themes)
     ('doric 'prot-emacs-doric-themes)
     ('standard 'prot-emacs-standard-themes))))

;;;; Pulsar
;; Read the pulsar manual: <https://protesilaos.com/emacs/pulsar>.
(prot-emacs-configure
  (prot-emacs-install pulsar)

  (pulsar-global-mode 1)

  (prot-emacs-hook
    (next-error-hook minibuffer-setup-hook)
    (pulsar-pulse-line-red pulsar-recenter-top pulsar-reveal-entry))

  (setq pulsar-delay 0.055)
  (setq pulsar-iterations 5)
  (setq pulsar-face 'pulsar-green)
  (setq pulsar-region-face 'pulsar-yellow)
  (setq pulsar-highlight-face 'pulsar-magenta)

  (prot-emacs-keybind global-map
    "C-x l" #'pulsar-pulse-line ; override `count-lines-page'
    "C-x L" #'pulsar-highlight-permanently-dwim)) ; or use `pulsar-highlight-temporarily-dwim'

;;;; Lin
;; Read the lin manual: <https://protesilaos.com/emacs/lin>.
(prot-emacs-configure
  (prot-emacs-install lin)
  (setq lin-face 'lin-cyan)
  (lin-global-mode 1))

;;;; Increase padding of windows/frames
;; Yet another one of my packages:
;; <https://protesilaos.com/codelog/2023-06-03-emacs-spacious-padding/>.
(when prot-display-graphic-p
  (prot-emacs-configure
    (prot-emacs-install spacious-padding)

    (spacious-padding-mode 1)

    (define-key global-map (kbd "<f8>") #'spacious-padding-mode)

    (setq spacious-padding-widths
          `( :internal-border-width 15
             :header-line-width 4
             :mode-line-width 6
             :tab-width 4
             :right-divider-width 15
             :scroll-bar-width ,(if prot-pgtk-p 12 6)
             :left-fringe-width 20
             :right-fringe-width 20))

    ;; (setq spacious-padding-subtle-mode-line nil)

    ;; Read the doc string of `spacious-padding-subtle-mode-line' as it
    ;; is very flexible.  Here we make the mode lines be a single
    ;; overline, while header lines have an underline.
    (setq spacious-padding-subtle-frame-lines
          '( :mode-line-active spacious-padding-line-active
             :mode-line-inactive spacious-padding-line-inactive
             :header-line-active spacious-padding-line-active
             :header-line-inactive spacious-padding-line-inactive))

    (when (< emacs-major-version 29)
      (setq x-underline-at-descent-line (when spacious-padding-subtle-frame-lines t)))))

;;;; Rainbow mode for colour previewing (rainbow-mode.el)
(prot-emacs-configure
  (prot-emacs-install rainbow-mode)
  (setq rainbow-ansi-colors nil)
  (setq rainbow-x-colors nil)

  (defun prot/rainbow-colorize-match (color &optional match)
    "Like `rainbow-colorize-match' but works with `hl-line-mode'."
    (let ((match (or match 0)))
      (put-text-property
       (match-beginning match) (match-end match)
       'face `((:background ,(if (> 0.5 (rainbow-x-color-luminance color))
                                 "white" "black"))
               (:foreground ,color)
               (:inverse-video t)))))

  (advice-add #'rainbow-colorize-match :override #'prot/rainbow-colorize-match)

  (defun prot/rainbow-mode-in-themes ()
    (when-let* ((file buffer-file-name)
                ((derived-mode-p 'emacs-lisp-mode))
                ((string-match-p "-theme" file)))
      (rainbow-mode 1)))

  (add-hook 'emacs-lisp-mode-hook #'prot/rainbow-mode-in-themes)

  (define-key ctl-x-x-map (kbd "c") #'rainbow-mode)) ; C-x x c

;;; Cursor appearance (cursory)
;; Read the manual: <https://protesilaos.com/emacs/cursory>.
(when prot-display-graphic-p
  (prot-emacs-configure
    (prot-emacs-install cursory)

    (require 'cursory)

    (setq cursory-presets
          '((box
             :blink-cursor-interval 1.2)
            (box-no-blink
             :inherit box
             :blink-cursor-mode -1)
            (bar
             :cursor-type (bar . 2)
             :cursor-color error ; will typically be red
             :blink-cursor-interval 0.8)
            (bar-no-other-window
             :inherit bar
             :cursor-in-non-selected-windows nil)
            (bar-no-blink
             :inherit bar
             :blink-cursor-mode -1)
            (underscore
             :cursor-color warning ; will typically be yellow
             :cursor-type (hbar . 3)

             :blink-cursor-blinks 50)
            (underscore-no-other-window
             :inherit underscore
             :cursor-in-non-selected-windows nil)
            (underscore-thick
             :inherit underscore
             :cursor-type (hbar . 8)
             :cursor-in-non-selected-windows (hbar . 3))
            (t ; the default values
             :cursor-color unspecified ; use the theme's original
             :cursor-type box
             :cursor-in-non-selected-windows hollow
             :blink-cursor-mode 1
             :blink-cursor-blinks 10
             :blink-cursor-interval 0.2
             :blink-cursor-delay 0.2)))

    (cursory-set-last-or-fallback)

    ;; Persist configurations between Emacs sessions.  Also apply the
    ;; :cursor-color again when swithcing to another theme.
    (cursory-mode 1)

    ;; We have to use the "point" mnemonic, because C-c c is often the
    ;; suggested binding for `org-capture' and is the one I use as well.
    (define-key global-map (kbd "C-c p") #'cursory-set-preset)))

;;;; Theme buffet
(prot-emacs-configure
  (prot-emacs-install theme-buffet)

  (setq theme-buffet-menu 'end-user)
  (setq theme-buffet-end-user
        '( :night     (modus-vivendi ef-dark ef-winter ef-autumn ef-night ef-duo-dark ef-symbiosis)
           :morning   (modus-operandi ef-light ef-cyprus ef-spring ef-frost ef-duo-light)
           :afternoon (modus-operandi-tinted ef-arbutus ef-day ef-kassio ef-summer ef-elea-light ef-maris-light ef-melissa-light ef-trio-light ef-reverie)
           :evening   (modus-vivendi-tinted ef-rosa ef-elea-dark ef-maris-dark ef-melissa-dark ef-trio-dark ef-dream)))

  (theme-buffet-timer-hours 1))

;;;; Fontaine (font configurations)
;; Read the manual: <https://protesilaos.com/emacs/fontaine>
(prot-emacs-configure
  (prot-emacs-install fontaine)

  (fontaine-mode 1)

  (prot-emacs-keybind global-map
    "C-c f" #'fontaine-set-preset
    "C-c F" #'fontaine-toggle-preset)

  (setq-default text-scale-remap-header-line t) ; Emacs 28

  ;; The font family is my design: <https://github.com/protesilaos/aporetic>.
  (setq fontaine-presets
        '((small
           :default-height 80)
          (regular) ; like this it uses all the fallback values and is named `regular'
          (medium
           :default-family "Aporetic Serif Mono"
           :default-height 115
           :fixed-pitch-family "Aporetic Serif Mono"
           :variable-pitch-family "Aporetic Sans")
          (large
           :default-height 150)
          (presentation
           :default-height 180)
          (jumbo
           :inherit medium
           :default-height 260)
          (t
           ;; I keep all properties for didactic purposes, but most can be
           ;; omitted.  See the fontaine manual for the technicalities:
           ;; <https://protesilaos.com/emacs/fontaine>.
           :default-family "Aporetic Sans Mono"
           :default-weight regular
           :default-slant normal
           :default-width normal
           :default-height 100

           :fixed-pitch-family "Aporetic Sans Mono"
           :fixed-pitch-weight nil
           :fixed-pitch-slant nil
           :fixed-pitch-width nil
           :fixed-pitch-height 1.0

           :fixed-pitch-serif-family nil
           :fixed-pitch-serif-weight nil
           :fixed-pitch-serif-slant nil
           :fixed-pitch-serif-width nil
           :fixed-pitch-serif-height 1.0

           :variable-pitch-family "Aporetic Serif"
           :variable-pitch-weight nil
           :variable-pitch-slant nil
           :variable-pitch-width nil
           :variable-pitch-height 1.0

           :mode-line-active-family nil
           :mode-line-active-weight nil
           :mode-line-active-slant nil
           :mode-line-active-width nil
           :mode-line-active-height 1.0

           :mode-line-inactive-family nil
           :mode-line-inactive-weight nil
           :mode-line-inactive-slant nil
           :mode-line-inactive-width nil
           :mode-line-inactive-height 1.0

           :header-line-family nil
           :header-line-weight nil
           :header-line-slant nil
           :header-line-width nil
           :header-line-height 1.0

           :line-number-family nil
           :line-number-weight nil
           :line-number-slant nil
           :line-number-width nil
           :line-number-height 1.0

           :tab-bar-family nil
           :tab-bar-weight nil
           :tab-bar-slant nil
           :tab-bar-width nil
           :tab-bar-height 1.0

           :tab-line-family nil
           :tab-line-weight nil
           :tab-line-slant nil
           :tab-line-width nil
           :tab-line-height 1.0

           :bold-family nil
           :bold-slant nil
           :bold-weight bold
           :bold-width nil
           :bold-height 1.0

           :italic-family nil
           :italic-weight nil
           :italic-slant italic
           :italic-width nil
           :italic-height 1.0

           :line-spacing nil)))

  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))

  (with-eval-after-load 'pulsar
    (add-hook 'fontaine-set-preset-hook #'pulsar-pulse-line)))

;;;; Show Font (preview fonts)
;; Read the manual: <https://protesilaos.com/emacs/show-font>
(when prot-display-graphic-p
  (prot-emacs-configure
    (prot-emacs-install show-font)

    (setq show-font-display-buffer-action-alist '(display-buffer-full-frame))

    (prot-emacs-keybind global-map
      "C-c S s" #'show-font-select-preview
      "C-c S l" #'show-font-tabulated)))

;;;;; `variable-pitch-mode' setup
(prot-emacs-configure
  (define-key ctl-x-x-map (kbd "v") #'variable-pitch-mode)

  (defun prot/enable-variable-pitch ()
    (unless (derived-mode-p 'mhtml-mode 'nxml-mode 'yaml-mode)
      (when (bound-and-true-p modus-themes-mixed-fonts)
        (variable-pitch-mode 1))))

  ;; NOTE 2022-11-20: This may not cover every case, though it works
  ;; fine in my workflow.  I am still undecided by EWW.
  (prot-emacs-hook
    (text-mode-hook notmuch-show-mode-hook elfeed-show-mode-hook)
    prot/enable-variable-pitch)
;;;;; Resize keys with global effect

  ;; Emacs 29 introduces commands that resize the font across all
  ;; buffers (including the minibuffer), which is what I want, as
  ;; opposed to doing it only in the current buffer.  The keys are the
  ;; same as the defaults.
  (prot-emacs-keybind global-map
    "C-x C-=" #'global-text-scale-adjust
    "C-x C-+" #'global-text-scale-adjust
    "C-x C-0" #'global-text-scale-adjust))

(provide 'prot-emacs-theme)
