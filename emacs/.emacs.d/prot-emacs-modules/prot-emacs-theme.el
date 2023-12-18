;;; Theme setup and related

;;;; Load the desired theme module
;; These all reference my packages: `modus-themes', `ef-themes',
;; `standard-themes'.
(when prot-emacs-load-theme-family
  (require
   (pcase prot-emacs-load-theme-family
     ('ef 'prot-emacs-ef-themes)
     ('modus 'prot-emacs-modus-themes)
     ('standard 'prot-emacs-standard-themes))))

;;;; Pulsar
;; Read the pulsar manual: <https://protesilaos.com/emacs/pulsar>.
(prot-emacs-package pulsar
  (:install t)
  (:delay 1)
  (dolist (cmd '( narrow-to-page narrow-to-defun
                  narrow-to-region widen
                  logos-forward-page-dwim
                  logos-backward-page-dwim))
    (add-to-list 'pulsar-pulse-functions cmd))

  (setopt pulsar-pulse t
          pulsar-delay 0.055
          pulsar-iterations 10
          pulsar-face 'pulsar-magenta
          pulsar-highlight-face 'pulsar-cyan)

  (pulsar-global-mode 1)

  ;; There are convenience functions/commands which pulse the line using
  ;; a specific colour: `pulsar-pulse-line-red' is one of them.
  (add-hook 'next-error-hook #'pulsar-pulse-line-red)

  (add-hook 'minibuffer-setup-hook #'pulsar-pulse-line-red)

  ;; pulsar does not define any key bindings.  This is just my personal
  ;; preference.  Remember to read the manual on the matter.  Evaluate:
  ;;
  ;; (info "(elisp) Key Binding Conventions")
  (prot-emacs-keybind global-map
    "C-x l" #'pulsar-pulse-line ; override `count-lines-page'
    "C-x L" #'pulsar-highlight-dwim)) ; or use `pulsar-highlight-line'

;;;; Lin
;; Read the lin manual: <https://protesilaos.com/emacs/lin>.
(prot-emacs-package lin
  (:install t)
  (:delay 1)
  ;; You can use this to live update the face:
  ;;
  ;; (customize-set-variable 'lin-face 'lin-green)
  ;;
  ;; Or `setopt' on Emacs 29: (setopt lin-face 'lin-yellow)
  ;;
  ;; I still prefer `setq' for consistency.
  (setq lin-face 'lin-magenta)

  (lin-global-mode 1)) ; applies to all `lin-mode-hooks'

;;;; Increase padding of windows/frames
;; Yet another one of my packages:
;; <https://protesilaos.com/codelog/2023-06-03-emacs-spacious-padding/>.
(prot-emacs-package spacious-padding
  (:install t)
  (:delay 1)

  ;; These are the defaults, but I keep it here for visiibility.
  (setq spacious-padding-widths
        '( :internal-border-width 15
           :header-line-width 4
           :mode-line-width 6
           :tab-width 4
           :right-divider-width 30
           :scroll-bar-width 8))

  ;; Read the doc string of `spacious-padding-subtle-mode-line' as it
  ;; is very flexible.
  (setq spacious-padding-subtle-mode-line
        '(:mode-line-active default :mode-line-inactive vertical-border))

  (spacious-padding-mode 1)

  (define-key global-map (kbd "<f8>") #'spacious-padding-mode))

;;;; Rainbow mode for colour previewing (rainbow-mode.el)
(prot-emacs-package rainbow-mode
  (:install t)
  (:delay 10)
  (setq rainbow-ansi-colors nil)
  (setq rainbow-x-colors nil)

  (defun prot/rainbow-mode-in-themes ()
    (when-let* ((file (buffer-file-name))
                ((derived-mode-p 'emacs-lisp-mode))
                ((string-match-p "-theme" file)))
      (rainbow-mode 1)))

  (add-hook 'emacs-lisp-mode-hook #'prot/rainbow-mode-in-themes)

  (define-key ctl-x-x-map "c" #'rainbow-mode)) ; C-x x c

;;;; Theme buffet
;; A package by Bruno Boal, that I am co-developing.  It cycles
;; through themes periodically.  Check its documentation for the
;; technicalities: <https://git.sr.ht/~bboal/theme-buffet>.
(prot-emacs-package theme-buffet
  (:install t)
  (:delay 1)
  (setq theme-buffet-menu 'end-user)
  (setq theme-buffet--end-user
        '( :night     (modus-vivendi ef-dark ef-winter ef-autumn ef-night ef-duo-dark ef-symbiosis)
           :morning   (modus-operandi ef-light ef-cyprus ef-spring ef-frost ef-duo-light)
           :afternoon (modu-operandi-tinted ef-day ef-kassio ef-summer ef-elea-light ef-maris-light ef-melissa-light ef-trio-light)
           :evening   (modus-vivendi-tinted ef-elea-dark ef-maris-dark ef-melissa-dark ef-trio-dark)))

  (theme-buffet-timer-hours 1))

;;;; Fontaine (font configurations)
;; Read the manual: <https://protesilaos.com/emacs/fontaine>
(prot-emacs-package fontaine
  (:install t)
  (:delay 1)
  ;; This is defined in Emacs C code: it belongs to font settings.
  (setq x-underline-at-descent-line nil)

  ;; And this is for Emacs28.
  (setq-default text-scale-remap-header-line t)

  ;; This is the default value.  Just including it here for
  ;; completeness.
  (setq fontaine-latest-state-file (locate-user-emacs-file "fontaine-latest-state.eld"))

  ;; Iosevka Comfy is my highly customised build of Iosevka with
  ;; monospaced and duospaced (quasi-proportional) variants as well as
  ;; support or no support for ligatures:
  ;; <https://git.sr.ht/~protesilaos/iosevka-comfy>.
  ;;
  ;; | Family                          | Shapes | Spacing | Style      | Ligatures |
  ;; |---------------------------------+--------+---------+------------+-----------|
  ;; | Iosevka Comfy                   | Sans   | Compact | Monospaced | Yes       |
  ;; | Iosevka Comfy Fixed             | Sans   | Compact | Monospaced | No        |
  ;; | Iosevka Comfy Duo               | Sans   | Compact | Duospaced  | Yes       |
  ;; |---------------------------------+--------+---------+------------+-----------|
  ;; | Iosevka Comfy Motion            | Slab   | Compact | Monospaced | Yes       |
  ;; | Iosevka Comfy Motion Fixed      | Slab   | Compact | Monospaced | No        |
  ;; | Iosevka Comfy Motion Duo        | Slab   | Compact | Duospaced  | Yes       |
  ;; |---------------------------------+--------+---------+------------+-----------|
  ;; | Iosevka Comfy Wide              | Sans   | Wide    | Monospaced | Yes       |
  ;; | Iosevka Comfy Wide Fixed        | Sans   | Wide    | Monospaced | No        |
  ;; | Iosevka Comfy Wide Duo          | Sans   | Wide    | Duospaced  | Yes       |
  ;; |---------------------------------+--------+---------+------------+-----------|
  ;; | Iosevka Comfy Wide Motion       | Slab   | Wide    | Monospaced | Yes       |
  ;; | Iosevka Comfy Wide Motion Fixed | Slab   | Wide    | Monospaced | No        |
  ;; | Iosevka Comfy Wide Motion Duo   | Slab   | Wide    | Duospaced  | Yes       |
  (setq fontaine-presets
        '((small
           :default-family "Iosevka Comfy Motion"
           :default-height 80
           :variable-pitch-family "Iosevka Comfy Duo")
          (regular) ; like this it uses all the fallback values and is named `regular'
          (medium
           :default-weight semilight
           :default-height 115
           :bold-weight extrabold)
          (large
           :inherit medium
           :default-height 150)
          (presentation
           :inherit medium
           :default-weight light
           :default-height 180)
          (t
           ;; I keep all properties for didactic purposes, but most can be
           ;; omitted.  See the fontaine manual for the technicalities:
           ;; <https://protesilaos.com/emacs/fontaine>.
           :default-family "Iosevka Comfy"
           :default-weight regular
           :default-height 100
           :fixed-pitch-family nil ; falls back to :default-family
           :fixed-pitch-weight nil ; falls back to :default-weight
           :fixed-pitch-height 1.0
           :fixed-pitch-serif-family nil ; falls back to :default-family
           :fixed-pitch-serif-weight nil ; falls back to :default-weight
           :fixed-pitch-serif-height 1.0
           :variable-pitch-family "Iosevka Comfy Motion Duo"
           :variable-pitch-weight nil
           :variable-pitch-height 1.0
           :bold-family nil ; use whatever the underlying face has
           :bold-weight bold
           :italic-family nil
           :italic-slant italic
           :line-spacing nil)))

  ;; Set last preset or fall back to desired style from `fontaine-presets'.
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))

  ;; The other side of `fontaine-restore-latest-preset'.
  (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset)

  ;; Persist font configurations while switching themes.  The
  ;; `enable-theme-functions' is from Emacs 29.
  (add-hook 'enable-theme-functions #'fontaine-apply-current-preset)

  (define-key global-map (kbd "C-c f") #'fontaine-set-preset)
  (define-key global-map (kbd "C-c F") #'fontaine-set-face-font))

;;;;; `variable-pitch-mode' setup

(prot-emacs-configure
  (:delay 5)
  (define-key ctl-x-x-map (kbd "v") #'variable-pitch-mode)

  ;; NOTE 2022-11-20: This may not cover every case, though it works
  ;; fine in my workflow.  I am still undecided by EWW.
  (defun prot/enable-variable-pitch ()
    (unless (derived-mode-p 'mhtml-mode 'nxml-mode 'yaml-mode)
      (variable-pitch-mode 1)))

  (defvar prot/enable-variable-pitch-in-hooks
    '(text-mode-hook
      notmuch-show-mode-hook
      elfeed-show-mode-hook)
    "List of hook symbols to add `prot/enable-variable-pitch' to.")

  (dolist (hook prot/enable-variable-pitch-in-hooks)
    (add-hook hook #'prot/enable-variable-pitch)))

(provide 'prot-emacs-theme)
