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
(use-package pulsar
  :ensure t
  :config
  (setopt pulsar-pulse t
          pulsar-delay 0.055
          pulsar-iterations 10
          pulsar-face 'pulsar-green
          pulsar-highlight-face 'pulsar-magenta)

  (pulsar-global-mode 1)
  :hook
  ;; There are convenience functions/commands which pulse the line using
  ;; a specific colour: `pulsar-pulse-line-red' is one of them.
  ((next-error . (pulsar-pulse-line-red pulsar-recenter-top pulsar-reveal-entry))
   (minibuffer-setup . pulsar-pulse-line-red))
  :bind
  ;; pulsar does not define any key bindings.  This is just my personal
  ;; preference.  Remember to read the manual on the matter.  Evaluate:
  ;;
  ;; (info "(elisp) Key Binding Conventions")
  (("C-x l" . pulsar-pulse-line) ; override `count-lines-page'
   ("C-x L" . pulsar-highlight-dwim))) ; or use `pulsar-highlight-line'

;;;; Lin
;; Read the lin manual: <https://protesilaos.com/emacs/lin>.
(use-package lin
  :ensure t
  :hook (after-init . lin-global-mode) ; applies to all `lin-mode-hooks'
  :config
  ;; You can use this to live update the face:
  ;;
  ;; (customize-set-variable 'lin-face 'lin-green)
  ;;
  ;; Or `setopt' on Emacs 29: (setopt lin-face 'lin-yellow)
  ;;
  ;; I still prefer `setq' for consistency.
  (setq lin-face 'lin-cyan))

;;;; Increase padding of windows/frames
;; Yet another one of my packages:
;; <https://protesilaos.com/codelog/2023-06-03-emacs-spacious-padding/>.
(use-package spacious-padding
  :ensure t
  :if (display-graphic-p)
  :hook (after-init . spacious-padding-mode)
  :bind ("<f8>" . spacious-padding-mode)
  :init
  ;; These are the defaults, but I keep it here for visiibility.
  (setq spacious-padding-widths
        '( :internal-border-width 30
           :header-line-width 4
           :mode-line-width 6
           :tab-width 4
           :right-divider-width 30
           :scroll-bar-width 8
           :left-fringe-width 20
           :right-fringe-width 20))

  ;; (setq spacious-padding-subtle-mode-line
  ;;       `( :mode-line-active ,(if (or (eq prot-emacs-load-theme-family 'modus)
  ;;                                     (eq prot-emacs-load-theme-family 'standard))
  ;;                                 'default
  ;;                               'help-key-binding)
  ;;          :mode-line-inactive window-divider))

  ;; Read the doc string of `spacious-padding-subtle-mode-line' as
  ;; it is very flexible.
  (setq spacious-padding-subtle-mode-line nil))

;;;; Rainbow mode for colour previewing (rainbow-mode.el)
(use-package rainbow-mode
  :ensure t
  :init
  (setq rainbow-ansi-colors nil)
  (setq rainbow-x-colors nil)

  (defun prot/rainbow-mode-in-themes ()
    (when-let* ((file (buffer-file-name))
                ((derived-mode-p 'emacs-lisp-mode))
                ((string-match-p "-theme" file)))
      (rainbow-mode 1)))
  :bind ( :map ctl-x-x-map
          ("c" . rainbow-mode)) ; C-x x c
  :hook (emacs-lisp-mode . prot/rainbow-mode-in-themes))

;;; Cursor appearance (cursory)
;; Read the manual: <https://protesilaos.com/emacs/cursory>.
(use-package cursory
  :ensure t
  :demand t
  :if (display-graphic-p)
  :config
  (setq cursory-presets
        '((box
           :blink-cursor-interval 1.2)
          (box-no-blink
           :blink-cursor-mode -1)
          (bar
           :cursor-type (bar . 2)
           :blink-cursor-interval 0.8)
          (bar-no-other-window
           :inherit bar
           :cursor-in-non-selected-windows nil)
          (bar-no-blink
           :cursor-type (bar . 2)
           :blink-cursor-mode -1)
          (underscore
           :cursor-type (hbar . 3)
           :blink-cursor-interval 0.3
           :blink-cursor-blinks 50)
          (underscore-no-other-window
           :inherit underscore
           :cursor-in-non-selected-windows nil)
          (underscore-thick
           :cursor-type (hbar . 8)
           :blink-cursor-interval 0.3
           :blink-cursor-blinks 50
           :cursor-in-non-selected-windows (hbar . 3))
          (underscore-thick-no-blink
           :blink-cursor-mode -1
           :cursor-type (hbar . 8)
           :cursor-in-non-selected-windows (hbar . 3))
          (t ; the default values
           :cursor-type box
           :cursor-in-non-selected-windows hollow
           :blink-cursor-mode 1
           :blink-cursor-blinks 10
           :blink-cursor-interval 0.2
           :blink-cursor-delay 0.2)))

  ;; I am using the default values of `cursory-latest-state-file'.

  ;; Set last preset or fall back to desired style from `cursory-presets'.
  (cursory-set-preset (or (cursory-restore-latest-preset) 'box))

  (cursory-mode 1)
  :bind
  ;; We have to use the "point" mnemonic, because C-c c is often the
  ;; suggested binding for `org-capture' and is the one I use as well.
  ("C-c p" . cursory-set-preset))

;;;; Theme buffet
(use-package theme-buffet
  :ensure t
  :after (:any modus-themes ef-themes)
  :defer 1
  :config
  (let ((modus-themes-p (featurep 'modus-themes))
        (ef-themes-p (featurep 'ef-themes)))
    (setq theme-buffet-menu 'end-user)
    (setq theme-buffet-end-user
          (cond
           ((and modus-themes-p ef-themes-p)
            '( :night     (modus-vivendi ef-dark ef-winter ef-autumn ef-night ef-duo-dark ef-symbiosis)
               :morning   (modus-operandi ef-light ef-cyprus ef-spring ef-frost ef-duo-light)
               :afternoon (modus-operandi-tinted ef-arbutus ef-day ef-kassio ef-summer ef-elea-light ef-maris-light ef-melissa-light ef-trio-light ef-reverie)
               :evening   (modus-vivendi-tinted ef-rosa ef-elea-dark ef-maris-dark ef-melissa-dark ef-trio-dark ef-dream)))
           (ef-themes-p
            '( :night     (ef-dark ef-winter ef-autumn ef-night ef-duo-dark ef-symbiosis ef-owl)
               :morning   (ef-light ef-cyprus ef-spring ef-frost ef-duo-light ef-eagle)
               :afternoon (ef-arbutus ef-day ef-kassio ef-summer ef-elea-light ef-maris-light ef-melissa-light ef-trio-light ef-reverie)
               :evening   (ef-rosa ef-elea-dark ef-maris-dark ef-melissa-dark ef-trio-dark ef-dream)))
           (modus-themes-p
            '( :night     (modus-vivendi modus-vivendi-tinted modus-vivendi-tritanopia modus-vivendi-deuteranopia)
               :morning   (modus-operandi modus-operandi-tinted modus-operandi-tritanopia modus-operandi-deuteranopia)
               :afternoon (modus-operandi modus-operandi-tinted modus-operandi-tritanopia modus-operandi-deuteranopia)
               :evening   (modus-vivendi modus-vivendi-tinted modus-vivendi-tritanopia modus-vivendi-deuteranopia)))))

    (when (or modus-themes-p ef-themes-p)
      (theme-buffet-timer-hours 1))))

;;;; Fontaine (font configurations)
;; Read the manual: <https://protesilaos.com/emacs/fontaine>
(use-package fontaine
  :ensure t
  :if (display-graphic-p)
  :hook
  ;; Persist the latest font preset when closing/starting Emacs and
  ;; while switching between themes.
  ((after-init . fontaine-mode)
   (after-init . (lambda ()
                   ;; Set last preset or fall back to desired style from `fontaine-presets'.
                   (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular)))))
  :bind (("C-c f" . fontaine-set-preset)
         ("C-c F" . fontaine-toggle-preset))
  :config
  ;; This is defined in Emacs C code: it belongs to font settings.
  (setq x-underline-at-descent-line nil)

  ;; And this is for Emacs28.
  (setq-default text-scale-remap-header-line t)

  ;; This is the default value.  Just including it here for
  ;; completeness.
  (setq fontaine-latest-state-file (locate-user-emacs-file "fontaine-latest-state.eld"))

  (setq fontaine-presets
        '((small
           :default-height 80)
          (regular) ; like this it uses all the fallback values and is named `regular'
          (medium
           :default-weight semilight
           :default-height 115
           :bold-weight extrabold)
          (large
           :inherit medium
           :default-height 150)
          (live-stream
           :default-family "Iosevka Comfy Wide Motion"
           :default-height 150
           :default-weight medium
           :fixed-pitch-family "Iosevka Comfy Wide Motion"
           :variable-pitch-family "Iosevka Comfy Wide Duo"
           :bold-weight extrabold)
          (presentation
           :default-height 180)
          (jumbo
           :default-height 260)
          (t
           ;; I keep all properties for didactic purposes, but most can be
           ;; omitted.  See the fontaine manual for the technicalities:
           ;; <https://protesilaos.com/emacs/fontaine>.
           :default-family "Iosevka Comfy"
           :default-weight regular
           :default-slant normal
           :default-width normal
           :default-height 100

           :fixed-pitch-family "Iosevka Comfy"
           :fixed-pitch-weight nil
           :fixed-pitch-slant nil
           :fixed-pitch-width nil
           :fixed-pitch-height 1.0

           :fixed-pitch-serif-family nil
           :fixed-pitch-serif-weight nil
           :fixed-pitch-serif-slant nil
           :fixed-pitch-serif-width nil
           :fixed-pitch-serif-height 1.0

           :variable-pitch-family "Iosevka Comfy Motion Duo"
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

  (with-eval-after-load 'pulsar
    (add-hook 'fontaine-set-preset-hook #'pulsar-pulse-line)))

;;;; Show Font (preview fonts)
;; Read the manual: <https://protesilaos.com/emacs/show-font>
(use-package show-font
  :ensure t
  :commands (show-font-select-preview show-font-list)
  :config
  ;; These are the defaults, but I keep them here for easier access.
  (setq show-font-pangram 'prot)
  (setq show-font-character-sample
        "
ABCDEFGHIJKLMNOPQRSTUVWXYZ
abcdefghijklmnopqrstuvwxyz
0123456789   !@#$¢%^&*~|
`'\"‘’“”.,;:  ()[]{}—-_+=<>

()[]{}<>«»‹› 6bB8&0ODdoa 1tiIlL|\/
!ij c¢ 5$Ss 7Z2z 9gqp nmMNNMW uvvwWuuw
x×X .,·°;:¡!¿?`'‘’   ÄAÃÀ TODO
"))

;;;;; `variable-pitch-mode' setup
(use-package face-remap
  :ensure nil
  :functions prot/enable-variable-pitch
  :bind ( :map ctl-x-x-map
          ("v" . variable-pitch-mode))
  :hook ((text-mode notmuch-show-mode elfeed-show-mode) . prot/enable-variable-pitch)
  :config
  ;; NOTE 2022-11-20: This may not cover every case, though it works
  ;; fine in my workflow.  I am still undecided by EWW.
  (defun prot/enable-variable-pitch ()
    (unless (derived-mode-p 'mhtml-mode 'nxml-mode 'yaml-mode)
      (variable-pitch-mode 1)))
;;;;; Resize keys with global effect
  :bind
  ;; Emacs 29 introduces commands that resize the font across all
  ;; buffers (including the minibuffer), which is what I want, as
  ;; opposed to doing it only in the current buffer.  The keys are the
  ;; same as the defaults.
  (("C-x C-=" . global-text-scale-adjust)
   ("C-x C-+" . global-text-scale-adjust)
   ("C-x C-0" . global-text-scale-adjust)))

(provide 'prot-emacs-theme)
