;;; Pulsar
;; Read the pulsar manual: <https://protesilaos.com/emacs/pulsar>.
(prot-emacs-elpa-package 'pulsar

  (dolist (cmd '( narrow-to-page narrow-to-defun
                  narrow-to-region widen
                  logos-forward-page-dwim
                  logos-backward-page-dwim))
    (add-to-list 'pulsar-pulse-functions cmd))

  (setq pulsar-pulse t)
  (setq pulsar-delay 0.055)
  (setq pulsar-iterations 10)
  (setq pulsar-face 'pulsar-blue)
  (setq pulsar-highlight-face 'pulsar-red)

  (pulsar-global-mode 1)

  ;; There are convenience functions/commands which pulse the line using
  ;; a specific colour: `pulsar-pulse-line-red' is one of them.
  (add-hook 'next-error-hook #'pulsar-pulse-line-red)

  ;; pulsar does not define any key bindings.  This is just my personal
  ;; preference.  Remember to read the manual on the matter.  Evaluate:
  ;;
  ;; (info "(elisp) Key Binding Conventions")
  (let ((map global-map))
    (define-key map (kbd "C-x l") #'pulsar-pulse-line) ; override `count-lines-page'
    (define-key map (kbd "C-x L") #'pulsar-highlight-dwim))) ; or use `pulsar-highlight-line'

;;; Lin
;; Read the lin manual: <https://protesilaos.com/emacs/lin>.
(prot-emacs-elpa-package 'lin
  ;; You can use this to live update the face:
  ;;
  ;; (customize-set-variable 'lin-face 'lin-green)
  (setq lin-face 'hl-line)
  (setq lin-mode-hooks
        '(bongo-mode-hook
          dired-mode-hook
          elfeed-search-mode-hook
          git-rebase-mode-hook
          ibuffer-mode-hook
          ilist-mode-hook
          ledger-report-mode-hook
          log-view-mode-hook
          magit-log-mode-hook
          mu4e-headers-mode
          notmuch-search-mode-hook
          notmuch-tree-mode-hook
          occur-mode-hook
          org-agenda-mode-hook
          tabulated-list-mode-hook))
  (lin-global-mode 1)) ; applies to all `lin-mode-hooks'

;;; Rainbow mode for colour previewing (rainbow-mode.el)
(prot-emacs-elpa-package 'rainbow-mode
  (setq rainbow-ansi-colors nil)
  (setq rainbow-x-colors nil))

;;; Line numbers and relevant indicators (prot-sideline.el)
(prot-emacs-builtin-package 'prot-sideline
  (require 'display-line-numbers)
  ;; Set absolute line numbers.  A value of "relative" is also useful.
  (setq display-line-numbers-type t)
  ;; Those two variables were introduced in Emacs 27.1
  (setq display-line-numbers-major-tick 0)
  (setq display-line-numbers-minor-tick 0)
  ;; Use absolute numbers in narrowed buffers
  (setq-default display-line-numbers-widen t)

  (prot-emacs-elpa-package 'diff-hl
    (setq diff-hl-draw-borders nil)
    (setq diff-hl-side 'left))

  (require 'hl-line)
  (setq hl-line-sticky-flag nil)
  (setq hl-line-overlay-priority -50) ; emacs28

  (require 'whitespace)

  (let ((map global-map))
    (define-key map (kbd "<f6>") #'prot-sideline-negative-space-toggle)
    (define-key map (kbd "<f7>") #'prot-sideline-mode)
    (define-key map (kbd "C-c z") #'delete-trailing-whitespace)))

;;; Fringe mode
(prot-emacs-builtin-package 'fringe
  (fringe-mode nil)
  (setq-default fringes-outside-margins nil)
  (setq-default indicate-buffer-boundaries nil)
  (setq-default indicate-empty-lines nil)
  (setq-default overflow-newline-into-fringe t))

;;; Cursor appearance (cursory)
;; Read the manual: <https://protesilaos.com/emacs/cursory>.
(prot-emacs-elpa-package 'cursory
  (setq cursory-presets
        '((box
           :blink-cursor-interval 0.8)
          (box-no-blink
           :blink-cursor-mode -1)
          (bar
           :cursor-type (bar . 2)
           :blink-cursor-interval 0.5)
          (underscore
           :cursor-type (hbar . 2)
           :blink-cursor-blinks 50)
          (t ; the default values
           :cursor-type box
           :cursor-in-non-selected-windows hollow
           :blink-cursor-mode 1
           :blink-cursor-blinks 10
           :blink-cursor-interval 0.2
           :blink-cursor-delay 0.2)))

  ;; I am using the default values of `cursory-latest-state-file'.

  ;; Set last preset or fall back to desired style from `cursory-presets'.
  (cursory-set-preset (or (cursory-restore-latest-preset) 'bar))

  ;; The other side of `cursory-restore-latest-preset'.
  (add-hook 'kill-emacs-hook #'cursory-store-latest-preset)

  ;; We have to use the "point" mnemonic, because C-c c is often the
  ;; suggested binding for `org-capture' and is the one I use as well.
  (define-key global-map (kbd "C-c p") #'cursory-set-preset))

(provide 'prot-emacs-theme-extras)
