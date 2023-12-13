;;; Mode line
(prot-emacs-package prot-modeline
  (:delay 1)
  (setq mode-line-compact nil) ; Emacs 28
  (setq mode-line-right-align-edge 'right-margin) ; Emacs 30
  (setq-default mode-line-format
                '("%e"
                  prot-modeline-kbd-macro
                  prot-modeline-narrow
                  prot-modeline-buffer-status
                  prot-modeline-input-method
                  prot-modeline-evil
                  prot-modeline-buffer-identification
                  "  "
                  prot-modeline-major-mode
                  prot-modeline-process
                  "  "
                  prot-modeline-vc-branch
                  "  "
                  prot-modeline-eglot
                  "  "
                  prot-modeline-flymake
                  "  "
                  mode-line-format-right-align ; Emacs 30
                  prot-modeline-notmuch-indicator
                  "  "
                  prot-modeline-misc-info))

  (with-eval-after-load 'spacious-padding
    (defun prot/modeline-spacious-indicators ()
      "Set box attribute to `'prot-modeline-indicator-button' if spacious-padding is enabled."
      (if (bound-and-true-p spacious-padding-mode)
          (set-face-attribute 'prot-modeline-indicator-button nil :box t)
        (set-face-attribute 'prot-modeline-indicator-button nil :box 'unspecified)))

    ;; Run it at startup and then afterwards whenever
    ;; `spacious-padding-mode' is toggled on/off.
    (prot/modeline-spacious-indicators)

    (add-hook 'spacious-padding-mode-hook #'prot/modeline-spacious-indicators)))

;;; Keycast mode
(prot-emacs-package keycast
  (:install t)
  (:delay 60)
  (setq keycast-mode-line-format "%2s%k%c%R")
  (setq keycast-mode-line-insert-after 'prot-modeline-vc-branch)
  (setq keycast-mode-line-window-predicate 'mode-line-window-selected-p)
  (setq keycast-mode-line-remove-tail-elements nil)

  (dolist (input '(self-insert-command org-self-insert-command))
    (add-to-list 'keycast-substitute-alist `(,input "." "Typing…")))

  (dolist (event '(mouse-event-p mouse-movement-p mwheel-scroll))
    (add-to-list 'keycast-substitute-alist `(,event nil))))

(provide 'prot-emacs-modeline)
