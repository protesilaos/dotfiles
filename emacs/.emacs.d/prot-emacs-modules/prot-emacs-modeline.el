;;; Mode line
(prot-emacs-package prot-modeline
  (:delay 1)
  (setq mode-line-compact nil) ; Emacs 28
  (setq mode-line-right-align-edge 'right-margin)
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
                  ;; prot-modeline-align-right
                  mode-line-format-right-align
                  prot-modeline-notmuch-indicator
                  "  "
                  prot-modeline-misc-info))

  (prot-modeline-subtle-mode 1)

  ;; Overrides the "two-column" gimmick that I will never use.
  (define-key global-map (kbd "<f2>") #'prot-modeline-subtle-mode))

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
