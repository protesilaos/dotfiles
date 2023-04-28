;;; Mode line
(prot-emacs-package prot-modeline
  (setq mode-line-percent-position '(-3 "%p"))
  (setq mode-line-position-column-line-format '("%l,%c")) ; Emacs 28
  (setq mode-line-compact nil)                            ; Emacs 28

  (setq-default mode-line-format
                '("%e"
                  prot-modeline-border
                  prot-modeline-kbd-macro
                  " "
                  mode-line-mule-info
                  mode-line-modified
                  mode-line-remote
                  " "
                  mode-line-buffer-identification
                  " "
                  mode-line-position
                  prot-modeline-modes
                  prot-modeline-flymake
                  " "
                  (vc-mode vc-mode)
                  " "
                  prot-modeline-align-right
                  prot-modeline-misc-info))

  (add-hook 'after-init-hook #'column-number-mode))

;;; Keycast mode
(prot-emacs-package keycast
  (:install t)
  (:delay 10)
  (setq keycast-mode-line-insert-after 'mode-line-buffer-identification)
  (setq keycast-mode-line-window-predicate 'mode-line-window-selected-p)
  (setq keycast-mode-line-remove-tail-elements nil)

  (dolist (input '(self-insert-command org-self-insert-command))
    (add-to-list 'keycast-substitute-alist `(,input "." "Typing…")))

  (dolist (event '(mouse-event-p mouse-movement-p mwheel-scroll))
    (add-to-list 'keycast-substitute-alist `(,event nil)))

  (with-eval-after-load 'prot-prefix
    (transient-append-suffix 'prot-prefix-toggle '(0 1 0)
      '("k" "keycast-mode" keycast-mode))))

(provide 'prot-emacs-modeline)
