;;; Mode line
(prot-emacs-configure
  (require 'prot-modeline)
  (setq mode-line-compact nil) ; Emacs 28
  (setq mode-line-right-align-edge 'right-margin) ; Emacs 30
  (setq prot-modeline-show-frame-name (alist-get 'undecorated initial-frame-alist))

  (setq-default mode-line-format
                '("%e"
                  prot-modeline-kbd-macro
                  prot-modeline-narrow
                  prot-modeline-buffer-status
                  prot-modeline-window-dedicated-status
                  prot-modeline-input-method
                  "  "
                  prot-modeline-buffer-identification
                  "  "
                  prot-modeline-major-mode
                  prot-modeline-process
                  "  "
                  prot-modeline-frame-name
                  prot-modeline-vc-branch
                  "  "
                  prot-modeline-eglot
                  "  "
                  prot-modeline-flymake
                  "  "
                  mode-line-format-right-align ; Emacs 30
                  prot-modeline-which-function-indicator
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

;;; Show the name of the current definition or heading for context (`which-function-mode')
(prot-emacs-configure
  (setq which-func-modes '(prog-mode org-mode))
  ;; NOTE 2025-10-26: I handle the indicator on my own via `prot-modeline-which-function-indicator'.
  (setq which-func-display 'mode) ; Emacs 30
  (setq which-func-unknown "")

  ;; NOTE 2025-10-24: This is an experiment.  It seems to work, but there may be downsides.
  (with-eval-after-load 'prot-modeline
    (defun prot/which-function ()
      "A more opinionated `which-function'."
      (let ((name nil))
        (cond
         ((derived-mode-p 'lisp-data-mode)
          (ignore-errors
            (when-let* ((text (save-excursion
                          (beginning-of-defun)
                          (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
                        (definition (replace-regexp-in-string "(.+?\s+\\(.*\\)" "\\1" text)))
              (setq name (if (string-prefix-p ";" definition)
                             ""
                           (prot-modeline-string-abbreviate-but-last definition 1))))))
         (t
          (when (null name)
            (setq name (add-log-current-defun)))
          ;; If Imenu is loaded, try to make an index alist with it.
          ;; If `add-log-current-defun' ran and gave nil, accept that.
          (when (and (null name)
                     (null add-log-current-defun-function))
            (when (and (null name)
                       (boundp 'imenu--index-alist)
                       (or (null imenu--index-alist)
                           ;; Update if outdated
                           (/= (buffer-chars-modified-tick) imenu-menubar-modified-tick))
                       (null which-function-imenu-failed))
              (ignore-errors (imenu--make-index-alist t))
              (unless imenu--index-alist
                (setq-local which-function-imenu-failed t)))
            ;; If we have an index alist, use it.
            (when (and (null name)
                       (boundp 'imenu--index-alist) imenu--index-alist)
              (let ((alist imenu--index-alist)
                    (minoffset (point-max))
                    offset pair mark imstack namestack)
                ;; Elements of alist are either ("name" . marker), or
                ;; ("submenu" ("name" . marker) ... ). The list can be
                ;; arbitrarily nested.
                (while (or alist imstack)
                  (if (null alist)
                      (setq alist     (car imstack)
                            namestack (cdr namestack)
                            imstack   (cdr imstack))

                    (setq pair (car-safe alist)
                          alist (cdr-safe alist))

                    (cond
                     ((atom pair))            ; Skip anything not a cons.

                     ((imenu--subalist-p pair)
                      (setq imstack   (cons alist imstack)
                            namestack (cons (car pair) namestack)
                            alist     (cdr pair)))

                     ((or (number-or-marker-p (setq mark (cdr pair)))
                          (and (overlayp mark)
                               (setq mark (overlay-start mark))))
                      (when (and (>= (setq offset (- (point) mark)) 0)
                                 (< offset minoffset)) ; Find the closest item.
                        (setq minoffset offset
                              name (if (null which-func-imenu-joiner-function)
                                       (car pair)
                                     (funcall
                                      which-func-imenu-joiner-function
                                      (reverse (cons (car pair) namestack)))))))))))))
          (prot-modeline-string-cut-end name)))))

    (advice-add #'which-function :override #'prot/which-function)

    (which-function-mode 1)))

;;; Keycast mode
(prot-emacs-configure
  (prot-emacs-install keycast)
  (with-eval-after-load 'prot-modeline
    (setq keycast-mode-line-format "%2s%k%c%R")
    (setq keycast-mode-line-insert-after 'prot-modeline-vc-branch)
    (setq keycast-mode-line-window-predicate 'mode-line-window-selected-p)
    (setq keycast-mode-line-remove-tail-elements nil))
  (with-eval-after-load 'keycast
    (dolist (input '(self-insert-command org-self-insert-command))
      (add-to-list 'keycast-substitute-alist `(,input "." "Typing…")))

    (dolist (event '("<mouse-event>" "<mouse-movement>" "<mouse-2>" "<drag-mouse-1>" "<wheel-up>" "<wheel-down>" "<double-wheel-up>" "<double-wheel-down>" "<triple-wheel-up>" "<triple-wheel-down>" "<wheel-left>" "<wheel-right>" handle-select-window mouse-set-point  mouse-drag-region))
      (add-to-list 'keycast-substitute-alist `(,event nil nil)))))

(provide 'prot-emacs-modeline)
