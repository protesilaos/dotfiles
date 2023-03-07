;;; Mode line
(setq mode-line-percent-position '(-3 "%p"))
(setq mode-line-position-column-line-format '(" %l,%c")) ; Emacs 28
(setq mode-line-defining-kbd-macro
      (propertize " Macro" 'face 'mode-line-emphasis))

;; Thanks to Daniel Mendler for this!  It removes the square brackets
;; that denote recursive edits in the modeline.  I do not need them
;; because I am using Daniel's `recursion-indicator':
;; <https://github.com/minad/recursion-indicator>.
(setq-default mode-line-modes
              (seq-filter (lambda (s)
                            (not (and (stringp s)
                                      (string-match-p
                                       "^\\(%\\[\\|%\\]\\)$" s))))
                          mode-line-modes))

(setq mode-line-compact nil)            ; Emacs 28

(defun prot/mode-line-current-window-p ()
  "Return non-nil if selected WINDOW modeline can show keycast."
  (and (not (minibufferp))
       (not (null mode-line-format))
       (eq (selected-window) (old-selected-window))))

(defun prot/mode-line-global-value ()
  "Return value of `global-mode-string'."
  (mapconcat
   (lambda (sym)
     (if (symbolp sym)
         (symbol-value sym)
       sym))
   global-mode-string
   nil))

(defun prot/mode-line-global-string-no-properties ()
  "Return `prot/mode-line-global-value' without face properties."
  (substring-no-properties
   (prot/mode-line-global-value)
   0 -1))

(defvar prot/mode-line-frame-identification
  '(:eval (when-let* ((fname (frame-parameter (selected-frame) 'name))
                      (bname (buffer-name))
                      ((not (string= fname bname)))
                      ((prot/mode-line-current-window-p)))
            (format " @ %s " (propertize fname
                                         'face 'italic
                                         'help-echo "Current frame name"))))
  "Mode line construct for the frame name.")
(put 'prot/mode-line-frame-identification 'risky-local-variable t)

;; NOTE 2023-02-09: The `:eval' parts are HIGHLY EXPERIMENTAL.
(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                mode-line-mule-info
                mode-line-modified
                mode-line-remote
                " "
                mode-line-buffer-identification
                ;; See my `beframe' package for how I use frames and
                ;; why having the name of one makes sense.
                prot/mode-line-frame-identification
                "  "
                mode-line-position
                mode-line-modes
                "  "
                (vc-mode vc-mode)
                "  "
                (:eval (propertize
                        " " 'display
                        ;; NOTE 2023-02-09: The `:align-to' is based
                        ;; on information from the Elisp manual.  Evaluate:
                        ;; (info "(elisp) Pixel Specification")
                        `((space :align-to
                                 (- (+ right-margin 1)
                                    ,(string-width (prot/mode-line-global-string-no-properties)))))))
                (:eval
                 (when (prot/mode-line-current-window-p)
                   mode-line-misc-info))
                mode-line-end-spaces))

(add-hook 'after-init-hook #'column-number-mode)

;;; Hide modeline "lighters" (minions.el)
(prot-emacs-elpa-package 'minions
  (setq minions-mode-line-lighter "")
  ;; NOTE: This will be expanded whenever I find a mode that should not
  ;; be hidden
  (setq minions-prominent-modes
        (list 'defining-kbd-macro
              'flymake-mode))
  (minions-mode 1))

;;; Mode line recursion indicators
(prot-emacs-elpa-package 'recursion-indicator
  (setq recursion-indicator-general "&")
  (setq recursion-indicator-minibuffer "@")
  (recursion-indicator-mode 1))

;;; Keycast mode
(prot-emacs-elpa-package 'keycast
  (setq keycast-mode-line-insert-after 'prot/mode-line-frame-identification)
  (setq keycast-mode-line-remove-tail-elements nil)

  (dolist (input '(self-insert-command
                   org-self-insert-command))
    (add-to-list 'keycast-substitute-alist `(,input "." "Typing…")))

  (dolist (event '(mouse-event-p
                   mouse-movement-p
                   mwheel-scroll))
    (add-to-list 'keycast-substitute-alist `(,event nil))))

(provide 'prot-emacs-modeline)
