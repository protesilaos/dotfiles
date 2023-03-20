;;; Mode line
(setq mode-line-percent-position '(-3 "%p"))
(setq mode-line-position-column-line-format '(" %l,%c")) ; Emacs 28
(setq mode-line-compact nil)                             ; Emacs 28

(defface prot/mode-line-intense
  '((((class color) (min-colors 88) (background light))
     :background "gray40" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "gray70" :foreground "black")
    (t :inverse-video t))
  "Face for intense mode line constructs.")

(set-face-attribute 'display-time-date-and-time nil :inherit 'prot/mode-line-intense) ; Emacs 30

(setq mode-line-defining-kbd-macro
      (propertize " KMacro " 'face 'prot/mode-line-intense))

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

(defvar prot/mode-line-modes
  (let ((recursive-edit-help-echo "Recursive edit, type M-C-c to get out"))
    (list (propertize "%[" 'help-echo recursive-edit-help-echo)
          `(:propertize ("" mode-name)
                        help-echo "Major mode\n\
mouse-1: Display major mode menu\n\
mouse-2: Show help for major mode\n\
mouse-3: Toggle minor modes"
                        mouse-face mode-line-highlight
                        local-map ,mode-line-major-mode-keymap)
          '("" mode-line-process)
          (propertize "%]" 'help-echo recursive-edit-help-echo)
          " "))
  "Mode line construct for displaying major modes.")
(put 'prot/mode-line-modes 'risky-local-variable t)

;; NOTE 2023-02-09: The `:eval' parts are HIGHLY EXPERIMENTAL.
(setq-default mode-line-format
              '("%e"
                (:eval (when (and defining-kbd-macro
                                  (prot/mode-line-current-window-p))
                         mode-line-defining-kbd-macro))
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
                prot/mode-line-modes
                (:eval (when flymake-mode
                         flymake-mode-line-format))
                "  "
                (vc-mode vc-mode)
                "  "
                (:eval (propertize
                        " " 'display
                        ;; NOTE 2023-02-09: The `:align-to' is based
                        ;; on information from the Elisp manual.  Evaluate:
                        ;; (info "(elisp) Pixel Specification")
                        `((space :align-to
                                 (- (+ right-margin 3)
                                    ,(string-width (prot/mode-line-global-string-no-properties)))))))
                (:eval
                 (when (prot/mode-line-current-window-p)
                   mode-line-misc-info))))

(add-hook 'after-init-hook #'column-number-mode)

;;; Mode line recursion indicators
(prot-emacs-elpa-package 'recursion-indicator
  (setq recursion-indicator-general "&")
  (setq recursion-indicator-minibuffer "@")

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
