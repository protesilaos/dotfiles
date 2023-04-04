;;; Mode line
(setq mode-line-percent-position '(-3 "%p"))
(setq mode-line-position-column-line-format '("%l,%c")) ; Emacs 28
(setq mode-line-compact nil)                            ; Emacs 28

(defface prot/mode-line-intense
  '((((class color) (min-colors 88) (background light))
     :background "gray40" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "gray70" :foreground "black")
    (t :inverse-video t))
  "Face for intense mode line constructs.")

(setq mode-line-defining-kbd-macro
      (propertize " KMacro " 'face 'prot/mode-line-intense))

(defun prot/mode-line-global-value ()
  "Return value of `global-mode-string'."
  (mapconcat
   (lambda (sym)
     (if (symbolp sym)
         (symbol-value sym)
       sym))
   global-mode-string
   nil))

;; NOTE 2023-03-22: The `risky-local-variable' is critical, as those
;; variables will not work without it.
(defvar prot/mode-line-modes
  (list (propertize "%[" 'face 'error)
        `(:propertize ("" mode-name)
                      mouse-face mode-line-highlight
                      local-map ,mode-line-major-mode-keymap)
        '("" mode-line-process)
        (propertize "%]" 'face 'error)
        " ")
  "Mode line construct for displaying major modes.")

(defvar prot/mode-line-align-right
  '(:eval (propertize
           " " 'display
           `((space :align-to
                    (- (+ right right-fringe right-margin)
                       ,(string-width
                          (format-mode-line mode-line-misc-info)))))))
  "Mode line construct to align following elements to the right.
Read Info node `(elisp) Pixel Specification'.")

(defvar prot/mode-line-kbd-macro
  '(:eval (when (and defining-kbd-macro (mode-line-window-selected-p))
            mode-line-defining-kbd-macro))
  "Mode line construct displaying `mode-line-defining-kbd-macro'.
Specific to the current window's mode line.")

(defvar prot/mode-line-flymake
  '(:eval (when flymake-mode
            flymake-mode-line-format))
  "Mode line construct displaying `flymake-mode-line-format'.
Specific to the current window's mode line.")

(defvar prot/mode-line-misc-info
  '(:eval
    (when (mode-line-window-selected-p)
      mode-line-misc-info))
  "Mode line construct displaying `mode-line-misc-info'.
Specific to the current window's mode line.")

(dolist (construct '( prot/mode-line-modes prot/mode-line-align-right
                      prot/mode-line-kbd-macro prot/mode-line-flymake
                      prot/mode-line-misc-info))
  (put construct 'risky-local-variable t))

(setq-default mode-line-format
              '("%e"
                prot/mode-line-kbd-macro
                " "
                mode-line-mule-info
                mode-line-modified
                mode-line-remote
                " "
                mode-line-buffer-identification
                " "
                mode-line-position
                prot/mode-line-modes
                prot/mode-line-flymake
                " "
                (vc-mode vc-mode)
                " "
                prot/mode-line-align-right
                prot/mode-line-misc-info))

(add-hook 'after-init-hook #'column-number-mode)

;;; Keycast mode
(prot-emacs-elpa-package 'keycast (:delay 10)
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
