(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize 'force
      frame-title-format '("%b")
      ring-bell-function 'ignore
      use-dialog-box t ; only for mouse events, which I seldom use
      use-file-dialog nil
      use-short-answers t
      inhibit-splash-screen t
      inhibit-startup-screen t
      inhibit-x-resources t
      inhibit-startup-echo-area-message user-login-name ; read the docstring
      inhibit-startup-buffer-menu t)

(defvar prot-laptop-p (null (directory-empty-p "/sys/class/power_supply/"))
  "When non-nil, we assume to be working on a laptop.")

(setq initial-frame-alist `((horizontal-scroll-bars . nil)
                            (menu-bar-lines . 0) ; alternative to disabling `menu-bar-mode'
                            (tool-bar-lines . 0) ; alternative to disabling `tool-bar-mode'
                            (vertical-scroll-bars . nil)
                            (scroll-bar-width . 6)
                            (width . (text-pixels . 800))
                            (height . (text-pixels . 900))
                            (undecorated . t)
                            (border-width . 5)
                            ,@(when prot-laptop-p
                                (list '(fullscreen . maximized)))))

;; Do it again after init so that any intermediate changes are not
;; retained.  Note that we cannot rely on setting this to
;; `initial-frame-alist' as that may change in the meantime.  We
;; explicitly set the value to be certain of the outcome.  This does
;; not inhibit other programs from modifying the list, though I would
;; consider it undesirable if they were touching these specific
;; settings.
(add-hook 'after-init-hook (lambda ()
                             (setq default-frame-alist `((horizontal-scroll-bars . nil)
                                                         (menu-bar-lines . 0) ; alternative to disabling `menu-bar-mode'
                                                         (tool-bar-lines . 0) ; alternative to disabling `tool-bar-mode'
                                                         (vertical-scroll-bars . nil)
                                                         (scroll-bar-width . 6)
                                                         (width . (text-pixels . 800))
                                                         (height . (text-pixels . 900))
                                                         (undecorated . t)
                                                         (border-width . 5)
                                                         ,@(when prot-laptop-p
                                                             (list '(fullscreen . maximized)))))))

(defun prot-emacs-no-minibuffer-scroll-bar (frame)
  "Remove the minibuffer scroll bars from FRAME."
  (when scroll-bar-mode
    (set-window-scroll-bars (minibuffer-window frame) nil nil nil nil :persistent)))

(add-hook 'after-make-frame-functions #'prot-emacs-no-minibuffer-scroll-bar)

;; Temporarily increase the garbage collection threshold.  These
;; changes help shave off about half a second of startup time.  The
;; `most-positive-fixnum' is DANGEROUS AS A PERMANENT VALUE.  See the
;; `emacs-startup-hook' a few lines below for what I actually use.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

;; Same idea as above for the `file-name-handler-alist' and the
;; `vc-handled-backends' with regard to startup speed optimisation.
;; Here I am storing the default value with the intent of restoring it
;; via the `emacs-startup-hook'.
(defvar prot-emacs--file-name-handler-alist file-name-handler-alist)
(defvar prot-emacs--vc-handled-backends vc-handled-backends)

(setq file-name-handler-alist nil
      vc-handled-backends nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 100 100 8)
                  gc-cons-percentage 0.1
                  file-name-handler-alist prot-emacs--file-name-handler-alist
                  vc-handled-backends prot-emacs--vc-handled-backends)))

;; Initialise installed packages at this early stage, by using the
;; available cache.  I had tried a setup with this set to nil in the
;; early-init.el, but (i) it ended up being slower and (ii) various
;; package commands, like `describe-package', did not have an index of
;; packages to work with, requiring a `package-refresh-contents'.
(setq package-enable-at-startup t)

(add-hook 'after-init-hook (lambda () (set-frame-name "home")))
