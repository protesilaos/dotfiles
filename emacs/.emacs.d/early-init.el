(defvar prot-emacs-tiling-window-manager-regexp "bspwm\\|herbstluftwm\\|i3"
  "Regular expression to  tiling window managers.
See definition of `prot-emacs-with-desktop-session'.")

(defmacro prot-emacs-with-desktop-session (&rest body)
  "Expand BODY if desktop session is not a tiling window manager.
See `prot-emacs-tiling-window-manager-regexp' for what
constitutes a matching tiling window manager."
  (declare (indent 0))
  `(when-let* ((session (getenv "DESKTOP_SESSION"))
               ((not (string-match-p session prot-emacs-tiling-window-manager-regexp))))
     ,@body))

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

(when prot-laptop-p
  (add-hook 'window-size-change-functions #'frame-hide-title-bar-when-maximized))

(setq initial-frame-alist `((horizontal-scroll-bars . nil)
                            (menu-bar-lines . 0) ; alternative to disabling `menu-bar-mode'
                            (tool-bar-lines . 0) ; alternative to disabling `tool-bar-mode'
                            (width . (text-pixels . 800))
                            (height . (text-pixels . 900))
                            ,@(when prot-laptop-p
                                (list '(fullscreen . maximized)))
                            ,@(if x-toolkit-scroll-bars
                                  (list
                                   '(vertical-scroll-bars . nil)
                                   '(scroll-bar-width . 12))
                                (list
                                 '(vertical-scroll-bars . right)
                                 '(scroll-bar-width . 6)))))

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
                                                         (width . (text-pixels . 800))
                                                         (height . (text-pixels . 900))
                                                         ,@(when prot-laptop-p
                                                             (list '(fullscreen . maximized)))
                                                         ,@(if x-toolkit-scroll-bars
                                                               (list
                                                                '(vertical-scroll-bars . nil)
                                                                '(scroll-bar-width . 12))
                                                             (list
                                                              '(vertical-scroll-bars . right)
                                                              '(scroll-bar-width . 6)))))))

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

;;;; General theme code

(defun prot-emacs-theme-gsettings-dark-p ()
  "Return non-nil if gsettings (GNOME) has a dark theme.
Return nil if the DESKTOP_SESSION is either bspwm or
herbstluftwm, per the configuration of my dotfiles.  Also check
the `delight.sh' shell script."
  (prot-emacs-with-desktop-session
    (string-match-p
     "dark"
     (shell-command-to-string "gsettings get org.gnome.desktop.interface color-scheme"))))

(defun prot-emacs-theme-twm-dark-p ()
  "Return non-nil if my custom setup has a dark theme.
I place a file in ~/.config/prot-xtwm-active-theme which contains
a single word describing my system-wide theme.  This is part of
my dotfiles.  Check my `delight.sh' shell script for more."
  (when-let* ((file "~/.config/prot-xtwm-active-theme")
              ((file-exists-p file)))
    (string-match-p
     "dark"
     (with-temp-buffer
       (insert-file-contents file)
       (buffer-string)))))

(defun prot-emacs-theme-environment-dark-p ()
  "Return non-nil if environment theme is dark."
  (or (prot-emacs-theme-twm-dark-p)
      (prot-emacs-theme-gsettings-dark-p)))

(defun prot-emacs-re-enable-frame-theme (_frame)
  "Re-enable active theme, if any, upon FRAME creation.
Add this to `after-make-frame-functions' so that new frames do
not retain the generic background set by the function
`prot-emacs-avoid-initial-flash-of-light'."
  (when-let* ((theme (car custom-enabled-themes)))
    (enable-theme theme)))

;; NOTE 2023-02-05: The reason the following works is because (i) the
;; `mode-line-format' is specified again and (ii) the
;; `prot-emacs-theme-gsettings-dark-p' will load a dark theme.
(defun prot-emacs-avoid-initial-flash-of-light ()
  "Avoid flash of light when starting Emacs, if needed.
New frames are instructed to call `prot-emacs-re-enable-frame-theme'."
  (when (prot-emacs-theme-environment-dark-p)
    (setq mode-line-format nil)
    (set-face-attribute 'default nil :background "#000000" :foreground "#ffffff")
    (set-face-attribute 'mode-line nil :background "#000000" :foreground "#ffffff" :box 'unspecified)
    (add-hook 'after-make-frame-functions #'prot-emacs-re-enable-frame-theme)))

(prot-emacs-avoid-initial-flash-of-light)

(add-hook 'after-init-hook (lambda () (set-frame-name "home")))
