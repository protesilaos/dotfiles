(defvar prot-emacs-tiling-window-manager-regexp
  (regexp-opt '("bspwm" "herbstluftwm" "i3"))
  "Regular expression to  tiling window managers.
See definition of `prot-emacs-with-desktop-session'.")

(defmacro prot-emacs-with-desktop-session (&rest body)
  "Expand BODY if desktop session is not a tiling window manager.
See `prot-emacs-tiling-window-manager-regexp' for what
constitutes a matching tiling window manager."
  (declare (indent 0))
  `(when-let ((session (getenv "DESKTOP_SESSION"))
              ((not (string-match-p session prot-emacs-tiling-window-manager-regexp))))
     ,@body))

(defun prot-emacs-add-to-list (list element)
  "Add to symbol of LIST the given ELEMENT.
Simplified version of `add-to-list'."
  (set list (cons element (symbol-value list))))

(prot-emacs-with-desktop-session
  (mapc
   (lambda (var)
     (prot-emacs-add-to-list var '(width . (text-pixels . 900)))
     (prot-emacs-add-to-list var '(height . (text-pixels . 600)))
     (prot-emacs-add-to-list var '(scroll-bar-width  . 10)))
   '(default-frame-alist initial-frame-alist)))

(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
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

;; I do not use those graphical elements by default, but I do enable
;; them from time-to-time for testing purposes or to demonstrate
;; something.  NEVER tell a beginner to disable any of these.  They
;; are helpful.
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

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
            (setq gc-cons-threshold (* 1024 1024 20)
                  gc-cons-percentage 0.2
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
the `delight' shell script."
  (prot-emacs-with-desktop-session
    (string-match-p
     "dark"
     (shell-command-to-string "gsettings get org.gnome.desktop.interface color-scheme"))))

(defun prot-emacs-theme-twm-dark-p ()
  "Return non-nil if my custom setup has a dark theme.
I place a file in ~/.config/prot-xtwm-active-theme which contains
a single word describing my system-wide theme.  This is part of
my dotfiles.  Check my `delight' shell script for more."
  (when-let ((file "~/.config/prot-xtwm-active-theme")
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
  (when-let ((theme (car custom-enabled-themes)))
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
