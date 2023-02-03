;;; early-init.el --- Early Init File -*- lexical-binding: t -*-

;; Copyright (c) 2020-2023  Protesilaos Stavrou <info@protesilaos.com>

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos.com/emacs/dotemacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

;; This file is NOT part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Prior to Emacs 27, the `init.el' was supposed to handle the
;; initialisation of the package manager, by means of calling
;; `package-initialize'.  Starting with Emacs 27, the default
;; behaviour is to start the package manager before loading the init
;; file.
;;
;; See my dotfiles: https://git.sr.ht/~protesilaos/dotfiles

;;; Code:

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(dolist (var '(default-frame-alist initial-frame-alist))
  (add-to-list var '(width . (text-pixels . 1200)))
  (add-to-list var '(height . (text-pixels . 900))))

(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t)

(setq use-dialog-box t ; only for mouse events, which I seldom use
      use-file-dialog nil
      inhibit-splash-screen t
      inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name ; read the docstring
      inhibit-startup-buffer-menu t)

;; Initialise installed packages
(setq package-enable-at-startup t)

(defvar package-quickstart)

;; Allow loading from the package cache
(setq package-quickstart t)

(setq native-comp-async-report-warnings-errors 'silent) ; emacs28 with native compilation


;;;; General theme code

(defun prot-emacs-theme-gesttings-dark-p ()
  "Return non-nil if gsettings (GNOME) has a dark theme.
Return nil if the DESKTOP_SESSION is either bspwm or
herbstluftwm, per the configuration of my dotfiles.  Also check
the `delight' shell script."
  (let ((session (getenv "DESKTOP_SESSION")))
    (unless (or (string= session "bspwm")
                (string= session "herbstluftwm"))
      (string-match-p
       "dark"
       (shell-command-to-string "gsettings get org.gnome.desktop.interface color-scheme")))))

(defun prot-emacs-theme-twm-dark-p ()
  "Return non-nil if my custom setup has a dark theme.
I place a file in ~/.config/bspwm/active-theme or
~/.config/herbstluftwm/active-theme which contains a single word
describing my system-wide theme.  This is part of my dotfiles.
Check my `delight' shell script for more."
  (when-let* ((bspwm "~/.config/bspwm/active-theme")
              (hlwm "~/.config/herbstluftwm/active-theme")
              (file (cond
                     ((file-exists-p bspwm) bspwm)
                     ((file-exists-p hlwm) hlwm))))
    (string-match-p
     "dark"
     (with-temp-buffer
       (insert-file-contents file)
       (buffer-substring-no-properties (point-min) (point-max))))))

(defun prot-emacs-theme-environment-dark-p ()
  "Return non-nil if environment theme is dark."
  (or (prot-emacs-theme-twm-dark-p)
      (prot-emacs-theme-gesttings-dark-p)))

;; Avoid flash of light, if needed
(when (prot-emacs-theme-environment-dark-p)
  (setq mode-line-format nil)
  (set-face-background 'default "#000000"))

;;; early-init.el ends here
