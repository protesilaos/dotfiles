;;; early-init.el --- Early Init File -*- lexical-binding: t -*-

;; Copyright (c) 2020-2022  Protesilaos Stavrou <info@protesilaos.com>

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

(setq frame-resize-pixelwise t)

;; Do not resize the frame at this early stage.
(setq frame-inhibit-implied-resize t)

;; ;; NOTE 2022-11-15: This does not work, presumably because it is too
;; ;; early for `display-pixel-width' to do its job.
;;
;; (let* ((dimensions (if (> (display-pixel-width) 1920)
;;                        (list 1920 1080)
;;                      (list 1600 900)))
;;        (h (car dimensions))
;;        (w (cadr dimensions)))
;;   (dolist (var '(default-frame-alist initial-frame-alist))
;;     (add-to-list var `(width . ,(cons 'text-pixels w)))
;;     (add-to-list var `(height . ,(cons 'text-pixels h)))))

(dolist (var '(default-frame-alist initial-frame-alist))
  (add-to-list var '(width . (text-pixels . 1200)))
  (add-to-list var '(height . (text-pixels . 900))))

;; Initialise installed packages
(setq package-enable-at-startup t)

(defvar package-quickstart)

;; Allow loading from the package cache
(setq package-quickstart t)

;; Disable GUI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)
(setq use-dialog-box t)                 ; only for mouse events
(setq use-file-dialog nil)

(setq inhibit-startup-echo-area-message user-login-name) ; read the docstring
(setq inhibit-startup-screen t)
(setq inhibit-startup-buffer-menu t)

(setq native-comp-async-report-warnings-errors 'silent) ; emacs28 with native compilation

;;; early-init.el ends here
