;; early-init.el --- Early Init File for >= Emacs 27.

;; Copyright (c) 2020-2021  Protesilaos Stavrou <info@protesilaos.com>
;;
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
;; file.  This can create unexpected results with existing
;; configuration files.  I prefer the old behaviour so I defer the
;; process to `init.el'.
;;
;; See my dotfiles: https://gitlab.com/protesilaos/dotfiles

;;; Code:

;; Do not initialise installed packages (I use `straight.el')
(setq package-enable-at-startup nil)

;; Do not allow loading from the package cache (same reason).
(setq package-quickstart nil)

;; Do not resize the frame at this early stage.
(setq frame-inhibit-implied-resize t)

;; Disable GUI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)
(setq use-dialog-box t)               ; only for mouse events
(setq use-file-dialog nil)
