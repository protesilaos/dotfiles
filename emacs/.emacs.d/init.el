;; init.el --- Personal GNU Emacs configuration file.

;; Copyright (c) 2019-2020 Protesilaos Stavrou <info@protesilaos.com>
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

;; This file sets up the essentials for incorporating my init org
;; file.  This is known as "literate programming", which I think is
;; particularly helpful for sharing Emacs configurations with a wider
;; audience that includes new or potential users (I am still very new
;; myself).
;;
;; See my dotfiles: https://gitlab.com/protesilaos/dotfile

;;; Code:

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

;; In Emacs >=27 packages are initialised before loading the user's
;; init file.  This can be configured in the `early-init-file'.  Older
;; versions of Emacs will read `package-enable-at-startup' from here.
(when (< emacs-major-version 27)
  (setq package-enable-at-startup nil))

(package-initialize)

(require 'org)
(org-babel-load-file (expand-file-name "~/.emacs.d/emacs-init.org"))

;;; init.el ends here
