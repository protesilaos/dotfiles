;;; init.el --- Personal configuration file -*- lexical-binding: t -*-

;; Copyright (c) 2019-2021  Protesilaos Stavrou <info@protesilaos.com>

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos.com/dotemacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))

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

;; This file sets up the essentials for incorporating my init org
;; file.  This is known as "literate programming", which I think is
;; particularly helpful for sharing Emacs configurations with a wider
;; audience that includes new or potential users (I am still very new
;; myself).
;;
;; See my dotfiles: https://gitlab.com/protesilaos/dotfiles

;;; Code:

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(defmacro prot-emacs-manual-package (package &rest args)
  "Set up manually installed PACKAGE (symbol) with rest ARGS."
  (declare (indent defun))
  `(let ((path (file-name-as-directory
                (concat user-emacs-directory
                        "contrib-lisp/"
                        (pp-to-string ,package)))))
     (if (file-directory-p path)
         (progn
           (add-to-list 'load-path path)
           (require ,package)
           (with-eval-after-load ,package
             ,@args))
       (user-error "`%s' is not available in `%s'" ,package path))))

(defvar prot-emacs-ensure-install nil
  "List of package names to install, if missing.")

(defun prot-emacs-install-ensured ()
  "Install all `prot-emacs-ensure-install' packages, if needed."
  (interactive)
  (package-refresh-contents)
  (mapcar (lambda (package)
            (unless (package-installed-p package)
              (package-install package)))
          prot-emacs-ensure-install))

(require 'vc)
(setq vc-follow-symlinks t) ; Because my dotfiles are managed that way

;; For my custom libraries
(add-to-list 'load-path (concat user-emacs-directory "prot-lisp/"))
(add-to-list 'load-path (concat user-emacs-directory "modus-themes/"))

;; For other libraries
(add-to-list 'load-path (concat user-emacs-directory "contrib-lisp/"))

;; Some basic settings
(setq frame-title-format '("%b"))
(setq default-input-method "greek")
(setq ring-bell-function 'ignore)

(defalias 'yes-or-no-p 'y-or-n-p)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'overwrite-mode 'disabled t)

(setq initial-buffer-choice t)			; always start with *scratch*

;; I create an "el" version of my Org configuration file as a final step
;; before closing down Emacs.  This is done to load the latest version
;; of my code upon startup.
;;
;; Also helps with initialisation times.  Not that I care too much about
;; those… Hence why I no longer bother with deferring package loading
;; either by default or on a case-by-case basis.
(let* ((conf (concat user-emacs-directory "prot-emacs"))
       (el (concat conf ".el"))
       (org (concat conf ".org")))
  (if (file-exists-p el)
      (load-file el)
    (require 'org)
    (org-babel-load-file org)))

;;; init.el ends here
