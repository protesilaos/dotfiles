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

;; This variable is incremented in prot-emacs.org.  The idea is to
;; produce a list of packages that we want to install on demand from an
;; ELPA.  So someone who tries to reproduce my Emacs setup will first
;; get a bunch of warnings about unavailable packages, though not
;; show-stopping errors, and will then have to use the command
;; `prot-emacs-builtin-package'.  After that command does its job, a
;; re-run of my Emacs configurations will yield the expected results.
;;
;; The assumption is that such a user will want to inspect the elements
;; of `prot-emacs-ensure-install', remove from the setup whatever code
;; block they do not want, and then call the aforementioned command.
;;
;; I do not want to maintain a setup that auto-installs everything on
;; first boot.  I think that is a bad practice because it teaches the
;; user to just trust the provider, which they should not do in advance.
;; Besides, this is not an Emacs distro targeted at a general audience:
;; it is just my personal config.
(defvar prot-emacs-ensure-install nil
  "List of package names used by `prot-emacs-install-ensured'.")

(defun prot-emacs-install-ensured ()
  "Install all `prot-emacs-ensure-install' packages, if needed.
If a package is already installed, no further action is performed
on it."
  (interactive)
  (when (yes-or-no-p (format "Try to install %d packages?"
                             (length prot-emacs-ensure-install)))
    (package-refresh-contents)
    (mapc (lambda (package)
            (unless (package-installed-p package)
              (package-install package)))
          prot-emacs-ensure-install)))

(defmacro prot-emacs-builtin-package (package &rest body)
  "Set up builtin PACKAGE with rest BODY.
PACKAGE is a quoted symbol, while BODY consists of balanced
expressions."
  (declare (indent 1))
  `(progn
     (unless (require ,package nil 'noerror)
       (display-warning 'prot-emacs (format "Loading `%s' failed" ,package) :error))
     ,@body))

(defmacro prot-emacs-elpa-package (package &rest body)
  "Set up PACKAGE from an Elisp archive with rest BODY.
PACKAGE is a quoted symbol, while BODY consists of balanced
expressions."
  (declare (indent 1))
  `(progn
     (unless (require ,package nil 'noerror)
       (display-warning 'prot-emacs (format "Loading `%s' failed" ,package) :error)
       (add-to-list 'prot-emacs-ensure-install ,package)
       (display-warning
        'prot-emacs
        (format "Run `prot-emacs-install-ensured' to install all packages in `prot-emacs-ensure-install'")
        :warning))
     ,@body))

(defmacro prot-emacs-manual-package (package &rest body)
  "Set up manually installed PACKAGE with rest BODY.
PACKAGE is a quoted symbol, while BODY consists of balanced
expressions."
  (declare (indent 1))
  (let ((path (thread-last user-emacs-directory
                (expand-file-name "contrib-lisp")
                (expand-file-name (symbol-name (eval package))))))
    `(progn
       (eval-and-compile
         (add-to-list 'load-path ,path))
       (unless (require ,package nil 'noerror)
         (display-warning 'prot-emacs (format "Loading `%s' failed" ,package) :error)
         (display-warning 'prot-emacs (format "This must be available at %s" ,path) :warning))
       (with-eval-after-load ,package
         ,@body))))

(require 'vc)
(setq vc-follow-symlinks t) ; Because my dotfiles are managed that way

;; For my custom libraries
(add-to-list 'load-path (expand-file-name
                         (concat user-emacs-directory "prot-lisp")))
(add-to-list 'load-path (expand-file-name
                         (concat user-emacs-directory "modus-themes")))

;; For other libraries
(add-to-list 'load-path (expand-file-name
                         (concat user-emacs-directory "contrib-lisp")))

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
