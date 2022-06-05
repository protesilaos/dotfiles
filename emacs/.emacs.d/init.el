;;; init.el --- Personal configuration file -*- lexical-binding: t -*-

;; Copyright (c) 2019-2022  Protesilaos Stavrou <info@protesilaos.com>

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

;; This file sets up the essentials for incorporating my init org
;; file.  This is known as "literate programming", which I think is
;; particularly helpful for sharing Emacs configurations with a wider
;; audience that includes new or potential users (I am still very new
;; myself).
;;
;; See my dotfiles: https://git.sr.ht/~protesilaos/dotfiles

;;; Code:

(require 'package)

(add-to-list 'package-archives
             '("elpa-devel" . "https://elpa.gnu.org/devel/"))

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(defvar prot-emacs-autoinstall-elpa nil
  "Whether `prot-emacs-elpa-package' should install packages.
The default nil value means never to automatically install
packages.  A non-nil value is always interpreted as consent for
auto-installing everything---this process does not cover manually
maintained git repos, controlled by `prot-emacs-manual-package'.")

(defvar prot-emacs-basic-init "basic-init.el"
  "Name of 'basic init' file.

This file is meant to store user configurations that are evaluated
before loading `prot-emacs-configuration-main-file' and, when
available, `prot-emacs-configuration-user-file'.  Those values
control the behaviour of the Emacs setup.

The only variable that is currently expected to be in the 'basic
init' file is `prot-emacs-autoinstall-elpa'.

See `prot-emacs-basic-init-setup' for the actual initialisation
process.")

(defun prot-emacs-basic-init-setup ()
  "Load 'basic-init.el' if it exists.
This is meant to evaluate forms that control the rest of my Emacs
setup."
  (let* ((init prot-emacs-basic-init)
         (file (locate-user-emacs-file init)))
    (when (file-exists-p file)
      (load-file file))))

;; This variable is incremented in prot-emacs.org.  The idea is to
;; produce a list of packages that we want to install on demand from an
;; ELPA, when `prot-emacs-autoinstall-elpa' is set to nil (the default).
;;
;; So someone who tries to reproduce my Emacs setup will first get a
;; bunch of warnings about unavailable packages, though not
;; show-stopping errors, and will then have to use the command
;; `prot-emacs-install-ensured'.  After that command does its job, a
;; re-run of my Emacs configurations will yield the expected results.
;;
;; The assumption is that such a user will want to inspect the elements
;; of `prot-emacs-ensure-install', remove from the setup whatever code
;; block they do not want, and then call the aforementioned command.
;;
;; I do not want to maintain a setup that auto-installs everything on
;; first boot without requiring explicit consent.  I think that is a bad
;; practice because it teaches the user to simply put their faith in the
;; provider.
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
       (display-warning 'prot-emacs (format "Loading `%s' failed" ,package) :warning))
     ,@body))

(defmacro prot-emacs-elpa-package (package &rest body)
  "Set up PACKAGE from an Elisp archive with rest BODY.
PACKAGE is a quoted symbol, while BODY consists of balanced
expressions.

When `prot-emacs-autoinstall-elpa' is non-nil try to install the
package if it is missing."
  (declare (indent 1))
  `(progn
     (when (and prot-emacs-autoinstall-elpa
                (not (package-installed-p ,package)))
       (package-install ,package))
     (if (require ,package nil 'noerror)
         (progn ,@body)
       (display-warning 'prot-emacs (format "Loading `%s' failed" ,package) :warning)
       (add-to-list 'prot-emacs-ensure-install ,package)
       (display-warning
        'prot-emacs
        "Run `prot-emacs-install-ensured' to install all packages in `prot-emacs-ensure-install'"
        :warning))))

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
       (if (require ,package nil 'noerror)
	       (progn ,@body)
         (display-warning 'prot-emacs (format "Loading `%s' failed" ,package) :warning)
         (display-warning 'prot-emacs (format "This must be available at %s" ,path) :warning)))))

(require 'vc)
(setq vc-follow-symlinks t) ; Because my dotfiles are managed that way

(defvar prot-emacs-elisp-directories '("prot-lisp" "contrib-lisp" "modus-themes")
  "List of directories in the Emacs setup with custom Elisp.")

;; "prot-lisp" is for all my custom libraries; "contrib-lisp" is for
;; third-party code that I handle manually; while "modus-themes"
;; contains my themes which I use directly from source for development
;; purposes.
(dolist (path prot-emacs-elisp-directories)
  (add-to-list 'load-path (locate-user-emacs-file path)))

;; Some basic settings
(setq frame-title-format '("%b"))
(setq default-input-method "greek")
(setq ring-bell-function 'ignore)

(setq use-short-answers t)    ; for Emacs28, replaces the defalias below
;; (defalias 'yes-or-no-p 'y-or-n-p)

(dolist (c '( narrow-to-region narrow-to-page upcase-region
              downcase-region dired-find-alternate-file))
  (put c 'disabled nil))

(put 'overwrite-mode 'disabled t)

(setq initial-buffer-choice t)			; always start with *scratch*

;; I create an "el" version of my Org configuration file as a final step
;; before closing down Emacs (see further below).  This is done to load
;; the latest version of my code upon startup.  Also helps with
;; initialisation times.  Not that I care too much about those...

(defvar prot-emacs-configuration-main-file "prot-emacs"
  "Base name of the main configuration file.")

;; THIS IS EXPERIMENTAL.  Basically I want to test how we can let users
;; include their own customisations in addition to my own.  Those will
;; be stored in a separate Org file.
(defvar prot-emacs-configuration-user-file "user-emacs"
  "Base name of user-specific configuration file.")

(defun prot-emacs--expand-file-name (file extension)
  "Return canonical path to FILE to Emacs config with EXTENSION."
  (locate-user-emacs-file
   (concat file extension)))

(defun prot-emacs-load-config ()
  "Load main Emacs configurations, either '.el' or '.org' file."
  (let* ((main-init prot-emacs-configuration-main-file)
         (main-init-el (prot-emacs--expand-file-name main-init ".el"))
         (main-init-org (prot-emacs--expand-file-name main-init ".org"))
         (user-init prot-emacs-configuration-user-file)
         (user-init-el (prot-emacs--expand-file-name user-init ".el"))
         (user-init-org (prot-emacs--expand-file-name user-init ".org")))
    (prot-emacs-basic-init-setup)
    (require 'org)
    (cond
     ((file-exists-p main-init-el)
      (load-file main-init-el))
     ((file-exists-p main-init-org)
      (org-babel-load-file main-init-org)))
    (cond
     ((file-exists-p user-init-el)
      (load-file user-init-el))
     ((file-exists-p user-init-org)
      (org-babel-load-file user-init-org)))))

;; Load configurations.
(prot-emacs-load-config)

;; The following as for when we close the Emacs session.
(declare-function org-babel-tangle-file "ob-tangle")

(defun prot-emacs-build-config ()
  "Produce Elisp init from my Org dotemacs.
Add this to `kill-emacs-hook', to use the newest file in the next
session.  The idea is to reduce startup time, though just by
rolling it over to the end of a session rather than the beginning
of it."
  (interactive)
  (let* ((main-init prot-emacs-configuration-main-file)
         (main-init-el (prot-emacs--expand-file-name main-init ".el"))
         (main-init-org (prot-emacs--expand-file-name main-init ".org"))
         (user-init prot-emacs-configuration-user-file)
         (user-init-el (prot-emacs--expand-file-name user-init ".el"))
         (user-init-org (prot-emacs--expand-file-name user-init ".org")))
    (when (file-exists-p main-init-el)
      (delete-file main-init-el))
    (when (file-exists-p user-init-el)
      (delete-file user-init-el))
    (require 'org)
    (when (file-exists-p main-init-org)
      (org-babel-tangle-file main-init-org main-init-el)
      (byte-compile-file main-init-el))
    (when (file-exists-p user-init-org)
      (org-babel-tangle-file user-init-org user-init-el)
      (byte-compile-file user-init-el))))

;; NOTE 2022-06-05: Experimental.  Since I have lots of custom code of
;; mine, I thought it would be a good idea to have .elc versions of it.
;; Whether this will have a noticeable effect or not remains to be
;; determined.
(defun prot-emacs-byte-compile-files ()
  "Byte compile everything in `prot-emacs-elisp-directories'."
  (mapc (lambda (dir)
          (byte-recompile-directory
           (file-name-as-directory (concat user-emacs-directory dir)) 0))
        prot-emacs-elisp-directories))

(add-hook 'kill-emacs-hook #'prot-emacs-build-config)
(add-hook 'kill-emacs-hook #'prot-emacs-byte-compile-files)

;;; init.el ends here
