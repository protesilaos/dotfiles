;;; init.el --- Personal configuration file -*- lexical-binding: t -*-

;; Copyright (c) 2019-2023  Protesilaos Stavrou <info@protesilaos.com>

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

;; See my dotfiles: <https://git.sr.ht/~protesilaos/dotfiles>

;;; Code:

(defgroup prot-emacs nil
  "User options for my dotemacs."
  :group 'file)

;; For those who use my dotfiles and need an easy way to write their
;; own extras on top of what I already load: search below for the files
;; prot-emacs-pre-custom.el and prot-emacs-post-custom.el

(defcustom prot-emacs-load-theme-family 'modus
  "Set of themes to load.
Valid values are the symbols `ef', `modus', and `standard', which
reference the `ef-themes', `modus-themes', and `standard-themes',
respectively.

A nil value does not load any of the above (use Emacs without a
theme).

This user option must be set in the `prot-emacs-pre-custom.el'
file.  If that file exists in the Emacs directory, it is loaded
before all other modules of my setup."
  :group 'prot-emacs
  :type '(choice :tag "Set of themes to load" :value modus
                 (const :tag "The `ef-themes' module" ef)
                 (const :tag "The `modus-themes' module" modus)
                 (const :tag "The `standard-themes' module" standard)
                 (const :tag "Do not load a theme module" nil)))

(defcustom prot-emacs-completion-ui 'vertico
  "Choose minibuffer completion UI between `mct' or `vertico'."
  :group 'prot-emacs
  :type '(choice :tag "Minibuffer user interface"
                 (const :tag "The `mct' module" mct)
                 (const :tag "The `vertico' module" vertico)))

(defcustom prot-emacs-omit-packages nil
  "List of package names to not load.
This instructs the relevant macros to not `require' the given
package.  In the case of `prot-emacs-elpa-package', the package
will not be installed if it is not already available on the
system.

This user option must be set in the `prot-emacs-pre-custom.el'
file.  If that file exists in the Emacs directory, it is loaded
before all other modules of my setup."
  :group 'prot-emacs
  :type '(repeat symbol))

;; Some basic settings
(setq frame-title-format '("%b"))
(setq ring-bell-function 'ignore)
(setq use-short-answers t)

;; In 'M-x find-library RET greek' we also find the greek-postfix style,
;; though I don't need it right now.
(setq default-input-method "greek")

;; Enable those
(dolist (c '( narrow-to-region narrow-to-page upcase-region downcase-region))
  (put c 'disabled nil))

(put 'overwrite-mode 'disabled t)

(setq initial-buffer-choice t) ; always start with *scratch*

;;;; Packages

(dolist (path '("prot-lisp" "prot-emacs-modules"))
  (add-to-list 'load-path (locate-user-emacs-file path)))

(require 'package)
;; All variables are for Emacs 28+
(setq package-name-column-width 40)
(setq package-version-column-width 14)
(setq package-status-column-width 12)
(setq package-archive-column-width 8)
(add-hook 'package-menu-mode-hook #'hl-line-mode)

;; Also read: <https://protesilaos.com/codelog/2022-05-13-emacs-elpa-devel/>
(setq package-archives
      '(("elpa" . "https://elpa.gnu.org/packages/")
        ("elpa-devel" . "https://elpa.gnu.org/devel/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

;; Highest number gets priority (what is not mentioned has priority 0)
(setq package-archive-priorities
      '(("elpa" . 2)
        ("nongnu" . 1)))

;; I want to use my own packages from specific repositories.  All
;; others will rely on `package-archive-priorities'.  I do this to
;; test that the packaged version works as intended.
(defvar prot-emacs-my-packages
  '(agitate
    altcaps
    beframe
    cursory
    denote
    ef-themes
    fontaine
    lin
    logos
    mct
    modus-themes
    notmuch-indicator
    pulsar
    standard-themes
    substitute
    sxhkdrc-mode
    tmr)
  "List of symbols representing the packages I develop/maintain.")

(setq package-pinned-packages
      `(,@(mapcar
           (lambda (package)
             (cons package "elpa-devel"))
           prot-emacs-my-packages)))

(setq custom-safe-themes t)

(defmacro prot-emacs-builtin-package (package &rest body)
  "Set up builtin PACKAGE with rest BODY.
PACKAGE is a quoted symbol, while BODY consists of balanced
expressions.

Ignore PACKAGE if it is a member of `prot-emacs-omit-packages'.

The first element of BODY can be a property list of (:delay
number).  PACKAGE and the `cdr' of BODY are then loaded with the
given delay as idle time, per `run-with-idle-timer'."
  (declare (indent 1))
  (let ((common
         `(unless (and (not (memq ,package prot-emacs-omit-packages))
                       (require ,package nil 'noerror))
            (display-warning 'prot-emacs (format "`%s' failed" ,package) :warning))))
  (if-let ((delay (plist-get (car body) :delay)))
      `(run-with-idle-timer ,delay nil (lambda () ,common ,@(cdr body)))
    `(progn ,common ,@body))))

(defmacro prot-emacs-elpa-package (package &rest body)
  "Set up PACKAGE from an Elisp archive with rest BODY.
PACKAGE is a quoted symbol, while BODY consists of balanced
expressions.

Try to install the PACKAGE if it is missing.

Ignore PACKAGE, including the step of installing it, if it is a
member of `prot-emacs-omit-packages'.

The first element of BODY can be a property list of (:delay
number).  PACKAGE and the `cdr' of BODY are then loaded with the
given delay as idle time, per `run-with-idle-timer'."
  (declare (indent 1))
  ;; FIXME 2023-04-04: Avoid duplication like with `prot-emacs-builtin-package'.
  (if-let ((delay (plist-get (car body) :delay)))
      `(run-with-idle-timer
        ,delay nil
        (lambda ()
          (unless (memq ,package prot-emacs-omit-packages)
            (progn
              (when (not (package-installed-p ,package))
                (unless package-archive-contents
                  (package-refresh-contents))
                (package-install ,package))
              (if (require ,package nil 'noerror)
                  (progn ,@(cdr body))
                (display-warning 'prot-emacs (format "`%s' failed" ,package) :warning))))))
    `(unless (memq ,package prot-emacs-omit-packages)
       (progn
         (when (not (package-installed-p ,package))
           (unless package-archive-contents
             (package-refresh-contents))
           (package-install ,package))
         (if (require ,package nil 'noerror)
             (progn ,@body)
           (display-warning 'prot-emacs (format "`%s' failed" ,package) :warning))))))

(defmacro prot-emacs-vc-package (package remote &rest body)
  "Set up PACKAGE from its REMOTE source.
REMOTE is a plist that specifies:

- :url     A string pointing to the URL of the PACKAGE source.
           This is required.

- :branch  The branch to build from.  This is optional.  It
            defaults to the REMOTE's main branch.

BODY is the configuration associated with PACKAGE."
  (declare (indent 1))
  `(unless (memq ,package prot-emacs-omit-packages)
     (progn
       (when (not (package-installed-p ,package))
         (package-vc-install
          (cons ,package (list :url ,(plist-get remote :url)
                               ,@(when-let ((b (plist-get remote :branch)))
                                   (list :branch b))))))
       (if (require ,package nil 'noerror)
           (progn ,@body)
         (display-warning 'prot-emacs
                          (format "Loading `%s' failed" ,package)
                          :warning)))))

(defvar prot-emacs-package-form-regexp
  "^(\\(prot-emacs-.*-package\\|require\\) +'\\([0-9a-zA-Z-]+\\)"
  "Regexp to add packages to `lisp-imenu-generic-expression'.")

(eval-after-load 'lisp-mode
  `(add-to-list 'lisp-imenu-generic-expression
                (list "Packages" ,prot-emacs-package-form-regexp 2)))

;; For those who use my dotfiles and need an easy way to write their
;; own extras on top of what I already load.  The file must exist at
;; ~/.emacs.d/prot-emacs-pre-custom.el
;;
;; The purpose of this file is for the user to define their
;; preferences BEFORE loading any of the modules.  For example, the
;; user option `prot-emacs-omit-packages' lets the user specify which
;; packages not to load.  Search for all `defcustom' forms in this
;; file for other obvious customisations.
(when-let* ((file (locate-user-emacs-file "prot-emacs-pre-custom.el"))
            ((file-exists-p file)))
  (load-file file))

(require 'prot-emacs-essentials)
(pcase prot-emacs-load-theme-family
  ('ef (require 'prot-emacs-ef-themes))
  ('modus (require 'prot-emacs-modus-themes))
  ('standard (require 'prot-emacs-standard-themes)))
(require 'prot-emacs-theme-extras)
(require 'prot-emacs-font)
(require 'prot-emacs-modeline)
(require 'prot-emacs-completion-common)
(pcase prot-emacs-completion-ui
  ('mct (require 'prot-emacs-completion-mct))
  ('vertico (require 'prot-emacs-completion-vertico)))
(require 'prot-emacs-search)
(require 'prot-emacs-dired)
(require 'prot-emacs-window)
(require 'prot-emacs-git)               ; git, diff, and related
(require 'prot-emacs-write)             ; denote, logos, etc.
(require 'prot-emacs-org)               ; org, calendar, appt
(require 'prot-emacs-langs)
(require 'prot-emacs-email)
(when (executable-find "notmuch")
  (require 'prot-emacs-email-notmuch))
(require 'prot-emacs-web)               ; eww, elfeed, rcirc
(require 'prot-emacs-conveniences)

(setq safe-local-variable-values
      '((org-hide-leading-stars . t)
        (org-hide-macro-markers . t)))

;; For those who use my dotfiles and need an easy way to write their
;; own extras on top of what I already load.  The file must exist at
;; ~/.emacs.d/user-emacs.el OR ~/.emacs.d/prot-emacs-post-custom.el
;;
;; The purpose of the "post customisations" is to make tweaks to what
;; I already define, such as to change the default theme.  See above
;; for the `prot-emacs-pre-custom.el' to make changes BEFORE loading
;; any of my other configurations.
(when-let* ((file-new (locate-user-emacs-file "prot-emacs-post-custom.el"))
            (file-old (locate-user-emacs-file "user-emacs.el"))
            (file (cond
                   ((file-exists-p file-new)
                    file-new)
                   ((file-exists-p file-old)
                    file-old))))
  (load-file file))

;; init.el ends here
