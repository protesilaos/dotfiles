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

;; See my dotfiles: <https://git.sr.ht/~protesilaos/dotfiles>

;;; Code:

;; Some basic settings
(setq frame-title-format '("%b"))
(setq default-input-method "greek")
(setq ring-bell-function 'ignore)
(setq use-short-answers t)

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

;; I want to use my own packages from specific repositories.  All others
;; will rely on `package-archive-priorities'.
;;
;; Note that the `modus-themes' are built into Emacs 28 and are synced
;; to GNU ELPA from emacs.git.  As I already run Emacs from source, I am
;; using MELPA for the `modus-themes' here: it is for testing purposes.
(setq package-pinned-packages
      '((cursory . "elpa-devel")
        (denote . "elpa-devel")
        (fontaine . "elpa-devel")
        (lin . "elpa-devel")
        (logos . "elpa-devel")
        (modus-themes . "melpa")
        (pulsar . "elpa-devel")
        (tmr . "elpa-devel")))

(defmacro prot-emacs-builtin-package (package &rest body)
  "Set up builtin PACKAGE with rest BODY.
PACKAGE is a quoted symbol, while BODY consists of balanced
expressions."
  (declare (indent 1))
  `(progn
     (unless (require ,package nil 'noerror)
       (display-warning 'prot-emacs
                        (format "Loading `%s' failed" ,package)
                        :warning))
     ,@body))

(defmacro prot-emacs-elpa-package (package &rest body)
  "Set up PACKAGE from an Elisp archive with rest BODY.
PACKAGE is a quoted symbol, while BODY consists of balanced
expressions.

Try to install the package if it is missing."
  (declare (indent 1))
  `(progn
     (when (not (package-installed-p ,package))
       (unless package-archive-contents
         (package-refresh-contents))
       (package-install ,package))
     (if (require ,package nil 'noerror)
         (progn ,@body)
       (display-warning 'prot-emacs
                        (format "Loading `%s' failed" ,package)
                        :warning))))

(require 'prot-emacs-essentials)
(require 'prot-emacs-theme)
(require 'prot-emacs-theme-extras)
(require 'prot-emacs-font)
(require 'prot-emacs-modeline)
(require 'prot-emacs-completion)
(require 'prot-emacs-search)
(require 'prot-emacs-dired)             ; dired and ibuffer
(require 'prot-emacs-window)
(require 'prot-emacs-git)               ; git, diff, and related
(require 'prot-emacs-shell)             ; e?shell, man, proced, pass
(require 'prot-emacs-write)             ; denote, logos, etc.
(require 'prot-emacs-org)               ; org, calendar, appt
(require 'prot-emacs-langs)
(require 'prot-emacs-email)
(require 'prot-emacs-web)               ; eww, elfeed, rcirc
(require 'prot-emacs-conveniences)
(require 'prot-emacs-history)

;; For those who use my dotfiles and need an easy way to write their own
;; extras.  The file must exist at ~/.emacs.d/user-emacs.el
(when-let* ((file (locate-user-emacs-file "user-emacs.el"))
            ((file-exists-p file)))
  (load-file file))

;; init.el ends here
