;;; prot-dired.el --- Extensions to dired.el for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2020-2021  Protesilaos Stavrou

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos.com/dotemacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This covers my dired.el extensions, for use in my Emacs setup:
;; https://protesilaos.com/dotemacs.

;;; Code:

(require 'prot-common)

;; NOTE 2021-01-04: This library is deprecated and superseded by
;; `prot-consult.el'.

(defun prot-dired--expand-root-dir ()
  "Expand file name of project or current directory."
  (expand-file-name (or (vc-root-dir)
                        (locate-dominating-file "." ".git")
                        default-directory)))

(defmacro prot-dired-fd-command (name doc prompt &rest flags)
  "Make commands for selecting 'fd' results with completion.
NAME is how the function should be named.  DOC is the function's
documentation string.  PROMPT describes the scope of the query.
FLAGS are the command line arguments passed to the 'fd'
executable, each of which is a string."
  `(defun ,name (&optional arg)
     ,doc
     (interactive "P")
     (if (executable-find "fd")
         (let* ((dir (prot-dired--expand-root-dir))
                (regexp (read-regexp
                         (format "%s matching REGEXP in %s: " ,prompt
                                 (propertize dir 'face 'bold))))
                (names (prot-common-completion-table
                        'file
                        (process-lines "fd" ,@flags regexp dir)))
                (buf "*FD Dired*"))
           (if names
             (if arg
                 (dired (cons (generate-new-buffer-name buf) names))
               (find-file
                (completing-read (format "Items matching %s: "
                                         (propertize regexp 'face 'success))
                                 names nil t)))
             (message "No match for %s" regexp)))
     (error "<< fd >> executable not found"))))

(prot-dired-fd-command
 prot-dired-fd-dirs
 "Search for directories in VC root or PWD.
With optional prefix argument (\\[universal-argument]) put the
results in a `dired' buffer.  This relies on the external 'fd'
executable."
 "Subdirectories"
 "-i" "-H" "-a" "-t" "d" "-c" "never")

(prot-dired-fd-command
 prot-dired-fd-files-and-dirs
 "Search for files and directories in VC root or PWD.
With optional prefix argument (\\[universal-argument]) put the
results in a `dired' buffer.  This relies on the external 'fd'
executable."
 "Files and dirs"
 "-i" "-H" "-a" "-t" "d" "-t" "f" "-c" "never")

(provide 'prot-dired)
;;; prot-dired.el ends here
