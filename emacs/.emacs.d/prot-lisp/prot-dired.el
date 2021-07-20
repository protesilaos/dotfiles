;;; prot-dired.el --- Extensions to dired.el for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2020-2021  Protesilaos Stavrou

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos.com/dotemacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

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
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'prot-common)

(defgroup prot-dired ()
  "Extensions for Dired."
  :group 'dired)

;;;; File associations

(defcustom prot-dired-media-extensions
  "\\.\\(mp[34]\\|ogg\\|flac\\|webm\\|mkv\\)"
  "Regular expression for media file extensions.

Also see the function `prot-dired-media-player' and the variable
`prot-dired-media-players'.

To be used in user configurations while setting up the variable
`dired-guess-shell-alist-user'."
  :type 'string
  :group 'prot-dired)

(defcustom prot-dired-image-extensions
  "\\.\\(png\\|jpe?g\\|tiff\\)"
  "Regular expression for media file extensions.

Also see the function `prot-dired-image-viewer' and the variable
`prot-dired-image-viewers'.

To be used in user configurations while setting up the variable
`dired-guess-shell-alist-user'."
  :type 'string
  :group 'prot-dired)

(defcustom prot-dired-media-players '("mpv" "vlc")
  "List of strings for media player programs.

Also see the function `prot-dired-media-player' and the variable
`prot-dired-media-extensions'.

To be used in user configurations while setting up the variable
`dired-guess-shell-alist-user'."
  :type '(repeat string)
  :group 'prot-dired)

(defcustom prot-dired-image-viewers '("feh" "sxiv")
  "List of strings for image viewer programs.

Also see the function `prot-dired-image-viewer' and the variable
`prot-dired-image-extensions'.

To be used in user configurations while setting up the variable
`dired-guess-shell-alist-user'."
  :type '(repeat string)
  :group 'prot-dired)

;; NOTE 2021-06-28: I am not sure why the compiler complains without
;; this, even though we require cl-lib.
(declare-function cl-remove-if "cl-lib")

(defmacro prot-dired-file-association (name programs)
  "Make NAME function to check for PROGRAMS."
  (declare (indent defun))
  `(defun ,name ()
     ,(format "Return available program.

This checks each entry in `%s' and returns the first program that
is available on the system.  If none is present, it falls back to
xdg-open (for GNU/Linux only).

This function is for use in `dired-guess-shell-alist-user'."
              programs)
     (let ((executables)
           (program-list (cl-remove-if nil ,programs)))
       (dolist (p program-list)
         (when (executable-find p)
           (push p executables)))
       (if executables
           (car (reverse executables))
         "xdg-open"))))

(prot-dired-file-association
  prot-dired-media-player
  prot-dired-media-players)

(prot-dired-file-association
  prot-dired-image-viewer
  prot-dired-image-viewers)

;;;; General commands

(autoload 'dired-mark-files-regexp "dired")
(autoload 'dired-toggle-marks "dired")
(autoload 'dired-do-kill-lines "dired-aux")

(defvar prot-dired--limit-hist '()
  "Minibuffer history for `prot-dired-limit-regexp'.")

;;;###autoload
(defun prot-dired-limit-regexp (regexp omit)
  "Limit Dired to keep files matching REGEXP.

With optional OMIT argument as a prefix (\\[universal-argument]),
exclude files matching REGEXP.

Restore the buffer with \\<dired-mode-map>`\\[revert-buffer]'."
  (interactive
   (list
    (read-regexp "Files matching PATTERN: " nil 'prot-dired--limit-hist)
    current-prefix-arg))
  (dired-mark-files-regexp regexp)
  (unless omit (dired-toggle-marks))
  (dired-do-kill-lines)
  (add-to-history 'prot-dired--limit-hist regexp))

;;;; Subdir extras and Imenu setup

(defvar prot-dired--directory-header-regexp "^ +\\(.+\\):\n"
  "Pattern to match Dired directory headings.")

;;;###autoload
(defun prot-dired-subdirectory-next (&optional arg)
  "Move to next or optional ARGth Dired subdirectory heading.
For more on such headings, read `dired-maybe-insert-subdir'."
  (interactive "p")
  (let ((pos (point))
        (subdir prot-dired--directory-header-regexp))
    (goto-char (point-at-eol))
    (if (re-search-forward subdir nil t (or arg nil))
        (progn
          (goto-char (match-beginning 1))
          (goto-char (point-at-bol)))
      (goto-char pos))))

;;;###autoload
(defun prot-dired-subdirectory-previous (&optional arg)
  "Move to previous or optional ARGth Dired subdirectory heading.
For more on such headings, read `dired-maybe-insert-subdir'."
  (interactive "p")
  (let ((pos (point))
        (subdir prot-dired--directory-header-regexp))
    (goto-char (point-at-bol))
    (if (re-search-backward subdir nil t (or arg nil))
        (goto-char (point-at-bol))
      (goto-char pos))))

(autoload 'dired-get-filename "dired")
(autoload 'dired-maybe-insert-subdir "dired-aux")
(defvar dired-subdir-switches)
(defvar dired-actual-switches)

;;;###autoload
(defun prot-dired-insert-subdir (subdir &optional switches)
  "Insert SUBDIR in current Dired buffer.
With optional SWITCHES, prompt for the ls switches to use."
  (interactive
   (list
    (or (dired-get-filename 'verbatim t)
        (read-directory-name "Insert directory: "))
    (when current-prefix-arg
	  (read-string "Switches for listing: "
			       (or dired-subdir-switches dired-actual-switches)))))
  (dired-maybe-insert-subdir (expand-file-name subdir) (or switches nil) t))

(defun prot-dired-imenu-prev-index-position ()
  "Find the previous file in the buffer."
  (let ((subdir prot-dired--directory-header-regexp))
    (re-search-backward subdir nil t)))

(defun prot-dired-imenu-extract-index-name ()
  "Return the name of the file at point."
  (buffer-substring-no-properties (+ (point-at-bol) 3) (1- (point-at-eol))))

;;;###autoload
(defun prot-dired-setup-imenu ()
  "Configure imenu for the current dired buffer.
Add this to `dired-mode-hook'."
  (set (make-local-variable 'imenu-prev-index-position-function)
       'prot-dired-imenu-prev-index-position)
  (set (make-local-variable 'imenu-extract-index-name-function)
       'prot-dired-imenu-extract-index-name))

(provide 'prot-dired)
;;; prot-dired.el ends here
