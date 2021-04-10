;;; prot-elfeed-bongo.el --- Bongo+Elfeed integration for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2021  Protesilaos Stavrou

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
;; Extensions for integrating Elfeed with Bongo, intended for use in my
;; Emacs setup: https://protesilaos.com/dotemacs.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

;; XXX Written on 2021-01-18.  Remains to be reviewed.

(eval-when-compile (require 'subr-x))
(require 'bongo nil t)
(require 'elfeed nil t)

(defgroup prot-elfeed-bongo ()
  "Personal extensions for Bongo."
  :group 'bongo)

(defcustom prot-elfeed-bongo-playlist "*prot-elfeed-bongo-queue*"
  "Name of the Elfeed+Bongo multimedia playlist."
  :type 'string
  :group 'prot-elfeed-bongo)

(autoload 'bongo-insert-comment-text "bongo")
(autoload 'bongo-insert-uri "bongo")
(autoload 'bongo-playlist-buffer "bongo")
(autoload 'bongo-playlist-buffer-p "bongo")
(autoload 'bongo-playlist-mode "bongo")
(autoload 'bongo-progressive-playback-mode "bongo")
(autoload 'bongo-recenter "bongo")
(autoload 'elfeed-entry-enclosures "elfeed")
(autoload 'elfeed-entry-link "elfeed")
(autoload 'elfeed-entry-title "elfeed")
(autoload 'elfeed-search-selected "elfeed")
(defvar elfeed-show-entry)

;;;###autoload
(defun prot-elfeed-bongo-insert-item ()
  "Insert `elfeed' multimedia links in `bongo' playlist buffer.

The playlist buffer has a unique name so that it will never
interfere with the default variable `bongo-playlist-buffer'."
  (interactive)
  (let* ((entry (if (eq major-mode 'elfeed-show-mode)
                    elfeed-show-entry
                  (elfeed-search-selected :ignore-region)))
         (link (elfeed-entry-link entry))
         (enclosure (elt (car (elfeed-entry-enclosures entry)) 0))
         (url (or enclosure link))
         (title (elfeed-entry-title entry))
         (bongo-pl prot-elfeed-bongo-playlist)
         (buffer (get-buffer-create bongo-pl)))
    (unless (bongo-playlist-buffer)
      (bongo-playlist-buffer))
    (display-buffer buffer)
    (with-current-buffer buffer
 	  (when (not (bongo-playlist-buffer-p))
 	    (bongo-playlist-mode)
        (setq-local bongo-library-buffer (get-buffer "*elfeed-search*"))
        (setq-local bongo-enabled-backends '(vlc mpv))
        (bongo-progressive-playback-mode))
 	  (goto-char (point-max))
      (bongo-insert-uri url title)
      (bongo-insert-comment-text (format "     ==> %s\n" url))
      (let ((inhibit-read-only t))
        (delete-duplicate-lines (point-min) (point-max)))
      (bongo-recenter))
    (message "Enqueued %s “%s” in %s"
             (if enclosure "podcast" "video")
             (propertize title 'face 'italic)
             (propertize bongo-pl 'face 'bold))))

(defun prot-elfeed-bongo-switch-to-playlist ()
  "Switch to `prot-elfeed-bongo-playlist'."
  (interactive)
  (let* ((bongo-pl prot-elfeed-bongo-playlist)
         (buffer (get-buffer bongo-pl)))
    (if buffer
        (switch-to-buffer buffer)
      (message "No `bongo' playlist is associated with `elfeed'."))))

(provide 'prot-elfeed-bongo)
;;; prot-elfeed-bongo.el ends here
