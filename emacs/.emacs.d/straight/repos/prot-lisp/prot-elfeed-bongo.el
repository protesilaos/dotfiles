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

;;; Code:

;; XXX Written on 2021-01-18.  Remains to be reviewed.

(eval-when-compile (require 'subr-x))
(when (featurep 'bongo)
  (require 'bongo))
(when (featurep 'elfeed)
  (require 'elfeed))

(defgroup prot-elfeed-bongo ()
  "Personal extensions for Bongo."
  :group 'bongo)

(defcustom prot-elfeed-bongo-playlist "*prot-elfeed-bongo-queue*"
  "Name of the Elfeed+Bongo multimedia playlist."
  :type 'string
  :group 'prot-elfeed-bongo)

(declare-function bongo-insert-comment-text "bongo")
(declare-function bongo-insert-uri "bongo")
(declare-function bongo-playlist-buffer "bongo")
(declare-function bongo-playlist-buffer-p "bongo")
(declare-function bongo-playlist-mode "bongo")
(declare-function bongo-progressive-playback-mode "bongo")
(declare-function bongo-recenter "bongo")
(declare-function define-bongo-backend "bongo")
(declare-function elfeed-entry-enclosures "elfeed")
(declare-function elfeed-entry-link "elfeed")
(declare-function elfeed-entry-title "elfeed")
(declare-function elfeed-search-selected "elfeed")
(defvar elfeed-show-entry)

(defvar prot-elfeed-bongo-mpv)

;; Here we define a slightly tweaked variant of the standard mpv
;; backend.  We will be using this to play back audio and video URLs.
;; The latter will spawn a new MPV player window.  Refer to my Elfeed
;; section for the implementation details.
(define-bongo-backend prot-elfeed-bongo-mpv
  ;; :constructor 'bongo-start-mpv-player
  :program-name 'mpv
  :extra-program-arguments nil
  :matcher '((local-file "file:" "http:" "ftp:")
             "ogg" "flac" "mp3" "mka" "wav" "wma"
             "mpg" "mpeg" "vob" "avi" "ogm" "mp4" "mkv"
             "mov" "asf" "wmv" "rm" "rmvb" "ts")
  :matcher '(("mms:" "mmst:" "rtp:" "rtsp:" "udp:" "unsv:"
              "dvd:" "vcd:" "tv:" "dvb:" "mf:" "cdda:" "cddb:"
              "cue:" "sdp:" "mpst:" "tivo:") . t)
  :matcher '(("http:" "https:") . t))

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
        (setq-local bongo-enabled-backends '(prot-elfeed-bongo-mpv))
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
