;;; prot-elfeed.el --- Elfeed extensions for my dotemacs -*- lexical-binding: t -*-

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
;; Extensions for Elfeed, intended for use in my Emacs setup:
;; https://protesilaos.com/dotemacs.

;;; Code:

(when (featurep 'elfeed)
  (require 'elfeed))

(defgroup prot-elfeed ()
  "Personal extensions for Elfeed."
  :group 'elfeed)

(defcustom prot-elfeed-feeds-file (concat user-emacs-directory "feeds.el.gpg")
  "Path to file with `elfeed-feeds'."
  :type 'string
  :group 'prot-elfeed)

(defcustom prot-elfeed-archives-directory "~/Documents/feeds/"
  "Path to directory for storing Elfeed entries."
  :type 'string
  :group 'prot-elfeed)

(defcustom prot-elfeed-tag-faces nil
  "Add faces for certain tags.
The tags are: critical, important, personal."
  :type 'boolean
  :group 'prot-elfeed)

(defcustom prot-elfeed-laptop-resolution-breakpoint 1366
  "Determine video resolution based on this display width.
This is used to check whether I am on the laptop or whether an
external display is attached to it.  In the latter case, a
`prot-elfeed-video-resolution-large' video resolution will be
used, else `prot-elfeed-video-resolution-small'."
  :type 'integer
  :group 'prot-elfeed)

(defcustom prot-elfeed-video-resolution-small 720
  "Set video resolution width for smaller displays."
  :type 'integer
  :group 'prot-elfeed)

(defcustom prot-elfeed-video-resolution-large 1080
  "Set video resolution width for larger displays."
  :type 'integer
  :group 'prot-elfeed)

(defface prot-elfeed-entry-critical
  '((((class color) (min-colors 88) (background light))
     :inherit elfeed-search-title-face :foreground "#972500")
    (((class color) (min-colors 88) (background dark))
     :inherit elfeed-search-title-face :foreground "#f4923b")
    (t :inherit (font-lock-builtin-face elfeed-search-title-face)))
  "Face for Elfeed entries tagged 'critical'.")

(defface prot-elfeed-entry-important
  '((((class color) (min-colors 88) (background light))
     :inherit elfeed-search-title-face :foreground "#315b00")
    (((class color) (min-colors 88) (background dark))
     :inherit elfeed-search-title-face :foreground "#70c900")
    (t :inherit (font-lock-string-face elfeed-search-title-face)))
  "Face for Elfeed entries tagged 'important'.")

(defface prot-elfeed-entry-personal
    '((((class color) (min-colors 88) (background light))
     :inherit elfeed-search-title-face :foreground "#8f0075")
    (((class color) (min-colors 88) (background dark))
     :inherit elfeed-search-title-face :foreground "#f78fe7")
    (t :inherit (font-lock-keyword-face elfeed-search-title-face)))
  "Face for Elfeed entries tagged 'personal'.")

;;;; Utilities

;;;###autoload
(defun prot-elfeed-load-feeds ()
  "Load file containing the `elfeed-feeds' list.
Add this to `elfeed-search-mode-hook'."
  (let ((feeds prot-elfeed-feeds-file))
    (if (file-exists-p feeds)
        (load-file feeds)
      (user-error "Missing feeds' file"))))

(defvar elfeed-search-face-alist)

;;;###autoload
(defun prot-elfeed-fontify-tags ()
  "Expand Elfeed faces if `prot-elfeed-tag-faces' is non-nil."
  (if prot-elfeed-tag-faces
      (setq elfeed-search-face-alist
            '((critical prot-elfeed-entry-critical)
              (important prot-elfeed-entry-important)
              (personal prot-elfeed-entry-personal)
              (unread elfeed-search-unread-title-face)))
    (setq elfeed-search-face-alist
          '((unread elfeed-search-unread-title-face)))))

(defvar prot-elfeed-search-tags
  '(critical essential important)
  "List of tags used by `prot-elfeed-toggle-tag'.")

(declare-function elfeed-search-toggle-all "elfeed")

;;;###autoload
(defun prot-elfeed-toggle-tag (&optional tag)
  "Toggle tag on current item.

A list of tags is provided by `prot-elfeed-search-tags'.
Otherwise an optional TAG symbol will suffice."
  (interactive)
  (let* ((tags prot-elfeed-search-tags)
         (input (or tag (intern (completing-read "Set tag: " tags nil t)))))
    (elfeed-search-toggle-all input)))

(defvar elfeed-show-entry)
(defvar elfeed-show-truncate-long-urls)
(declare-function elfeed-entry-title "elfeed")
(declare-function elfeed-show-refresh "elfeed")

;;;###autoload
(defun prot-elfeed-show-archive-entry ()
  "Store a plain text copy of the current `elfeed' entry.

The destination is defined in `prot-elfeed-archives-directory'
and will be created if it does not exist."
  (interactive)
  (let* ((entry (if (eq major-mode 'elfeed-show-mode)
                    elfeed-show-entry
                  (elfeed-search-selected :ignore-region)))
         (title (elfeed-entry-title entry))
         (elfeed-show-truncate-long-urls nil)
         (archives (file-name-as-directory prot-elfeed-archives-directory))
         (file (format "%s%s.txt" archives title)))
    (unless (file-exists-p archives)
      (make-directory archives t))
    (when (derived-mode-p 'elfeed-show-mode)
      ;; Refresh to expand truncated URLs
      (elfeed-show-refresh)
      (write-file file t)
      (message "Saved buffer at %s" file))))

;;;; General commands

(defvar elfeed-show-entry)
(declare-function elfeed-search-selected "elfeed")
(declare-function elfeed-entry-link "elfeed")

;;;###autoload
(defun prot-elfeed-show-eww (&optional link)
  "Browse current entry's link or optional LINK in `eww'.

Only show the readable part once the website loads.  This can
fail on poorly-designed websites."
  (interactive)
  (let* ((entry (if (eq major-mode 'elfeed-show-mode)
                    elfeed-show-entry
                  (elfeed-search-selected :ignore-region)))
         (link (or link (elfeed-entry-link entry))))
    (eww link)
    (add-hook 'eww-after-render-hook 'eww-readable nil t)))

(declare-function elfeed-search-untag-all-unread "elfeed")
(declare-function elfeed-search-show-entry "elfeed")

;;;###autoload
(defun prot-elfeed-search-open-other-window (&optional arg)
  "Browse `elfeed' entry in the other window.
With optional prefix ARG (\\[universal-argument]) browse the
entry in `eww' using the `prot-elfeed-show-eww' wrapper."
  (interactive "P")
  (let* ((entry (if (eq major-mode 'elfeed-show-mode)
                    elfeed-show-entry
                  (elfeed-search-selected :ignore-region)))
         (link (elfeed-entry-link entry))
         (win (selected-window)))
    (with-current-buffer (get-buffer "*elfeed-search*")
      (unless (one-window-p)              ; experimental
        (delete-other-windows win))
      (split-window win (/ (frame-height) 5) 'below)
      (other-window 1)
      (if arg
          (progn
            (when (eq major-mode 'elfeed-search-mode)
              (elfeed-search-untag-all-unread))
            (prot-elfeed-show-eww link))
        (elfeed-search-show-entry entry)))))

(declare-function elfeed-kill-buffer "elfeed")
(declare-function elfeed-search-quit-window "elfeed")

;;;###autoload
(defun prot-elfeed-kill-buffer-close-window-dwim ()
  "Do-what-I-mean way to handle `elfeed' windows and buffers.

When in an entry buffer, kill the buffer and return to the Elfeed
Search view.  If the entry is in its own window, delete it as
well.

When in the search view, close all other windows.  Else just kill
the buffer."
  (interactive)
  (let ((win (selected-window)))
    (cond ((eq major-mode 'elfeed-show-mode)
           (elfeed-kill-buffer)
           (unless (one-window-p) (delete-window win))
           (switch-to-buffer "*elfeed-search*"))
          ((eq major-mode 'elfeed-search-mode)
           (if (one-window-p)
               (elfeed-search-quit-window)
             (delete-other-windows win))))))

(defvar elfeed-search-filter-active)
(defvar elfeed-search-filter)
(declare-function elfeed-db-get-all-tags "elfeed")
(declare-function string-join "elfeed")
(declare-function elfeed-search-update "elfeed")
(declare-function elfeed-search-clear-filter "elfeed")

;;;###autoload
(defun prot-elfeed-search-tag-filter ()
  "Filter Elfeed search buffer by tags using completion.

Completion accepts multiple inputs, delimited by `crm-separator'.
Arbitrary input is also possible, but you may have to exit the
minibuffer with something like `exit-minibuffer'."
  (interactive)
  (unwind-protect
      (elfeed-search-clear-filter)
    (let* ((elfeed-search-filter-active :live)
           (db-tags (elfeed-db-get-all-tags))
           (plus-tags (mapcar (lambda (tag)
                                (format "+%s" tag))
                              db-tags))
           (minus-tags (mapcar (lambda (tag)
                                 (format "-%s" tag))
                               db-tags))
           (all-tags (delete-dups (append plus-tags minus-tags)))
           (tags (completing-read-multiple "Apply one or more tags: " all-tags nil t))
           (input (string-join `(,elfeed-search-filter ,@tags) " ")))
      (setq elfeed-search-filter input))
    (elfeed-search-update :force)))

;;;; Elfeed multimedia extras

(defvar prot-elfeed-mpv-buffer-name "*prot-elfeed-mpv-output*"
  "Name of buffer holding Elfeed MPV output.")

(defun prot-elfeed--video-resolution ()
  "Determine display resolution.
This checks `prot-elfeed-laptop-resolution-breakpoint'."
  (if (<= (display-pixel-width) prot-elfeed-laptop-resolution-breakpoint)
      prot-elfeed-video-resolution-small
    prot-elfeed-video-resolution-large))

(defun prot-elfeed--get-mpv-buffer ()
  "Prepare `prot-elfeed-mpv-buffer-name' buffer."
  (let ((buf (get-buffer prot-elfeed-mpv-buffer-name))
        (inhibit-read-only t))
    (with-current-buffer buf
      (erase-buffer))))

(declare-function elfeed-entry-enclosures "elfeed")

;;;###autoload
(defun prot-elfeed-mpv-dwim ()
  "Play entry link with the external MPV program.
When there is an audio enclosure (assumed to be a podcast), play
just the audio.  Else spawn a video player at a resolution that
accounts for the current monitor's width."
  (interactive)
  (let* ((entry (if (eq major-mode 'elfeed-show-mode)
                    elfeed-show-entry
                  (elfeed-search-selected :ignore-region)))
         (link (elfeed-entry-link entry))
         (enclosure (elt (car (elfeed-entry-enclosures entry)) 0)) ; fragile?
         (audio "--no-video")
         ;; Here the display width checks if I am on the laptop
         (height (prot-elfeed--video-resolution))
         (video                       ; this assumes mpv+youtube-dl
          (format "--ytdl-format=bestvideo[height\\<=?%s]+bestaudio/best" height))
         (buf (pop-to-buffer prot-elfeed-mpv-buffer-name)))
    (prot-elfeed--get-mpv-buffer)
    (if enclosure
        (progn
          (async-shell-command (format "mpv %s %s" audio enclosure) buf)
          (message "Launching MPV for %s" enclosure))
      (async-shell-command (format "mpv %s %s" video link) buf)
      (message "Launching MPV for %s" link))))

;;;; Elfeed and Bongo integration

;; XXX REVIEW XXX 2021-01-15: This section is a work in progress.

(defvar prot-elfeed-bongo-playlist "*prot-elfeed-bongo-queue*"
  "Name of the Elfeed+Bongo multimedia playlist.")

(declare-function bongo-playlist-buffer "elfeed")
(declare-function bongo-playlist-buffer-p "elfeed")
(declare-function bongo-playlist-mode "elfeed")
(declare-function bongo-progressive-playback-mode "elfeed")
(declare-function bongo-insert-uri "elfeed")
(declare-function bongo-insert-comment-text "elfeed")
(declare-function bongo-recenter "elfeed")

;;;###autoload
(defun prot-elfeed-bongo-insert-item ()
  "Insert `elfeed' multimedia links in `bongo' playlist buffer.

The playlist buffer has a unique name so that it will never
interfere with the default variable `bongo-playlist-buffer'."
  (interactive)
  (if (featurep 'bongo)
      (require 'bongo)
    (error "Bongo is not available"))
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
        (setq-local bongo-enabled-backends '(mpv))
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

(provide 'prot-elfeed)
;;; prot-elfeed.el ends here
