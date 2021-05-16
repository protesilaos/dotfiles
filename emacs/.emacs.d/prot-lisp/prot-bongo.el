;;; prot-bongo.el --- Bongo extensions for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2021  Protesilaos Stavrou

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
;; Extensions for Bongo, intended for use in my Emacs setup:
;; https://protesilaos.com/dotemacs.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

;; XXX Written on 2021-01-18.  Remains to be reviewed.

(eval-when-compile (require 'subr-x))
(require 'bongo nil t)
(require 'prot-common)

(defgroup prot-bongo ()
  "Personal extensions for Bongo."
  :group 'bongo)

(defcustom prot-bongo-enabled-backends '(mpv vlc)
  "List of enabled backends.
See `bongo-backends' for a list of available backends."
  :type 'list
  :group 'prot-bongo)

(defcustom prot-bongo-playlist-section-delimiter (make-string 30 ?*)
  "Delimiter for inserted groups in Bongo playlist buffers.
It is recommended to set this to a few character length, as it
should be placed on its own line to demacrate groups of enqueued
media."
  :type 'string
  :group 'prot-bongo)

(defcustom prot-bongo-playlist-heading-delimiter "§"
  "Delimiter for custom headings in Bongo playlist buffers.
It is recommended to set this to a single character, as it will
be complemented with the name of the enqueued item."
  :type 'string
  :group 'prot-bongo)

(defvar bongo-default-directory)

(defcustom prot-bongo-playlist-directory
  (concat
   (file-name-as-directory bongo-default-directory)
   (file-name-as-directory "playlists"))
  "Path to playlist files.
Such files are plain text documents that contain a filesystem
path on each line which points to a multimedia item (e.g. a
directory with music files).

Make sure this is a valid path, as we will not make any attempt
at creating it or running any other kind of check."
  :type 'string
  :group 'prot-bongo)

;;;; Basic setup

(defvar bongo-enabled-backends)

;;;###autoload
(defun prot-bongo-enabled-backends (&optional negation)
  "Assign variable `prot-bongo-enabled-backends' to Bongo.
With optional NEGATION, undo this assignment."
  (if negation
      (progn
        (setq bongo-enabled-backends nil)
        (remove-hook 'bongo-mode-hook #'prot-bongo-enabled-backends))
    (setq bongo-enabled-backends prot-bongo-enabled-backends)
    (add-hook 'bongo-mode-hook #'prot-bongo-enabled-backends)))

;; The original idea for the advice setup to hide the Bongo comment
;; headers comes from the Emacs configuration of Nicolas De Jaeghere:
;; <https://github.com/Koekelas/dotfiles/blob/master/emacs.org>.

(defvar bongo-default-playlist-buffer-name)
(defvar bongo-default-library-buffer-name)
(declare-function bongo-playlist-mode "bongo")
(declare-function bongo-library-mode "bongo")

(defun prot-bongo-playlist-buffer-no-banner ()
  "Set up a Bongo playlist buffer without its header commentary.
To be advised as override for `bongo-default-playlist-buffer'.

To actually enable this, evaluate `prot-bongo-remove-headers'."
  (with-current-buffer (get-buffer-create bongo-default-playlist-buffer-name)
    (unless (derived-mode-p 'bongo-playlist-mode)
      (bongo-playlist-mode))
    (current-buffer)))

(defun prot-bongo-library-buffer-no-banner ()
  "Set up a Bongo library buffer without its header commentary.
To be advised as override for `bongo-default-library-buffer'.

To actually enable this, evaluate `prot-bongo-remove-headers'."
  (with-current-buffer (get-buffer-create bongo-default-library-buffer-name)
    (unless (derived-mode-p 'bongo-library-mode)
      (bongo-library-mode))
    (current-buffer)))

;;;###autoload
(defun prot-bongo-remove-headers (&optional negation)
  "Remove comment headers from Bongo buffers.
With optional NEGATION undo the changes."
  (if negation
      (progn
        (advice-remove 'bongo-default-playlist-buffer #'prot-bongo-playlist-buffer-no-banner)
        (advice-remove 'bongo-default-library-buffer #'prot-bongo-library-buffer-no-banner))
    (advice-add 'bongo-default-playlist-buffer :override #'prot-bongo-playlist-buffer-no-banner)
    (advice-add 'bongo-default-library-buffer :override #'prot-bongo-library-buffer-no-banner)))

;;;; Custom delimiters for headings and sections

(declare-function bongo-insert-comment-text "bongo")

(defun prot-bongo-playlist-heading (title &optional description)
  "Insert `bongo' comment with TITLE and DESCRIPTION.
Use this to add a custom heading for the enqueued media items."
  (bongo-insert-comment-text
   (format "%s %s%s\n"
           prot-bongo-playlist-heading-delimiter
           title
           (if description (concat " " description) ""))))

(defun prot-bongo-playlist-section ()
  "Make `prot-bongo-playlist-section-delimiter' comment."
  (bongo-insert-comment-text
   (format "\n%s\n\n" prot-bongo-playlist-section-delimiter)))

;;;; Motions and actions for custom sections

;; REVIEW: there probably is a better way to parametrise move-buf and
;; move-point so that one key checks for appropriate forward or backward
;; motions, but this is okay right now.
(defmacro prot-bongo-playlist-motion (fn desc rx move-buf move-point)
  "Produce interactive commands to navigate custom bongo delimiters.

FN is the resulting interactive function's name.  DESC is its doc
string.  RX is the regular expression that matches the custom
bongo playlist delimiter (see `prot-bongo-playlist-delimiter' and
`prot-bongo-playlist-heading').

MOVE-BUF is a motion across an arbitrary number of lines.
Currently it assumes (though does test) either
`re-search-forward' or `re-search-backward'.  Likewise,
MOVE-POINT expects `point-at-eol' or `point-at-bol'.  These
motions should go in pairs, in the order they are presented here."
  (declare (indent defun))
  `(defun ,fn ()
     ,desc
     (interactive)
     (let ((section ,rx))
       (when (save-excursion (funcall ,move-buf section nil t))
         (goto-char (funcall ,move-point))
         (funcall ,move-buf section nil t)))))

(prot-bongo-playlist-motion
  prot-bongo-playlist-heading-next
  "Move to next `bongo' playlist custom heading."
  (format "^.*%s.*$" prot-bongo-playlist-heading-delimiter)
  're-search-forward
  'point-at-eol)

(prot-bongo-playlist-motion
  prot-bongo-playlist-heading-previous
  "Move to previous `bongo' playlist custom heading."
  (format "^.*%s.*$" prot-bongo-playlist-heading-delimiter)
  're-search-backward
  'point-at-bol)

(defun prot-bongo--section-delimiter-string ()
  "Format regexp for `prot-bongo-playlist-section-delimiter'."
  (let* ((string prot-bongo-playlist-section-delimiter)
         (char (regexp-quote (substring string 0 1))))
    (format "^%s+$" char)))

(prot-bongo-playlist-motion
  prot-bongo-playlist-section-next
  "Move to next `bongo' playlist custom section delimiter."
  (prot-bongo--section-delimiter-string)
  're-search-forward
  'point-at-eol)

(prot-bongo-playlist-motion
  prot-bongo-playlist-section-previous
  "Move to previous `bongo' playlist custom section delimiter."
  (prot-bongo--section-delimiter-string)
  're-search-backward
  'point-at-bol)

;;;###autoload
(defun prot-bongo-playlist-mark-section ()
  "Mark `bongo' playlist section, delimited by custom markers.
The marker is `prot-bongo-playlist-delimiter'."
  (interactive)
  (let ((section (prot-bongo--section-delimiter-string)))
    (search-forward-regexp section nil t)
    (push-mark nil t)
    (forward-line -1)
    ;; REVIEW any predicate to replace this `save-excursion'?
    (if (save-excursion (re-search-backward section nil t))
        (progn
          (search-backward-regexp section nil t)
          (forward-line 1))
      (goto-char (point-min)))
    (activate-mark)))

(declare-function bongo-kill "bongo")

;;;###autoload
(defun prot-bongo-playlist-kill-section ()
  "Kill `bongo' playlist-section at point.
This operates on a custom delimited section of the buffer.  See
`prot-bongo-playlist-kill-section'."
  (interactive)
  (prot-bongo-playlist-mark-section)
  (bongo-kill))

;;;; Imenu setup for custom sections

(defvar prot-bongo-playlist-setup-hook nil
  "Hook that runs after inserting items to the Bongo playlist.
See, for example, `prot/bongo-playlist-insert-playlist-file' or
`prot/bongo-dired-insert-files'.")

(defun prot-bongo--playlist-imenu-heading ()
  "Return the text of the custom `bongo' playlist heading."
  (let* ((string prot-bongo-playlist-heading-delimiter)
         (char (substring string 0 1)))
    (nth 1
         (split-string
          (buffer-substring-no-properties (point-at-bol) (point-at-eol))
          (concat char " ")))))

;;;###autoload
(defun prot-bongo-imenu-setup (&optional negation)
  "Set up `imenu' bindings for the Bongo playlist buffer.
With optional NEGATION, remove them."
  (if negation
      (progn
        (dolist (local '(imenu-prev-index-position-function
                         imenu-extract-index-name-function))
          (kill-local-variable local))
        (remove-hook 'prot-bongo-playlist-setup-hook #'prot-bongo-imenu-setup))
    (add-hook 'prot-bongo-playlist-setup-hook #'prot-bongo-imenu-setup)
    (setq-local imenu-prev-index-position-function
                'prot-bongo-playlist-heading-previous)
    (setq-local imenu-extract-index-name-function
                'prot-bongo--playlist-imenu-heading)))

;;;; Commands

(declare-function bongo-erase-buffer "bongo")
(declare-function bongo-library-buffer-p "bongo")
(declare-function bongo-play-random "bongo")
(declare-function bongo-playing-p "bongo")
(declare-function bongo-playlist-buffer "bongo")
(declare-function bongo-playlist-buffer-p "bongo")
(declare-function bongo-progressive-playback-mode "bongo")
(declare-function bongo-random-playback-mode "bongo")
(declare-function bongo-recenter "bongo")
(declare-function bongo-reset-playlist "bongo")
(declare-function bongo-stop "bongo")

;;;###autoload
(defun prot-bongo-playlist-play-random ()
  "Play random `bongo' track and determine further conditions."
  (interactive)
  (unless (bongo-playlist-buffer)
    (bongo-playlist-buffer))
  (when (or (bongo-playlist-buffer-p)
            (bongo-library-buffer-p))
    (unless (bongo-playing-p)
      (with-current-buffer (bongo-playlist-buffer)
        (bongo-play-random)
        (bongo-random-playback-mode)
        (bongo-recenter)))))

(defvar bongo-next-action)

;;;###autoload
(defun prot-bongo-playlist-random-toggle ()
  "Toggle `bongo-random-playback-mode' in playlist buffers."
  (interactive)
  (if (eq bongo-next-action 'bongo-play-random-or-stop)
      (bongo-progressive-playback-mode)
    (bongo-random-playback-mode)))

;;;###autoload
(defun prot-bongo-playlist-reset ()
  "Stop playback and reset Bongo playlist.
To reset the playlist is to undo the marks produced by non-nil
`bongo-mark-played-tracks'."
  (interactive)
  (when (bongo-playlist-buffer-p)
    (bongo-stop)
    (bongo-reset-playlist)))

;;;###autoload
(defun prot-bongo-playlist-terminate ()
  "Stop playback and clear the entire `bongo' playlist buffer.
Contrary to the standard `bongo-erase-buffer', this also removes
the currently playing track."
  (interactive)
  (when (bongo-playlist-buffer-p)
    (bongo-stop)
    (bongo-erase-buffer)))

(defvar prot-bongo--playlist-history '()
  "Input history of `prot-bongo-playlist-insert-playlist-file'.")

(defun prot-bongo--playlist-prompt ()
  "Prompt for a file in `prot-bongo-playlist-directory'.
Helper function for `prot-bongo-playlist-insert-playlist-file'."
  (let* ((path prot-bongo-playlist-directory)
         (dotless directory-files-no-dot-files-regexp)
         (playlists (mapc
                       'abbreviate-file-name
                       (directory-files path nil dotless))))
    (completing-read-multiple
     "Add playlist: " playlists
     #'prot-common-crm-exclude-selected-p
     t nil 'prot-bongo--playlist-history)))

(declare-function bongo-insert-playlist-contents "bongo")

;;;###autoload
(defun prot-bongo-playlist-insert-playlist-file ()
  "Insert contents of playlist file to a `bongo' playlist.
Upon insertion, playback starts immediately, in accordance with
`prot-bongo-play-random'.

The available options at the completion prompt are pre-configured
files that contain absolute filesystem paths of directories or
media items one per line.  Think of them as meta-directories that
mix manually selected media items (yes, I never liked 'smart'
playlists).

To insert multiple playlists complete the first, then type a
character that matches `crm-separator' to complete the second,
and so on.

Also see `prot-bongo-dired-make-playlist-file'."
  (interactive)
  (let ((path prot-bongo-playlist-directory))
    (unless (file-directory-p path)
      (error "'%s' is not an existing directory" path))
    (let ((choice
           (if (not (bongo-playlist-buffer-p (current-buffer)))
               (user-error "Not in a `bongo' playlist buffer")
             (prot-bongo--playlist-prompt))))
      (mapc (lambda (x)
              (save-excursion
                (goto-char (point-max))
                (prot-bongo-playlist-heading x "playlist file")
                (bongo-insert-playlist-contents
                 (format "%s%s" path x))
                (prot-bongo-playlist-section)))
            choice)
      (prot-bongo-playlist-play-random)
      (run-hooks 'prot-bongo-playlist-setup-hook))))

;;;; Setup for track changing

(defvar prot-bongo-playlist-change-track-hook nil
  "Hook that runs after `bongo' switches to a new track.")

(defun prot-bongo-playlist-run-hook-change-track (&rest _)
  "Run `prot-bongo-playlist-run-hook-change-track'.
This is meant to be loaded after the relevant `bongo' functions
that change tracks, such as `bongo-play-next-or-stop' and
`bongo-play-random-or-stop'."
  (run-hooks 'prot-bongo-playlist-change-track-hook))

(dolist (fn '(bongo-play-next-or-stop bongo-play-random-or-stop))
  (advice-add fn :after #'prot-bongo-playlist-run-hook-change-track))

;;;###autoload
(defun prot-bongo-playlist-recenter ()
  "Recenter `bongo' playlist buffer while in a live window.
Add to `prot-bongo-playlist-change-track-hook'."
  (with-current-buffer (bongo-playlist-buffer)
    (bongo-recenter)))

;;;; Bongo + Dired (bongo library buffer)

(declare-function bongo-dired-library-mode "bongo")
(declare-function bongo-insert-directory-tree "bongo")
(declare-function bongo-insert-file "bongo")
(declare-function bongo-library-buffer "bongo")
(autoload 'dired-filename-at-point "bongo")
(autoload 'dired-get-marked-files "bongo")
(autoload 'dired-next-line "bongo")

(defmacro prot-bongo-dired-library (name doc val)
  "Create Bongo library function NAME with DOC and VAL."
  (declare (indent defun))
  `(defun ,name ()
     ,doc
     (when (string-match-p (file-truename bongo-default-directory)
                           (file-truename default-directory))
       (bongo-dired-library-mode ,val))))

(prot-bongo-dired-library
  prot-bongo-dired-library-enable
  "Set `bongo-dired-library-mode' when accessing ~/Music.

Add this to `dired-mode-hook'.  Upon activation, the directory
and all its sub-directories become a valid library buffer for
Bongo, from where we can, among others, add tracks to playlists.
The added benefit is that Dired will continue to behave as
normal, making this a superior alternative to a purpose-specific
library buffer.

Note, though, that this will interfere with `wdired-mode'.  See
`prot-bongo-dired-library-disable'."
  1)

(prot-bongo-dired-library
  prot-bongo-dired-library-disable
  "Disable `bongo-dired-library-mode' when accessing ~/Music.
This should be added `wdired-mode-hook'.  For more, refer to
`prot-bongo-dired-library-enable'."
  -1)

(advice-add 'wdired-finish-edit :after #'prot-bongo-dired-library-enable)

(defun prot-bongo--dired-insert-files ()
  "Add files in a `dired' buffer to the `bongo' playlist."
  (let ((media (or (dired-get-marked-files) (dired-x-guess-file-name-at-point)))) ; emacs28
    (with-current-buffer (bongo-playlist-buffer)
      (goto-char (point-max))
      (mapc (lambda (x)
              (if (file-directory-p x)
                  (progn
                    (prot-bongo-playlist-heading (file-name-base x))
                    (bongo-insert-directory-tree x))
                (bongo-insert-file x)))
            media)
      (prot-bongo-playlist-section)
      (run-hooks 'prot-bongo-playlist-setup-hook))
    (with-current-buffer (bongo-library-buffer)
      (dired-next-line 1))))

;;;###autoload
(defun prot-bongo-dired-insert ()
  "Add `dired' item at point or marked ones to Bongo playlist.

The playlist buffer is created, if necessary, while some other
tweaks are introduced.  See `prot-bongo--dired-insert-files' as
well as `prot-bongo-playlist-play-random'.

Meant to work while inside a `dired' buffer that doubles as a
library buffer (see `prot-bongo-dired-library-enable')."
  (interactive)
  (when (bongo-library-buffer-p)
    (unless (bongo-playlist-buffer-p)
      (bongo-playlist-buffer))
    (prot-bongo--dired-insert-files)
    (prot-bongo-playlist-play-random)))

;;;###autoload
(defun prot-bongo-dired-make-playlist-file ()
  "Add `dired' marked items to playlist file using completion.

Files are stored in `prot-bongo-playlist-directory'.  These are
meant to reference filesystem paths: one path per line.  They
ease the task of playing media from closely related directory
trees, without having to interfere with the user's directory
structure (e.g. a playlist file 'rock' can include the paths of
~/Music/Scorpions and ~/Music/Queen).

This works by appending the absolute filesystem path of each item
to the selected playlist file.  If no Dired marked items are
available, the item at point will be used instead.

Selecting a non-existent file at the prompt will create a new
entry whose name matches the minibuffer input.

Also see `prot-bongo-playlist-insert-playlist-file'."
  (interactive)
  (let* ((dotless directory-files-no-dot-files-regexp)
         (pldir prot-bongo-playlist-directory)
         (playlists (mapcar
                     'abbreviate-file-name
                     (directory-files pldir nil dotless)))
         (plname (completing-read "Select playlist: " playlists nil))
         (plfile (concat pldir plname))
         (media-paths
          (if (derived-mode-p 'dired-mode)
              ;; TODO more efficient way to do ensure newline ending?
              ;;
              ;; The issue is that we need to have a newline at the
              ;; end of the file, so that when we append again we
              ;; start on an empty line.
              (concat
               (mapconcat #'identity
                          (dired-get-marked-files)
                          "\n")
               "\n")
            (user-error "Not in a `dired' buffer"))))
    ;; The following `when' just checks for an empty string.  If we
    ;; wanted to make this more robust we should also check for names
    ;; that contain only spaces and/or invalid characters…  This is
    ;; good enough for me.
    (when (string-empty-p plname)
      (user-error "No playlist file has been specified"))
    (unless (file-directory-p pldir)
      (make-directory pldir))
    (unless (and (file-exists-p plfile)
                 (file-readable-p plfile)
                 (not (file-directory-p plfile)))
      (make-empty-file plfile))
    (append-to-file media-paths nil plfile)
    (with-current-buffer (find-file-noselect plfile)
      (delete-duplicate-lines (point-min) (point-max))
      (sort-lines nil (point-min) (point-max))
      (save-buffer)
      (kill-buffer))))

(provide 'prot-bongo)
;;; prot-bongo.el ends here
