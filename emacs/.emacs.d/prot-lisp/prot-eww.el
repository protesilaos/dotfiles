;;; prot-eww.el --- Extensions for EWW -*- lexical-binding: t -*-

;; Copyright (C) 2021-2025  Protesilaos Stavrou, Abhiseck Paira

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos.com/emacs/dotemacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
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
;; Extensions for the eww, intended for my Emacs setup:
;; <https://protesilaos.com/emacs/dotemacs/>.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

(require 'eww)
(require 'prot-common)

(defgroup prot-eww nil
  "Tweaks for EWW."
  :group 'eww)

(defun prot-eww--get-urls ()
  "Get all links in the current buffer."
  (let ((links nil))
    (save-excursion
      (goto-char (point-min))
      (while (text-property-search-forward 'face 'shr-link)
        (when-let* ((position (point))
                    (button (button-at position)))
          (push
           (list position
                 (button-label button)
                 (shr-url-at-point nil))
           links))))
    (nreverse links)))

(defun prot-eww-buffer-url-prompt ()
  "Prompt for a url in the current buffer."
  (when-let* ((link-data (prot-eww--get-urls))
              (candidates (mapcar
                           (pcase-lambda (`(,position ,name ,_))
                             (format "%s	%s" position name))
                           link-data))
              (table (prot-common-completion-table-no-sort nil candidates))
              (selection
               (completing-read
                (format-prompt "Select link in the current page" nil)
                table))
              (position (car (split-string selection "\t")))
              (number (string-to-number position)))
    (assoc number link-data)))

(defun prot-eww-visit-url-on-page (&optional new-buffer)
  "Visit URL among those in the current buffer using completion.
With optional NEW-BUFFER as a prefix argument, visit the URL in a new
buffer instead of the current one."
  (interactive "P" eww-mode)
  (unless (derived-mode-p 'eww-mode)
    (user-error "This command only works in an EWW buffer"))
  (if-let* ((data (prot-eww-buffer-url-prompt))
            (url (nth 2 data)))
      (eww url new-buffer)
    (error "Cannot find URL in data `%s'" data)))

(defun prot-eww-jump-to-url-on-page ()
  "Go the position of a URL among those in the current buffer."
  (interactive nil eww-mode)
  (unless (derived-mode-p 'eww-mode)
    (user-error "This command only works in an EWW buffer"))
  (if-let* ((data (prot-eww-buffer-url-prompt))
            (position (car data)))
      (goto-char position)
    (error "Cannot position in data `%s'" data)))

(defvar prot-eww--occur-feed-regexp
  (concat "\\(rss\\|atom\\)\\+xml.\\(.\\|\n\\)"
          ".*href=[\"']\\(.*?\\)[\"']")
  "Regular expression to match web feeds in HTML source.")

(defun prot-eww-find-feed ()
  "Produce bespoke buffer with RSS/Atom links from XML source."
  (interactive nil eww-mode)
  (unless (derived-mode-p 'eww-mode)
    (user-error "This command only works in an EWW buffer"))
  (let* ((url (or (plist-get eww-data :start)
                  (plist-get eww-data :contents)
                  (plist-get eww-data :home)
                  (plist-get eww-data :url)))
         (title (or (plist-get eww-data :title) url))
         (source (plist-get eww-data :source))
         (buf-name (format "*feeds: %s # eww*" title)))
    (with-temp-buffer
      (insert source)
      (occur-1 prot-eww--occur-feed-regexp "\\3" (list (current-buffer)) buf-name))
    ;; Handle relative URLs, so that we get an absolute URL out of them.
    ;; Findings like "rss.xml" are not particularly helpful.
    ;;
    ;; NOTE 2021-03-31: the base-url heuristic may not always be
    ;; correct, though it has worked in all cases I have tested it on.
    (when (get-buffer buf-name)
      (with-current-buffer (get-buffer buf-name)
        (let ((inhibit-read-only t)
              (base-url (replace-regexp-in-string "\\(.*/\\)[^/]+\\'" "\\1" url)))
          (goto-char (point-min))
          (while (< (point) (point-max))
            (goto-char (line-beginning-position))
            (when (and (looking-at prot-common-url-regexp)
                       (not (looking-at (format "%s.*" url))))
              (insert base-url))
            (forward-line 1)))))))

;;;###autoload
(defun prot-eww-open-in-other-window ()
  "Use `eww-open-in-new-buffer' in another window."
  (interactive)
  (other-window-prefix)       ; For emacs28 -- it's a hack, but why not?
  (eww-open-in-new-buffer))

(provide 'prot-eww)
;;; prot-eww.el ends here
