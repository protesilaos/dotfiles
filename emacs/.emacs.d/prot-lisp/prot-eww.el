;;; prot-eww.el --- Extensions for EWW -*- lexical-binding: t -*-

;; Copyright (C) 2021  Protesilaos Stavrou

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos.com/dotemacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

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
;; <https://protesilaos.com/dotemacs/>.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

(require 'shr)
(require 'eww)
(require 'prot-common)
(require 'prot-pulse)

(defgroup prot-eww ()
  "Tweaks for EWW."
  :group 'eww)

;;;; Basic setup

(defun prot-eww--rename-buffer ()
  "Rename EWW buffer using page title or URL.
To be used by `eww-after-render-hook'."
  (let ((name (if (eq "" (plist-get eww-data :title))
                  (plist-get eww-data :url)
                (plist-get eww-data :title))))
    (rename-buffer (format "*%s # eww*" name) t)))

(add-hook 'eww-after-render-hook #'prot-eww--rename-buffer)
(advice-add 'eww-back-url :after #'prot-eww--rename-buffer)
(advice-add 'eww-forward-url :after #'prot-eww--rename-buffer)

(defvar prot-eww-visited-history '()
  "History of visited URLs.")

(defun prot-eww--record-history ()
  "Store URL in `prot-eww-visited-history'.
To be used by `eww-after-render-hook'."
  (let ((url (plist-get eww-data :url)))
    (add-to-history 'prot-eww-visited-history url)))

(add-hook 'eww-after-render-hook #'prot-eww--record-history)
(advice-add 'eww-back-url :after #'prot-eww--record-history)
(advice-add 'eww-forward-url :after #'prot-eww--record-history)

;;;; Commands

;;;###autoload
(defun prot-eww-browse-dwim (url &optional arg)
  "Visit a URL, maybe from `eww-prompt-history', with completion.

With optional prefix ARG (\\[universal-argument]) open URL in a
new eww buffer.

If URL does not look like a valid link, run a web query using
`eww-search-prefix'.

When called from an eww buffer, provide the current link as
initial input."
  (interactive
   (list
    (completing-read "Run EWW on: " (append prot-eww-visited-history eww-prompt-history)
                     nil nil (plist-get eww-data :url) 'eww-prompt-history)
    current-prefix-arg))
  (eww url (if arg 4 nil)))

;;;###autoload
(defun prot-eww-visit-bookmark (&optional arg)
  "Visit bookmarked URL.

With optional prefix ARG (\\[universal-argument]) open URL in a
new EWW buffer."
  (interactive "P")
  (eww-read-bookmarks t)
  (let ((list (gensym)))
    (dolist (bookmark eww-bookmarks)
      (push (plist-get bookmark :url) list))
    (eww (completing-read "Visit EWW bookmark: " list)
         (if arg 4 nil))))

;;;###autoload
(defun prot-eww-visit-url-on-page (&optional arg)
  "Visit URL from list of links on the page using completion.

With optional prefix ARG (\\[universal-argument]) open URL in a
new EWW buffer."
  (interactive "P")
  (when (derived-mode-p 'eww-mode)
    (let ((links))
      (save-excursion
        (goto-char (point-max))
        (while (text-property-search-backward 'shr-url nil nil t)
          (when (and (get-text-property (point) 'shr-url)
                     (not (get-text-property (point) 'eww-form)))
            (push (format "%s  @ %s"
                          (button-label (point))
                          (propertize (get-text-property (point) 'shr-url) 'face 'link))
                  links))))
      (let* ((selection (completing-read "Browse URL from page: " links nil t))
             (url (replace-regexp-in-string ".*@ " "" selection)))
        (eww url (if arg 4 nil))))))

;;;###autoload
(defun prot-eww-jump-to-url-on-page ()
  "Jump to URL position on the page using completion.

With optional prefix ARG (\\[universal-argument]) open URL in a
new EWW buffer."
  (interactive)
  (when (derived-mode-p 'eww-mode)
    (let ((links))
      (save-excursion
        (goto-char (point-max))
        (while (text-property-search-backward 'shr-url nil nil t)
          (when (and (get-text-property (point) 'shr-url)
                     (not (get-text-property (point) 'eww-form)))
            (push (format "%s  @ %s ~ %d"
                          (button-label (point))
                          (propertize (get-text-property (point) 'shr-url) 'face 'link)
                          (point))
                  links))))
      (let* ((selection (completing-read "Jump to URL on page: " links nil t))
             (position (replace-regexp-in-string ".*~ " "" selection))
             (point (string-to-number position)))
        (goto-char point)
        (prot-pulse-pulse-line)))))

(defvar prot-eww--occur-feed-regexp
  (concat "\\(rss\\|atom\\)\\+xml.\\(.\\|\n\\)"
          ".*href=[\"']\\(.*?\\)[\"']")
  "Regular expression to match web feeds in HTML source.")

;;;###autoload
(defun prot-eww-find-feed ()
  "Produce bespoke buffer with RSS/Atom links from XML source."
  (interactive)
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
    ;; correct, though it has worked in all websites I have tested it
    ;; in.
    (when (get-buffer buf-name)
      (with-current-buffer (get-buffer buf-name)
        (let ((inhibit-read-only t)
              (base-url (replace-regexp-in-string "\\(.*/\\)[^/]+\\'" "\\1" url)))
          (goto-char (point-min))
          (unless (re-search-forward prot-common-url-regexp nil t)
            (re-search-forward ".*")
            (replace-match (concat base-url "\\&"))))))))

(defvar prot-eww-search-engines
  '((debbugs . prot-eww-search-debbugs)
    (Wikipedia . prot-eww-search-wikipedia)
    (ArchWiki . prot-eww-search-arch-wiki)
    (AUR . prot-eww-search-arch-aur))
  "Alist of web search commands.
The car of each cons cell is an arbitrary string that describes
the function it is associated with.")

(defvar prot-eww--engine-hist '()
  "Input history for `prot-eww-search-engine'.")

;;;###autoload
(defun prot-eww-search-engine (engine)
  "Use ENGINE stored in `prot-eww-search-engines'."
  (interactive
   (list
    (completing-read
     "Search with: "
     (mapcar (lambda (x) (format "%s" (car x))) prot-eww-search-engines) nil t
     nil 'prot-eww--engine-hist)))
  (call-interactively (alist-get (intern engine) prot-eww-search-engines))
  (add-to-history 'prot-eww--engine-hist engine))

(defvar prot-eww--debbugs-hist '()
  "Input history for `prot-eww-search-debbugs'.")

;;;###autoload
(defun prot-eww-search-debbugs (number &optional arg)
  "Visit bug report NUMBER on debbugs.gnu.org.

With optional prefix ARG (\\[universal-argument]) open URL in a
new EWW buffer."
  (interactive
   (list (read-number "Browse debbugs number: " nil 'prot-eww--debbugs-hist)
         current-prefix-arg))
  (eww
   (format "https://debbugs.gnu.org/cgi/bugreport.cgi?bug=%d" number)
   (if arg 4 nil))
  (add-to-history 'prot-eww--debbugs-hist number))

(defvar prot-eww--wikipedia-hist '()
  "Input history for `prot-eww-search-wikipedia'.")

;;;###autoload
(defun prot-eww-search-wikipedia (string &optional arg)
  "Search Wikipedia page matching STRING.

With optional prefix ARG (\\[universal-argument]) open URL in a
new EWW buffer."
  (interactive
   (list (read-string "Search Wikipedia: " nil 'prot-eww--wikipedia-hist)
         current-prefix-arg))
  (eww
   (format "https://en.m.wikipedia.org/w/index.php?search=%s" string)
   (if arg 4 nil))
  (add-to-history 'prot-eww--wikipedia-hist string))

(defvar prot-eww--arch-wiki-hist '()
  "Input history for `prot-eww-search-arch-wiki'.")

;;;###autoload
(defun prot-eww-search-arch-wiki (string &optional arg)
  "Search Arch Linux Wiki page matching STRING.

With optional prefix ARG (\\[universal-argument]) open URL in a
new EWW buffer."
  (interactive
   (list (read-string "Search Arch Linux Wiki: " nil 'prot-eww--arch-wiki-hist)
         current-prefix-arg))
  (eww
   (format "https://wiki.archlinux.org/index.php?search=%s" string)
   (if arg 4 nil))
  (add-to-history 'prot-eww--arch-wiki-hist string))

(defvar prot-eww--arch-aur-hist '()
  "Input history for `prot-eww-search-arch-aur'.")

;;;###autoload
(defun prot-eww-search-arch-aur (string &optional arg)
  "Search Arch User Repository page matching STRING.

With optional prefix ARG (\\[universal-argument]) open URL in a
new EWW buffer."
  (interactive
   (list (read-string "Search Arch User Repository: " nil 'prot-eww--arch-aur-hist)
         current-prefix-arg))
  (eww
   (format "https://aur.archlinux.org/packages/?K=%s" (string-replace " " "+" string))
   (if arg 4 nil))
  (add-to-history 'prot-eww--arch-aur-hist string))

;;;###autoload
(defun prot-eww-open-in-other-window ()
  "Use `eww-open-in-new-buffer' in another window."
  (interactive)
  (other-window-prefix)       ; For emacs28 -- it's a hack, but why not?
  (eww-open-in-new-buffer))

;;;###autoload
(defun prot-eww-readable ()
  "Use more opinionated `eww-readable'.

Set width is set to `current-fill-column'.  Adjust size of
images."
  (interactive)
  (let ((shr-width (current-fill-column))
        (shr-max-image-proportion 0.35))
    (eww-readable)))

;;;###autoload
(defun prot-eww-bookmark-page (title)
  "Add eww bookmark named with TITLE."
  (interactive
   (list
    (read-string "Set bookmark title: " (plist-get eww-data :title))))
  (plist-put eww-data :title title)
  (eww-add-bookmark))

(defvar prot-eww--punctuation-regexp "[][{}!@#$%^&*()_=+'\"?,.\|;:~`‘’“”]*"
  "Regular expression of punctionation that should be removed.")

(defun prot-eww--slug-no-punct (str)
  "Convert STR to a file name slug."
  (replace-regexp-in-string prot-eww--punctuation-regexp "" str))

(defun prot-eww--slug-hyphenate (str)
  "Replace spaces with hyphens in STR.
Also replace multiple hyphens with a single one and remove any
trailing hyphen."
  (replace-regexp-in-string
    "-$" ""
   (replace-regexp-in-string
   "-\\{2,\\}" "-"
    (replace-regexp-in-string "--+\\|\s+" "-" str))))

(defun prot-eww--sluggify (str)
  "Make STR an appropriate file name slug."
  (downcase (prot-eww--slug-hyphenate (prot-eww--slug-no-punct str))))

;;;###autoload
(defun prot-eww-download-html (name)
  "Download web page and call the file with NAME."
  (interactive
   (list
    (prot-eww--sluggify
     (read-string "Set downloaded file name: " (plist-get eww-data :title)))))
  (let* ((path (thread-last eww-download-directory
                 (expand-file-name
                  (concat (format-time-string "%Y%m%d_%H%M%S") "--" name ".html"))))
         (out (prot-common-shell-command-with-exit-code-and-output
               "wget" "-q" (format "%s" (plist-get eww-data :url))
                      "-O" (format "%s" (shell-quote-argument path)))))
    (if (= (car out) 0)
        (message "Downloaded page at %s" path)
      (message "Error downloading page: %s" (cdr out)))))

(provide 'prot-eww)
;;; prot-eww.el ends here
