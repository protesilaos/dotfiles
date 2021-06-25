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
;;
;; Thanks to Abhiseck Paira for the patches (see commit log for this
;; file, such as with C-x v l (vc-print-log)).

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

(defcustom prot-eww-save-history-file
  (locate-user-emacs-file "prot-eww-visited-history")
  "File to save the value of `prot-eww-visited-history'."
  :type 'file
  :group 'prot-eww)

(defcustom prot-eww-save-visited-history nil
  "Whether to save `prot-eww-visited-history'.
If non-nil, save the value of `prot-eww-visited-history' in
`prot-eww-save-history-file'."
  :type 'boolean
  :group 'prot-eww)

(defun prot-eww-save-visited-history ()
  "Save the value of `prot-eww-visited-history' in a file.
The file is determined by the variable `prot-eww-save-history-file'."
  (when prot-eww-save-visited-history
    (with-temp-file prot-eww-save-history-file
      (insert (concat ";; Auto-generated file;"
                      " don't edit -*- mode: lisp-data -*-\n"))
      (pp prot-eww-visited-history (current-buffer)))))

(defun prot-eww-read-visited-history (&optional error-out)
  "Read history from `prot-eww-save-history-file'.
If ERROR-OUT, signal `user-error' if there is no history."
  (when prot-eww-save-visited-history
    (let ((file prot-eww-save-history-file))
      (setq prot-eww-visited-history
            (unless (zerop
                     (or (file-attribute-size (file-attributes file))
                         0))
              (with-temp-buffer
                (insert-file-contents file)
                (read (current-buffer)))))
      (when (and error-out (not prot-eww-visited-history))
        (user-error "No history is defined")))))

(unless prot-eww-visited-history
  (prot-eww-read-visited-history t))

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
new eww buffer.  If URL does not look like a valid link, run a
web query using `eww-search-prefix'.

When called from an eww buffer, provide the current link as
\\<minibuffer-local-map>\\[next-history-element]."
  (interactive
   (let ((all-history (delete-dups
                       (append prot-eww-visited-history
                               eww-prompt-history)))
         (current-url (plist-get eww-data :url)))
     (list
      (completing-read "Run EWW on: " all-history
                       nil nil current-url 'eww-prompt-history current-url)
      (prefix-numeric-value current-prefix-arg))))
  (eww url arg))

;;;###autoload
(defun prot-eww-visit-bookmark (&optional arg)
  "Visit bookmarked URL.

With optional prefix ARG (\\[universal-argument]) open URL in a
new EWW buffer."
  (interactive "P")
  (eww-read-bookmarks)
  (let ((list (gensym)))
    (dolist (bookmark eww-bookmarks)
      (push (plist-get bookmark :url) list))
    (if eww-bookmarks
        (eww (completing-read "Visit EWW bookmark: " list)
             (when arg 4))
      (user-error "No bookmarks"))))

(defun prot-eww--capture-url-on-page (&optional position)
  "Capture all the links on the current web page.

Return a list of strings.  Strings are in the form LABEL @ URL.
When optional argument POSITION is non-nil, include position info
in the strings too, so strings take the form
LABEL @ URL ~ POSITION."
  (let ((links)
        (match (gensym)))
    (save-excursion
      (goto-char (point-min))
      (while (setq match (text-property-search-forward
                          'shr-url nil nil t))
        (let* ((raw-url (prop-match-value match))
               (start-point-prop (prop-match-beginning match))
               (end-point-prop (prop-match-end match))
               (url (when (stringp raw-url)
                      (propertize raw-url 'face 'link)))
               (label (buffer-substring-no-properties
                       start-point-prop end-point-prop))
               (point start-point-prop))
          (when url
            (if position
                (push (format "%s  @ %s ~ %d"
                              label url point)
                      links)
              (push (format "%s  @ %s"
                            label url)
                    links))))))
    links))

;;;###autoload
(defun prot-eww-visit-url-on-page (&optional arg)
  "Visit URL from list of links on the page using completion.

With optional prefix ARG (\\[universal-argument]) open URL in a
new EWW buffer."
  (interactive "P")
  (when (derived-mode-p 'eww-mode)
    (let* ((links (prot-eww--capture-url-on-page))
           (selection (completing-read "Browse URL from page: " links nil t))
           (url (replace-regexp-in-string ".*@ " "" selection)))
      (eww url (when arg 4)))))

;;;###autoload
(defun prot-eww-jump-to-url-on-page ()
  "Jump to URL position on the page using completion.

With optional prefix ARG (\\[universal-argument]) open URL in a
new EWW buffer."
  (interactive)
  (when (derived-mode-p 'eww-mode)
    (let* ((links (prot-eww--capture-url-on-page t))
           (selection (completing-read "Jump to URL on page: " links nil t))
           (position (replace-regexp-in-string ".*~ " "" selection))
           (point (string-to-number position)))
      (goto-char point)
      (prot-pulse-pulse-line))))

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

;;TODO: Add this variable as user-option, that is, define it with
;;`defcustom' so that users can use the customization interface to
;;modify it.

(defvar prot-eww-search-engines
  '((debbugs . (debbugs
                "https://debbugs.gnu.org/cgi/bugreport.cgi?bug="
                hist-var prot-eww--debbugs-hist))
    (wikipedia . (wikipedia
                  "https://en.m.wikipedia.org/w/index.php?search="
                  hist-var prot-eww--wikipedia-hist))
    (archwiki . (archwiki
                 "https://wiki.archlinux.org/index.php?search="
                 hist-var prot-eww--archwiki-hist))
    (aur . (aur "https://aur.archlinux.org/packages/?K="
                hist-var prot-eww--aur-hist)))
  "Alist of Plist of web search engines related data.
From now on refer to this type of data as APLIST.  Each element
of APLIST is (KEY . VALUE) pair.  KEY is a symbol specifying
search engine name.  The VALUE is property list.

The plist has two key-value pairs.  K1 is the same symbol has KEY
and V1 is search string of the search engine.

K2 is the symbol 'hist-var', V2 is also a symbol that has a format
'prot-eww--K1-hist'.

NOTE: If you modify this variable after prot-eww is loaded you
need to run the following code after modification:
`(prot-eww--define-hist-var prot-eww-search-engines)'")

;; Below 's-string' is short for 'search-string'. For wikipedia which
;; is this string: "https://en.m.wikipedia.org/w/index.php?search=". I
;; use this name because I don't know it's proper name.

;; Define constructor and selectors functions to access
;; `prot-eww-search-engines'.
;; the constructor
(defun prot-eww--cons-search-engines (name s-string)
  "Include a new Alist element.
The alist element is added to variable `prot-eww-search-engines'.

NAME should be symbol representing the search engine.  S-STRING
should be string, which is specific to named search engine."
  (let ((my-plist `(,name ,s-string))
        (hist-var-name (format "prot-eww--%s-hist"
                               (symbol-name name))))
    (plist-put my-plist 'hist-var (intern hist-var-name))
    (let ((my-alist (cons name my-plist)))
      (add-to-list 'prot-eww-search-engines my-alist))))

;; Selectors definitions start
(defun prot-eww--select-hist-name (aplist engine-name)
  "Get hist-var-name from APLIST of ENGINE-NAME."
  (let ((hist-var-name (plist-get
                        (alist-get engine-name aplist)
                        'hist-var)))
    hist-var-name))

(defun prot-eww--select-engine-names (aplist)
  "Return a list of search-engine names from APLIST.
Each value of the list is a string."
  (mapcar (lambda (x) (format "%s" (car x)))
          aplist))

(defun prot-eww--select-s-string (aplist engine-name)
  "Return the search-string for specified ENGINE-NAME from APLIST."
  (plist-get
   (alist-get engine-name aplist)
   engine-name))
;; Selector definitions end here.

(defun prot-eww--define-hist-var (aplist)
  "Initialize APLIST hist-variables to empty list; return nil."
  (let ((engine-names
         (prot-eww--select-engine-names aplist)))
    (dolist (engine engine-names)
      (let ((hist-var-name
             (prot-eww--select-hist-name aplist
                                         (intern engine))))
        (set hist-var-name '())))))

(prot-eww--define-hist-var prot-eww-search-engines)

;;;###autoload
(defun prot-eww-search-engine (engine s-term &optional arg)
  "Search S-TERM using ENGINE.
ENGINE is an assossiation defined in `prot-eww-search-engines'.

With optional prefix ARG (\\[universal-argument]) open the search
result in a new buffer."
  (interactive
   (let* ((engine-list (prot-eww--select-engine-names
                        prot-eww-search-engines))
          (engine-name (completing-read
                        "Search with: " engine-list nil t nil
                        'prot-eww--engine-hist))
          (history-list (prot-eww--select-hist-name
                         prot-eww-search-engines
                         (intern engine-name)))
          (search-term (read-string
                        "Search for: " nil history-list)))
     (list engine-name search-term
           (prefix-numeric-value current-prefix-arg))))
  (let* ((s-string
          (prot-eww--select-s-string prot-eww-search-engines
                                     (intern engine)))
         (eww-pass (format "%s%s" s-string s-term))
         (history-list (prot-eww--select-hist-name
                        prot-eww-search-engines
                        (intern engine))))
    (add-to-history history-list s-term)
    (eww eww-pass arg)))

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

(defun prot-eww--kill-buffers-major-mode (mode-symbol)
  "Kill all the buffer which have MODE-SYMBOL as the `major-mode'."
  (save-current-buffer
    (let ((list-buffers (buffer-list)))
      (dolist (buffer list-buffers)
        (set-buffer buffer)
        (when (eq major-mode mode-symbol)
          (kill-buffer buffer))))))

(defun prot-eww-kill-eww-buffers ()
  "Kill all the buffers which have 'eww-mode' as `major-mode'."
  (prot-eww--kill-buffers-major-mode 'eww-mode))

(defcustom prot-eww-delete-cookies t
  "If non-nil delete cookies when prot-eww-quit is called."
  :type 'boolean
  :group 'prot-eww)

(defun prot-eww-delete-cookies ()
  "Delete cookies from the cookie file."
  (when prot-eww-delete-cookies
    (url-cookie-delete-cookies)))

;; TODO: Make it defcustom
(defvar prot-eww-quit-hook nil
  "Run this hook when `prot-eww-quit' is called.")

;; Populate the hook with these functions.
(dolist (func '(prot-eww-delete-cookies
                prot-eww-kill-eww-buffers
                prot-eww-save-visited-history))
  (add-hook 'prot-eww-quit-hook func))

(defun prot-eww-quit ()
  "Quit eww, kill all its buffers, delete all cookies.
As a final step, save `prot-eww-visited-history' to a file (see
`prot-eww-save-history-file')."
  (interactive)
  (if prot-eww-save-visited-history
      (when (y-or-n-p "Are you sure you want to quit eww? ")
        (run-hooks 'prot-eww-quit-hook))
    ;;
    ;; Now users have full control what `prot-eww-quit' does, by
    ;; modifying `prot-eww-quit-hook'.
    (when (yes-or-no-p "Are you sure you want to quit eww?")
      (run-hooks 'prot-eww-quit-hook))))

(provide 'prot-eww)
;;; prot-eww.el ends here
