;;;; `browse-url'
(use-package browse-url
  :ensure nil
  :commands
  ( browse-url browse-url-at-point browse-url-at-mouse browse-url-of-file
    browse-url-firefox browse-url-chromium browse-url-epiphany)
  :config
  (setq browse-url-browser-function 'eww-browse-url)
  (setq browse-url-secondary-browser-function 'browse-url-default-browser))

;;;; `goto-addr'
(use-package goto-addr
  :ensure nil
  :commands (goto-addr-mode goto-addr-prog-mode)
  :config
  (setq goto-address-url-face 'link)
  (setq goto-address-url-mouse-face 'highlight)
  (setq goto-address-mail-face nil)
  (setq goto-address-mail-mouse-face 'highlight))

;;;; `shr' (Simple HTML Renderer)
(use-package shr
  :ensure nil
  :defer t
  :config
  (setq shr-use-colors nil)             ; t is bad for accessibility
  (setq shr-use-fonts nil)              ; t is superfluous, given `variable-pitch-mode'
  (setq shr-max-image-proportion 0.6)
  (setq shr-image-animate nil)          ; No GIFs, thank you!
  (setq shr-width fill-column)
  (setq shr-max-width fill-column)
  (setq shr-discard-aria-hidden t)
  (setq shr-fill-text nil)              ; Emacs 31
  (setq shr-cookie-policy nil))

;;;; `url-cookie'
(use-package url-cookie
  :ensure nil
  :commands (url-cookie-list)
  :config
  (setq url-cookie-untrusted-urls '(".*")))

;;;; `eww' (Emacs Web Wowser)
(use-package eww
  :ensure nil
  :after prot-simple
  :commands (eww)
  :bind
  ( :map eww-mode-map
    ("S" . nil) ; unmap `eww-list-buffers'
    ("b" . prot-simple-buffers-major-mode) ; a general version to show buffer of current mode
    ("m" . bookmark-set)
    :map eww-link-keymap
    ("v" . nil) ; stop overriding `eww-view-source'
    :map dired-mode-map
    ("E" . eww-open-file)) ; to render local HTML files
  :config
  (setq eww-auto-rename-buffer 'title)
  (setq eww-header-line-format nil)
  (setq eww-bookmarks-directory (locate-user-emacs-file "eww-bookmarks/"))
  (setq eww-history-limit 150)
  (setq eww-use-external-browser-for-content-type
        "\\`\\(video/\\|audio\\)") ; On GNU/Linux check your mimeapps.list
  (setq eww-form-checkbox-selected-symbol "[X]")
  (setq eww-form-checkbox-symbol "[ ]")
  ;; NOTE `eww-retrieve-command' is for Emacs28.  I tried the following
  ;; two values.  The first would not render properly some plain text
  ;; pages, such as by messing up the spacing between paragraphs.  The
  ;; second is more reliable but feels slower.  So I just use the
  ;; default (nil), though I find wget to be a bit faster.  In that case
  ;; one could live with the occasional errors by using `eww-download'
  ;; on the offending page, but I prefer consistency.
  ;;
  ;; '("wget" "--quiet" "--output-document=-")
  ;; '("chromium" "--headless" "--dump-dom")
  (setq eww-retrieve-command nil)

  ;; NOTE 2025-02-15: Emacs has a robust framework for writing
  ;; bookmarks, which `eww' supports.  Though `eww' also defines its
  ;; own parallel bookmark data, which I do not want to use.  So here
  ;; I disable all the relevant commands.
  (dolist (command '( eww-list-bookmarks eww-add-bookmark eww-bookmark-mode
                      eww-list-buffers eww-toggle-fonts eww-toggle-colors
                      eww-switch-to-buffer))
    (put command 'disabled t)))

;;;; `prot-eww' extras
(use-package prot-eww
  :ensure nil
  :after eww
  :bind
  ( :map eww-mode-map
    ("F" . prot-eww-find-feed)
    ("o" . prot-eww-open-in-other-window)
    ("j" . prot-eww-jump-to-url-on-page)
    ("J" . prot-eww-visit-url-on-page)))

;;; Elfeed feed/RSS reader
(use-package elfeed
  :ensure t
  :hook
  (elfeed-show-mode . visual-line-mode)
  :bind
  ("C-c e" . elfeed)
  :config
  (setq elfeed-use-curl nil)
  (setq elfeed-curl-max-connections 10)
  (setq elfeed-db-directory (concat user-emacs-directory "elfeed/"))
  (setq elfeed-enclosure-default-dir "~/Downloads/")
  (setq elfeed-search-filter "@2-weeks-ago +unread")
  (setq elfeed-sort-order 'descending)
  (setq elfeed-search-clipboard-type 'CLIPBOARD)
  (setq elfeed-search-title-max-width 100)
  (setq elfeed-search-title-min-width 30)
  (setq elfeed-search-trailing-width 25)
  (setq elfeed-show-truncate-long-urls t)
  (setq elfeed-show-unique-buffers t)
  (setq elfeed-search-date-format '("%F %R" 16 :left))

  (prot-emacs-keybind elfeed-search-mode-map
    "w" #'elfeed-search-yank
    "g" #'elfeed-update
    "G" #'elfeed-search-update--force)

  (define-key elfeed-show-mode-map (kbd "w") #'elfeed-show-yank))

(use-package prot-elfeed
  :ensure nil
  :after elfeed
  :bind
  ( :map elfeed-search-mode-map
    ("s" . prot-elfeed-search-tag-filter)
    ("+" . prot-elfeed-toggle-tag)
    :map elfeed-show-mode-map
    ("+" . prot-elfeed-toggle-tag))
  :hook
  (elfeed-search-mode . prot-elfeed-load-feeds)
  :config
  (setq prot-elfeed-tag-faces t)
  (prot-elfeed-fontify-tags))

;;; Rcirc (IRC client)
(use-package rcirc
  :ensure nil
  :bind ("C-c i" . irc)
  :config
  (setq rcirc-server-alist
        `(("irc.libera.chat"
           :channels ("#emacs" "#rcirc")
           :port 6697
           :encryption tls
           :password ,(prot-common-auth-get-field "libera" :secret))))

  (setq rcirc-prompt "%t> ") ; Read the docs or use (customize-set-variable 'rcirc-prompt "%t> ")

  (setq rcirc-default-nick "protesilaos"
        rcirc-default-user-name rcirc-default-nick
        rcirc-default-full-name "Protesilaos Stavrou")

  ;; ;; NOTE 2021-11-28: demo from the days of EmacsConf 2021.  I don't
  ;; ;; actually need this.
  ;; (setq rcirc-bright-nicks '("bandali" "sachac" "zaeph"))

  ;; NOTE 2021-11-28: Is there a canonical way to disable this?
  (setq rcirc-timeout-seconds most-positive-fixnum)

  (rcirc-track-minor-mode 1))

(provide 'prot-emacs-web)
