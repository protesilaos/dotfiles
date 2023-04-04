;;; Simple HTML Renderer (shr), Emacs Web Wowser (eww), Elpher, and prot-eww.el
(prot-emacs-builtin-package 'browse-url
  (:delay 5)
  (setq browse-url-browser-function 'eww-browse-url)
  (setq browse-url-secondary-browser-function 'browse-url-default-browser))

(prot-emacs-builtin-package 'goto-addr
  (:delay 5)
  (setq goto-address-url-face 'link)
  (setq goto-address-url-mouse-face 'highlight)
  (setq goto-address-mail-face nil)
  (setq goto-address-mail-mouse-face 'highlight))

(prot-emacs-builtin-package 'shr
  (:delay 5)
  (setq shr-use-colors nil)             ; t is bad for accessibility
  (setq shr-use-fonts nil)              ; t is not for me
  (setq shr-max-image-proportion 0.6)
  (setq shr-image-animate nil)          ; No GIFs, thank you!
  (setq shr-width fill-column)          ; check `prot-eww-readable'
  (setq shr-max-width fill-column)
  (setq shr-discard-aria-hidden t)
  (setq shr-cookie-policy nil))

(prot-emacs-builtin-package 'url-cookie
  (:delay 5)
  (setq url-cookie-untrusted-urls '(".*")))

(prot-emacs-builtin-package 'eww
  (:delay 5)
  (setq eww-restore-desktop t)
  (setq eww-desktop-remove-duplicates t)
  (setq eww-header-line-format nil)
  (setq eww-search-prefix "https://duckduckgo.com/html/?q=")
  (setq eww-download-directory (expand-file-name "~/Documents/eww-downloads"))
  (setq eww-suggest-uris
        '(eww-links-at-point
          thing-at-point-url-at-point))
  (setq eww-bookmarks-directory (locate-user-emacs-file "eww-bookmarks/"))
  (setq eww-history-limit 150)
  (setq eww-use-external-browser-for-content-type
        "\\`\\(video/\\|audio\\)") ; On GNU/Linux check your mimeapps.list
  (setq eww-browse-url-new-window-is-tab nil)
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

  (define-key eww-link-keymap (kbd "v") nil) ; stop overriding `eww-view-source'
  (define-key eww-mode-map (kbd "L") #'eww-list-bookmarks)
  (define-key dired-mode-map (kbd "E") #'eww-open-file) ; to render local HTML files
  (define-key eww-buffers-mode-map (kbd "d") #'eww-bookmark-kill)   ; it actually deletes
  (define-key eww-bookmark-mode-map (kbd "d") #'eww-bookmark-kill)) ; same

(prot-emacs-elpa-package 'elpher)    ; NOTE 2021-07-24: work-in-progress

(prot-emacs-builtin-package 'prot-eww
  (setq prot-eww-save-history-file
        (locate-user-emacs-file "prot-eww-visited-history"))
  (setq prot-eww-save-visited-history t)
  (setq prot-eww-bookmark-link nil)

  (add-hook 'prot-eww-history-mode-hook #'hl-line-mode)

  (define-prefix-command 'prot-eww-map)
  (define-key global-map (kbd "C-c w") 'prot-eww-map)
  (let ((map prot-eww-map))
    (define-key map (kbd "b") #'prot-eww-visit-bookmark)
    (define-key map (kbd "e") #'prot-eww-browse-dwim)
    (define-key map (kbd "s") #'prot-eww-search-engine))
  (let ((map eww-mode-map))
    (define-key map (kbd "B") #'prot-eww-bookmark-page)
    (define-key map (kbd "D") #'prot-eww-download-html)
    (define-key map (kbd "F") #'prot-eww-find-feed)
    (define-key map (kbd "H") #'prot-eww-list-history)
    (define-key map (kbd "b") #'prot-eww-visit-bookmark)
    (define-key map (kbd "e") #'prot-eww-browse-dwim)
    (define-key map (kbd "o") #'prot-eww-open-in-other-window)
    (define-key map (kbd "E") #'prot-eww-visit-url-on-page)
    (define-key map (kbd "J") #'prot-eww-jump-to-url-on-page)
    (define-key map (kbd "R") #'prot-eww-readable)
    (define-key map (kbd "Q") #'prot-eww-quit)))

;;; Elfeed feed/RSS reader
(prot-emacs-elpa-package 'elfeed
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

  ;; Make sure to also check the section on shr and eww for how I handle
  ;; `shr-width' there.
  (add-hook 'elfeed-show-mode-hook
            (lambda () (setq-local shr-width (current-fill-column))))

  (define-key global-map (kbd "C-c e") #'elfeed)
  (let ((map elfeed-search-mode-map))
    (define-key map (kbd "w") #'elfeed-search-yank)
    (define-key map (kbd "g") #'elfeed-update)
    (define-key map (kbd "G") #'elfeed-search-update--force))
  (let ((map elfeed-show-mode-map))
    (define-key map (kbd "w") #'elfeed-show-yank)))

(with-eval-after-load 'elfeed
  (prot-emacs-builtin-package 'prot-elfeed
    (setq prot-elfeed-tag-faces t)
    (prot-elfeed-fontify-tags)
    (add-hook 'elfeed-search-mode-hook #'prot-elfeed-load-feeds)

    (let ((map elfeed-search-mode-map))
      (define-key map (kbd "s") #'prot-elfeed-search-tag-filter)
      (define-key map (kbd "+") #'prot-elfeed-toggle-tag))
    (define-key elfeed-show-mode-map (kbd "+") #'prot-elfeed-toggle-tag)))

;;; Elfeed extensions for watching videos (elfeed-tube)
(prot-emacs-elpa-package 'elfeed-tube
  ;; (setq elfeed-tube-auto-save-p nil) ; default value
  ;; (setq elfeed-tube-auto-fetch-p t)  ; default value
  (elfeed-tube-setup)

  (let ((map elfeed-show-mode-map))
    (define-key map (kbd "F") #'elfeed-tube-fetch)
    (define-key map [remap save-buffer] #'elfeed-tube-save))
  (let ((map elfeed-search-mode-map))
    (define-key map (kbd "F") #'elfeed-tube-fetch)
    (define-key map [remap save-buffer] #'elfeed-tube-save)))

(prot-emacs-elpa-package 'mpv)

(prot-emacs-elpa-package 'elfeed-tube-mpv
  (define-key elfeed-search-mode-map (kbd "v") 'elfeed-tube-mpv)
  (let ((map elfeed-show-mode-map))
    (define-key map (kbd "v") 'elfeed-tube-mpv)
    (define-key map (kbd "C-c C-v") 'elfeed-tube-mpv)
    (define-key map (kbd "C-c C-f") 'elfeed-tube-mpv-follow-mode)
    (define-key map (kbd "C-c C-w") 'elfeed-tube-mpv-where)))

;;; Rcirc (IRC client)
(prot-emacs-builtin-package 'rcirc
  (:delay 5)
  (setq rcirc-server-alist
        `(("irc.libera.chat"
           :channels ("#emacs" "#tropin" "#rcirc")
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

  (rcirc-track-minor-mode 1)

  (define-key global-map (kbd "C-c i") #'irc))

(provide 'prot-emacs-web)
