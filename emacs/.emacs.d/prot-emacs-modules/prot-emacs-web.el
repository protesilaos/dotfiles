;;;; `browse-url'
(prot-emacs-configure
  (setq browse-url-browser-function 'eww-browse-url)
  (setq browse-url-secondary-browser-function 'browse-url-default-browser))

;;;; `goto-addr'
(prot-emacs-configure
  (add-hook 'text-mode-hook #'goto-address-mode)
  (add-hook 'prog-mode-hook #'goto-address-prog-mode)
  (setq goto-address-url-face 'link)
  (setq goto-address-url-mouse-face 'highlight)
  (setq goto-address-mail-face nil)
  (setq goto-address-mail-mouse-face 'highlight))

;;;; `shr' (Simple HTML Renderer)
(prot-emacs-configure
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
(prot-emacs-configure
  (setq url-cookie-untrusted-urls '(".*")))

;;;; `eww' (Emacs Web Wowser)
(prot-emacs-configure
  (with-eval-after-load 'eww
    (autoload #'prot-simple-buffers-major-mode "prot-simple")

    (prot-emacs-keybind eww-mode-map
      "S" nil ; unmap `eww-list-buffers'
      "b" #'prot-simple-buffers-major-mode ; a general version to show buffer of current mode
      "m" #'bookmark-set)

    (define-key eww-link-keymap (kbd "v")  nil) ; stop overriding `eww-view-source'

    (with-eval-after-load 'dired
      (define-key dired-mode-map (kbd "E") #'eww-open-file)) ; to render local HTML files

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
      (put command 'disabled t))))

;;;; `prot-eww' extras
(prot-emacs-configure
  (with-eval-after-load 'eww
    (require 'prot-eww)
    (prot-emacs-keybind eww-mode-map
      "F" #'prot-eww-find-feed
      "o" #'prot-eww-open-in-other-window
      "j" #'prot-eww-jump-to-url-on-page
      "J" #'prot-eww-visit-url-on-page)))

;;; Elfeed feed/RSS reader
(prot-emacs-configure
  (prot-emacs-install elfeed)

  (define-key global-map (kbd "C-c e") #'elfeed)

  (with-eval-after-load 'elfeed

    (add-hook 'elfeed-show-mode-hook #'visual-line-mode)

    (prot-emacs-keybind elfeed-search-mode-map
      "w" #'elfeed-search-yank
      "g" #'elfeed-update
      "G" #'elfeed-search-update--force)

    (define-key elfeed-show-mode-map (kbd "w") #'elfeed-show-yank)

    (setq elfeed-use-curl nil)
    (setq elfeed-curl-max-connections 10)
    (setq elfeed-db-directory (expand-file-name "elfeed/" user-emacs-directory))
    (setq elfeed-enclosure-default-dir (expand-file-name "~/Downloads/"))
    (setq elfeed-search-filter "@2-weeks-ago +unread")
    (setq elfeed-sort-order 'descending)
    (setq elfeed-search-clipboard-type 'CLIPBOARD)
    (setq elfeed-search-title-max-width 100)
    (setq elfeed-search-title-min-width 30)
    (setq elfeed-search-trailing-width 25)
    (setq elfeed-show-truncate-long-urls t)
    (setq elfeed-show-unique-buffers t)
    (setq elfeed-search-date-format '("%F %R" 20 :left))
    ;; NOTE 2025-10-28: Instead of showing my `elfeed-feeds' here, I put
    ;; it in its own Emacs Lisp file.  The `load' function will evaluate
    ;; that code.  Internally, the file has just the following:
    ;;
    ;;     (setq elfeed-feeds FEEDS)
    (unless (bound-and-true-p elfeed-feeds)
      (load (expand-file-name "~/feeds.el.gpg") :no-error :no-message))
    ;; Make entries tagged as "personal" use the `bold-italic' face.
    ;; This way I can quickly notice my own entries in the listing.
    (add-to-list 'elfeed-search-face-alist '(personal bold-italic))))

;;; Rcirc (IRC client)
(prot-emacs-configure
  (with-eval-after-load 'rcirc
    (setq rcirc-server-alist
          `(("irc.libera.chat"
             :channels ("#emacs")
             :port 6697
             :encryption tls
             :password ,(prot-common-auth-get-field "libera" :secret))))

    (setq rcirc-prompt "%t> ") ; Read the docs or use (customize-set-variable 'rcirc-prompt "%t> ")

    (setq rcirc-default-nick "protesilaos"
          rcirc-default-user-name "protesilaos"
          rcirc-default-full-name "Protesilaos Stavrou")

    ;; ;; NOTE 2021-11-28: demo from the days of EmacsConf 2021.  I don't
    ;; ;; actually need this.
    ;; (setq rcirc-bright-nicks '("bandali" "sachac" "zaeph"))

    ;; NOTE 2021-11-28: Is there a canonical way to disable this?
    (setq rcirc-timeout-seconds most-positive-fixnum)

    (rcirc-track-minor-mode 1)))

(provide 'prot-emacs-web)
