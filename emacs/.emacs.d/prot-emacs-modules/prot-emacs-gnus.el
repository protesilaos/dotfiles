;; This is deprecated code as I stopped using Gnus a long time ago.  I
;; keep it here for when I need to test it for my themes (`ef-themes',
;; `modus-themes', `standard-themes', `doric-themes').


(use-package gnus
  :ensure nil
  :config
  (require 'gnus-sum)
  (require 'gnus-dired)
  (require 'gnus-topic)

;;; accounts
  (setq gnus-select-method '(nnnil ""))
  (setq gnus-secondary-select-methods
        '((nntp "news.gwene.org")
          ;; ;;  NOTE 2021-05-13: Switched to notmuch.
          ;; (nnmaildir "prv" (directory "~/.mail/prv")
          ;;  (gnus-search-engine gnus-search-notmuch ; this feature is from Emacs 28
 		  ;;   		           (remove-prefix "~/.mail/prv")))
          ;; (nnmaildir "inf" (directory "~/.mail/inf")
          ;;  (gnus-search-engine gnus-search-notmuch
 		  ;;   		           (remove-prefix "~/.mail/inf")))
          ;; (nnmaildir "pub" (directory "~/.mail/pub")
          ;;  (gnus-search-engine gnus-search-notmuch
 		  ;;   		           (remove-prefix "~/.mail/pub")))
          ))

  (setq gnus-search-use-parsed-queries nil) ; Emacs 28

  ;; ;;  NOTE 2021-05-13: Switched to notmuch.
  ;; (setq gnus-parameters
  ;;       '((".*"                         ; fallback option
  ;;          (posting-style
  ;;           (gcc "nnmaildir+inf:Sent")
  ;;           (From
  ;;            (format "%s <%s>" user-full-name
  ;;                    (prot-mail-auth-get-field "inf" :user)))))
  ;;         ("prv"
  ;;          (posting-style
  ;;           (gcc "nnmaildir+prv:Sent")
  ;;           (From
  ;;            (format "%s <%s>" user-full-name
  ;;                    (prot-mail-auth-get-field "prv" :user)))))
  ;;         ("pub"
  ;;          (posting-style               ; Uses default name+mail
  ;;           (gcc "nnmaildir+pub:Sent")))))

  (setq gnus-gcc-mark-as-read t)
  (setq gnus-agent t)
  (setq gnus-novice-user nil)           ; careful with this
  ;; checking sources
  (setq gnus-check-new-newsgroups 'ask-server)
  (setq gnus-read-active-file 'some)
  ;; dribble
  (setq gnus-use-dribble-file t)
  (setq gnus-always-read-dribble-file t)
;;; agent
  (setq gnus-agent-article-alist-save-format 1)  ; uncompressed
  (setq gnus-agent-cache t)
  (setq gnus-agent-confirmation-function 'y-or-n-p)
  (setq gnus-agent-consider-all-articles nil)
  (setq gnus-agent-directory "~/News/agent/")
  (setq gnus-agent-enable-expiration 'ENABLE)
  (setq gnus-agent-expire-all nil)
  (setq gnus-agent-expire-days 30)
  (setq gnus-agent-mark-unread-after-downloaded t)
  (setq gnus-agent-queue-mail t)        ; queue if unplugged
  (setq gnus-agent-synchronize-flags nil)
;;; article
  (setq gnus-article-browse-delete-temp 'ask)
  (setq gnus-article-over-scroll nil)
  (setq gnus-article-show-cursor t)
  (setq gnus-article-sort-functions
        '((not gnus-article-sort-by-number)
          (not gnus-article-sort-by-date)))
  (setq gnus-article-truncate-lines nil)
  (setq gnus-html-frame-width 80)
  (setq gnus-html-image-automatic-caching t)
  (setq gnus-inhibit-images t)
  (setq gnus-max-image-proportion 0.7)
  (setq gnus-treat-display-smileys nil)
  (setq gnus-article-mode-line-format "%G %S %m")
  (setq gnus-visible-headers
        '("^From:" "^To:" "^Cc:" "^Subject:" "^Newsgroups:" "^Date:"
          "Followup-To:" "Reply-To:" "^Organization:" "^X-Newsreader:"
          "^X-Mailer:"))
  (setq gnus-sorted-header-list gnus-visible-headers)
  (setq gnus-article-x-face-too-ugly ".*") ; all images in headers are outright annoying---disabled!
;;; async
  (setq gnus-asynchronous t)
  (setq gnus-use-article-prefetch 15)
;;; group
  (setq gnus-level-subscribed 6)
  (setq gnus-level-unsubscribed 7)
  (setq gnus-level-zombie 8)
  (setq gnus-activate-level 1)
  (setq gnus-list-groups-with-ticked-articles nil)
  (setq gnus-group-sort-function
        '((gnus-group-sort-by-unread)
          (gnus-group-sort-by-alphabet)
          (gnus-group-sort-by-rank)))
  (setq gnus-group-line-format "%M%p%P%5y:%B%(%g%)\n")
  (setq gnus-group-mode-line-format "%%b")
  (setq gnus-topic-display-empty-topics nil)
;;; summary
  (setq gnus-auto-select-first nil)
  (setq gnus-summary-ignore-duplicates t)
  (setq gnus-suppress-duplicates t)
  (setq gnus-save-duplicate-list t)
  (setq gnus-summary-goto-unread nil)
  (setq gnus-summary-make-false-root 'adopt)
  (setq gnus-summary-thread-gathering-function
        'gnus-gather-threads-by-subject)
  (setq gnus-summary-gather-subject-limit 'fuzzy)
  (setq gnus-thread-sort-functions
        '((not gnus-thread-sort-by-date)
          (not gnus-thread-sort-by-number)))
  (setq gnus-subthread-sort-functions
        'gnus-thread-sort-by-date)
  (setq gnus-thread-hide-subtree nil)
  (setq gnus-thread-ignore-subject nil)
  (setq gnus-user-date-format-alist
        '(((gnus-seconds-today) . "Today at %R")
          ((+ (* 60 60 24) (gnus-seconds-today)) . "Yesterday, %R")
          (t . "%Y-%m-%d %R")))

  ;; When the %f specifier in `gnus-summary-line-format' matches my
  ;; name, this will use the contents of the "To:" field, prefixed by
  ;; the string I specify.  Useful when checking your "Sent" summary or
  ;; a mailing list you participate in.
  (setq gnus-ignored-from-addresses "Protesilaos Stavrou")
  (setq gnus-summary-to-prefix "To: ")

  (setq gnus-summary-line-format "%U%R %-18,18&user-date; %4L:%-25,25f %B%s\n")
  (setq gnus-summary-mode-line-format "[%U] %p")
  (setq gnus-sum-thread-tree-false-root "")
  (setq gnus-sum-thread-tree-indent " ")
  (setq gnus-sum-thread-tree-single-indent "")
  (setq gnus-sum-thread-tree-leaf-with-other "+-> ")
  (setq gnus-sum-thread-tree-root "")
  (setq gnus-sum-thread-tree-single-leaf "\\-> ")
  (setq gnus-sum-thread-tree-vertical "|")

  (add-hook 'dired-mode-hook #'gnus-dired-mode) ; dired integration
  (add-hook 'gnus-group-mode-hook #'gnus-topic-mode)
  (add-hook 'gnus-select-group-hook #'gnus-group-set-timestamp)

  (dolist (mode '(gnus-group-mode-hook gnus-summary-mode-hook gnus-browse-mode-hook))
    (add-hook mode #'hl-line-mode))

  ;; ;;  NOTE 2021-05-13: Switched to notmuch.
  ;; (define-key global-map (kbd "C-c m") #'gnus)
  (let ((map gnus-article-mode-map))
    (define-key map (kbd "i") #'gnus-article-show-images)
    (define-key map (kbd "s") #'gnus-mime-save-part)
    (define-key map (kbd "o") #'gnus-mime-copy-part))
  (let ((map gnus-group-mode-map))       ; I always use `gnus-topic-mode'
    (define-key map (kbd "n") #'gnus-group-next-group)
    (define-key map (kbd "p") #'gnus-group-prev-group)
    (define-key map (kbd "M-n") #'gnus-topic-goto-next-topic)
    (define-key map (kbd "M-p") #'gnus-topic-goto-previous-topic))
  (let ((map gnus-summary-mode-map))
    (define-key map (kbd "<delete>") #'gnus-summary-delete-article)
    (define-key map (kbd "n") #'gnus-summary-next-article)
    (define-key map (kbd "p") #'gnus-summary-prev-article)
    (define-key map (kbd "N") #'gnus-summary-next-unread-article)
    (define-key map (kbd "P") #'gnus-summary-prev-unread-article)
    (define-key map (kbd "M-n") #'gnus-summary-next-thread)
    (define-key map (kbd "M-p") #'gnus-summary-prev-thread)
    (define-key map (kbd "C-M-n") #'gnus-summary-next-group)
    (define-key map (kbd "C-M-p") #'gnus-summary-prev-group)
    (define-key map (kbd "C-M-^") #'gnus-summary-refer-thread)))

(use-package nnmail
  :ensure nil
  :config
  (setq nnmail-expiry-wait 30))         ; careful with this

(provide 'prot-emacs-gnus)
