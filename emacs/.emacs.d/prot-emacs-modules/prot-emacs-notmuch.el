;;; Notmuch (mail indexer and mail user agent (MUA))

;; I install notmuch from the distro's repos because the CLI program is
;; not dependent on Emacs.  Though the package also includes notmuch.el
;; which is what we use here (they are maintained by the same people).
(use-package notmuch
  :load-path "/usr/share/emacs/site-lisp/"
  :defer t
  :commands (notmuch notmuch-mua-new-mail))

;;; Account settings
(use-package notmuch
  :defer t
  :config
  (let ((prv (prot-common-auth-get-field "prv" :user))
        (pub (prot-common-auth-get-field "pub" :user))
        (inf (prot-common-auth-get-field "inf" :user))
        (box (prot-common-auth-get-field "mailbox" :user)))
    (setq notmuch-identities
          (mapcar (lambda (str)
                    (format "%s <%s>" user-full-name str))
                  (list prv pub inf box))
          notmuch-fcc-dirs
          `((,prv . "mailbox/Sent")
            (,inf . "mailbox/Sent")
            (,pub . "mailbox/Sent")
            (,box . "mailbox/Sent")))))

;;;; General UI
(use-package notmuch
  :defer t
  :config
  (setq notmuch-show-logo nil
        notmuch-column-control 1.0
        notmuch-hello-auto-refresh t
        notmuch-hello-recent-searches-max 20
        notmuch-hello-thousands-separator ""
        notmuch-hello-sections '(notmuch-hello-insert-saved-searches)
        notmuch-show-all-tags-list t))

;;;; Search
(use-package notmuch
  :defer t
  :config
  (setq notmuch-search-oldest-first nil)
  (setq notmuch-search-result-format
        '(("date" . "%12s  ")
          ("count" . "%-7s  ")
          ("authors" . "%-20s  ")
          ("subject" . "%-80s  ")
          ("tags" . "(%s)")))
  (setq notmuch-tree-result-format
        '(("date" . "%12s  ")
          ("authors" . "%-20s  ")
          ((("tree" . "%s")
            ("subject" . "%s"))
           . " %-80s  ")
          ("tags" . "(%s)")))
  (setq notmuch-search-line-faces
        '(("unread" . notmuch-search-unread-face)
          ;; ;; NOTE 2022-09-19: I disable this because I add a cosmeic
          ;; ;; emoji via `notmuch-tag-formats'.  This way I do not get
          ;; ;; an intense style which is very distracting when I filter
          ;; ;; my mail to include this tag.
          ;;
          ;; ("flag" . notmuch-search-flagged-face)
          ;;
          ;; Using `italic' instead is just fine.  Though I also tried
          ;; it without any face and I was okay with it.  The upside of
          ;; having a face is that you can identify the message even
          ;; when the window is split and you don't see the tags.
          ("flag" . italic)))
  (setq notmuch-show-empty-saved-searches t)
  (setq notmuch-saved-searches
        `(( :name "📥 inbox"
            :query "tag:inbox"
            :sort-order newest-first
            :key ,(kbd "i"))
          ( :name "📚 all unread (inbox)"
            :query "tag:unread and tag:inbox"
            :sort-order newest-first
            :key ,(kbd "u"))
          ( :name "💬 unread other (inbox)"
            :query "tag:unread and tag:inbox and not tag:package and not tag:coach"
            :sort-order newest-first
            :key ,(kbd "o"))
          ( :name "🗂️ unread packages"
            :query "tag:unread and tag:package"
            :sort-order newest-first
            :key ,(kbd "p"))
          ;; My coaching job: <https://protesilaos.com/coach/>.
          ( :name "🏆 unread coaching"
            :query "tag:unread and tag:coach"
            :sort-order newest-first
            :key ,(kbd "c")))))

;;;; Tags
(use-package notmuch
  :defer t
  :config
  (setq notmuch-archive-tags nil ; I do not archive email
        notmuch-message-replied-tags '("+replied")
        notmuch-message-forwarded-tags '("+forwarded")
        notmuch-show-mark-read-tags '("-unread")
        notmuch-draft-tags '("+draft")
        notmuch-draft-folder "drafts"
        notmuch-draft-save-plaintext 'ask)

  ;; Also see `notmuch-tagging-keys' in the `prot-notmuch' section
  ;; further below.
  ;;
  ;; All emoji are cosmetic.  The tags are just the text.
  (setq notmuch-tag-formats
        '(("unread" (propertize tag 'face 'notmuch-tag-unread))
          ("flag" (propertize tag 'face 'notmuch-tag-flagged)
           (concat tag "🚩")))
        notmuch-tag-deleted-formats
        '(("unread" (notmuch-apply-face bare-tag 'notmuch-tag-deleted)
           (concat "👁️‍🗨️" tag))
          (".*" (notmuch-apply-face tag 'notmuch-tag-deleted)
           (concat "🚫" tag)))
        notmuch-tag-added-formats
        '(("del" (notmuch-apply-face tag 'notmuch-tag-added)
           (concat "💥" tag))
          (".*" (notmuch-apply-face tag 'notmuch-tag-added)
           (concat "🏷️" tag)))))

;;;; Email composition
(use-package notmuch
  :defer t
  :config
  (setq notmuch-mua-compose-in 'current-window)
  (setq notmuch-mua-hidden-headers nil)
  (setq notmuch-address-command 'internal) ; NOTE 2024-01-09: I am not using this and must review it.
  (setq notmuch-always-prompt-for-sender t)
  (setq notmuch-mua-cite-function 'message-cite-original-without-signature)
  (setq notmuch-mua-reply-insert-header-p-function 'notmuch-show-reply-insert-header-p-never)
  (setq notmuch-mua-user-agent-function nil)
  (setq notmuch-maildir-use-notmuch-insert t)
  (setq notmuch-crypto-process-mime t)
  (setq notmuch-crypto-get-keys-asynchronously t)
  (setq notmuch-mua-attachment-regexp   ; see `notmuch-mua-send-hook'
        (concat "\\b\\(attache\?ment\\|attached\\|attach\\|"
                "pi[èe]ce\s+jointe?\\|"
                "συνημμ[εέ]νο\\|επισυν[αά]πτω\\)\\b")))

;;;; Reading messages
(use-package notmuch
  :defer t
  :config
  (setq notmuch-show-relative-dates t)
  (setq notmuch-show-all-multipart/alternative-parts nil)
  (setq notmuch-show-indent-messages-width 0)
  (setq notmuch-show-indent-multipart nil)
  (setq notmuch-show-part-button-default-action 'notmuch-show-view-part)
  (setq notmuch-show-text/html-blocked-images ".") ; block everything
  (setq notmuch-wash-wrap-lines-length 120)
  (setq notmuch-unthreaded-show-out nil)
  (setq notmuch-message-headers '("To" "Cc" "Subject" "Date"))
  (setq notmuch-message-headers-visible t)

  (let ((count most-positive-fixnum)) ; I don't like the buttonisation of long quotes
    (setq notmuch-wash-citation-lines-prefix count
          notmuch-wash-citation-lines-suffix count)))

;;;; Hooks and key bindings
(use-package notmuch
  :hook
  (notmuch-mua-send . notmuch-mua-attachment-check) ; also see `notmuch-mua-attachment-regexp'
  (notmuch-show . (lambda () (setq-local header-line-format nil)))
  :config
  (remove-hook 'notmuch-show-hook #'notmuch-show-turn-on-visual-line-mode)
  (remove-hook 'notmuch-search-hook #'notmuch-hl-line-mode) ; Check my `lin' package
  :bind
  ( :map global-map
    ("C-c m" . notmuch)
    ("C-x m" . notmuch-mua-new-mail) ; override `compose-mail'
    :map notmuch-search-mode-map ; I normally don't use the tree view, otherwise check `notmuch-tree-mode-map'
    ("a" . nil) ; the default is too easy to hit accidentally and I do not archive stuff
    ("A" . nil)
    ("/" . notmuch-search-filter) ; alias for l
    ("r" . notmuch-search-reply-to-thread) ; easier to reply to all by default
    ("R" . notmuch-search-reply-to-thread-sender)
    :map notmuch-show-mode-map
    ("a" . nil) ; the default is too easy to hit accidentally and I do not archive stuff
    ("A" . nil)
    ("r" . notmuch-show-reply) ; easier to reply to all by default
    ("R" . notmuch-show-reply-sender)
    :map notmuch-hello-mode-map
    ("C-<tab>" . nil)))

;;; My own tweaks for notmuch (prot-notmuch.el)
(use-package prot-notmuch
  :ensure nil
  :after notmuch
  :bind
  ( :map notmuch-search-mode-map
    ("D" . prot-notmuch-search-delete-thread)
    ("S" . prot-notmuch-search-spam-thread)
    ("g" . prot-notmuch-refresh-buffer)
    :map notmuch-show-mode-map
    ("D" . prot-notmuch-show-delete-message)
    ("S" . prot-notmuch-show-spam-message)
    :map notmuch-show-stash-map
    ("S" . prot-notmuch-stash-sourcehut-link))
  :config
  ;; Those are for the actions that are available after pressing 'k'
  ;; (`notmuch-tag-jump').  For direct actions, refer to the key
  ;; bindings below.
  (setq notmuch-tagging-keys
        `((,(kbd "d") prot-notmuch-mark-delete-tags "💥 Mark for deletion")
          (,(kbd "f") prot-notmuch-mark-flag-tags "🚩 Flag as important")
          (,(kbd "s") prot-notmuch-mark-spam-tags "🔥 Mark as spam")
          (,(kbd "r") ("-unread") "👁️‍🗨️ Mark as read")
          (,(kbd "u") ("+unread") "🗨️ Mark as unread")))

  ;; These emoji are purely cosmetic.  The tag remains the same: I
  ;; would not like to input emoji for searching.
  (add-to-list 'notmuch-tag-formats '("encrypted" (concat tag "🔒")))
  (add-to-list 'notmuch-tag-formats '("attachment" (concat tag "📎")))
  (add-to-list 'notmuch-tag-formats '("coach" (concat tag "🏆")))
  (add-to-list 'notmuch-tag-formats '("package" (concat tag "🗂️"))))

;;; Glue code for notmuch and org-link (ol-notmuch.el)
(use-package ol-notmuch
  :ensure t
  :after notmuch)

;;; notmuch-indicator (another package of mine)
(use-package notmuch-indicator
  :ensure t
  :after notmuch
  :config
  (setq notmuch-indicator-args
        '(( :terms "tag:unread and tag:inbox"
            :label "[A] "
            :label-face prot-modeline-indicator-green
            :counter-face prot-modeline-indicator-green)
          ( :terms "tag:unread and tag:inbox and not tag:package and not tag:coach"
            :label "[U] "
            :label-face prot-modeline-indicator-cyan
            :counter-face prot-modeline-indicator-cyan)
          ( :terms "tag:unread and tag:package and tag:inbox"
            :label "[P] "
            :label-face prot-modeline-indicator-magenta
            :counter-face prot-modeline-indicator-magenta)
          ( :terms "tag:unread and tag:coach and tag:inbox"
            :label "[C] "
            :label-face prot-modeline-indicator-red
            :counter-face prot-modeline-indicator-red))

        notmuch-indicator-refresh-count (* 60 3)
        notmuch-indicator-hide-empty-counters t
        notmuch-indicator-force-refresh-commands '(notmuch-refresh-this-buffer))

  ;; I control its placement myself.  See prot-emacs-modeline.el where
  ;; I set the `mode-line-format'.
  (setq notmuch-indicator-add-to-mode-line-misc-info nil)

  (notmuch-indicator-mode 1))

(provide 'prot-emacs-notmuch)
