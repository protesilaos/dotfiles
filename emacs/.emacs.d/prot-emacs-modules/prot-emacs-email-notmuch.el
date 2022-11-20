;; NOTE 2022-11-15: See prot-emacs-email.el for the general setup.
;; This file contains the coode specific to notmuch and is loaded from
;; the init.el conditionally if the notmuch executable is available.

;;; Notmuch (mail indexer and mail user agent (MUA))
;; I install notmuch from the distro's repos because the CLI program is
;; not dependent on Emacs.  Though the package also includes notmuch.el
;; which is what we use here (they are maintained by the same people).
(add-to-list 'load-path "/usr/share/emacs/site-lisp/")
(prot-emacs-builtin-package 'notmuch

;;; Account settings
  (let ((prv (prot-common-auth-get-field "prv" :user))
        (pub (prot-common-auth-get-field "pub" :user))
        (inf (prot-common-auth-get-field "inf" :user)))
    (setopt notmuch-identities
            (mapcar (lambda (str)
                      (format "%s <%s>" user-full-name str))
                    (list prv pub inf))
            notmuch-fcc-dirs
            `((,prv . "prv/Sent")
              (,inf . "inf/Sent")
              (,pub . "pub/Sent"))))

;;;; General UI
  (setopt notmuch-show-logo nil
          notmuch-column-control 1.0
          notmuch-hello-auto-refresh t
          notmuch-hello-recent-searches-max 20
          notmuch-hello-thousands-separator ""
          notmuch-hello-sections '(notmuch-hello-insert-saved-searches)
          notmuch-show-all-tags-list t)

;;;; Search
  (setopt notmuch-search-oldest-first nil
          notmuch-search-result-format
          '(("date" . "%12s  ")
            ("count" . "%-7s  ")
            ("authors" . "%-20s  ")
            ("subject" . "%-80s  ")
            ("tags" . "(%s)"))
          notmuch-tree-result-format
          '(("date" . "%12s  ")
            ("authors" . "%-20s  ")
            ((("tree" . "%s")
              ("subject" . "%s"))
             . " %-80s  ")
            ("tags" . "(%s)"))
          notmuch-search-line-faces
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
            ("flag" . italic))
          notmuch-show-empty-saved-searches t
          notmuch-saved-searches
          `(( :name "📥 inbox"
              :query "tag:inbox"
              :sort-order newest-first
              :key ,(kbd "i"))
            ( :name "📔 unread (inbox)"
              :query "tag:unread and tag:inbox"
              :sort-order newest-first
              :key ,(kbd "u"))
            ( :name "📯 unread all"
              :query "tag:unread not tag:archived"
              :sort-order newest-first
              :key ,(kbd "U"))
            ( :name "📦 My packages"
              :query "(from:~protesilaos/.*@lists.sr.ht or to:~protesilaos/.*@lists.sr.ht) not tag:archived not tag:list"
              :sort-order newest-first
              :key ,(kbd "p"))
            ( :name "📬 mailing lists"
              :query "tag:list not tag:archived"
              :sort-order newest-first
              :key ,(kbd "m"))
            ;; Emacs
            ( :name "🔨 emacs-devel"
              :query "(from:emacs-devel@gnu.org or to:emacs-devel@gnu.org) not tag:archived"
              :sort-order newest-first
              :key ,(kbd "e d"))
            ( :name "🦄 emacs-orgmode"
              :query "(from:emacs-orgmode@gnu.org or to:emacs-orgmode@gnu.org) not tag:archived"
              :sort-order newest-first
              :key ,(kbd "e o"))
            ( :name "🐛 emacs-bugs"
              :query "'to:\"/*@debbugs.gnu.org*/\"' not tag:archived"
              :sort-order newest-first :key ,(kbd "e b"))
            ( :name "📚 emacs-humanities"
              :query "(from:emacs-humanities@gnu.org or to:emacs-humanities@gnu.org) not tag:archived"
              :sort-order newest-first :key ,(kbd "e h"))
            ;; Others
            ( :name "📧 notmuch"
              :query "(from:notmuch@notmuchmail.org or to:notmuch@notmuchmail.org) not tag:archived"
              :sort-order newest-first
              :key ,(kbd "on"))
            ( :name "🛖 sourcehut"
              :query "(from:~sircmpwn/sr.ht-discuss@lists.sr.ht or to:~sircmpwn/sr.ht-discuss@lists.sr.ht) not tag:archived"
              :sort-order newest-first
              :key ,(kbd "os"))))

;;;; Tags
  (setopt notmuch-archive-tags nil ; I do not archive email
          notmuch-message-replied-tags '("+replied")
          notmuch-message-forwarded-tags '("+forwarded")
          notmuch-show-mark-read-tags '("-unread")
          notmuch-draft-tags '("+draft")
          notmuch-draft-folder "drafts"
          notmuch-draft-save-plaintext 'ask)

  ;; Also see `notmuch-tagging-keys' in the `prot-notmuch' section
  ;; further below.

  ;; FIXME 2022-09-29: `setopt' does not work for this one, even
  ;; though `setq' does the right thing.  Check the definition of
  ;; `notmuch-tag-format-type'.
  (setq notmuch-tag-formats
        '(("unread" (propertize tag 'face 'notmuch-tag-unread))
          ("flag" (propertize tag 'face 'notmuch-tag-flagged)
           (concat tag "🚩"))) ; the tag is still "flag"; the emoji is cosmetic
        notmuch-tag-deleted-formats
        '(("unread" (notmuch-apply-face bare-tag 'notmuch-tag-deleted)
           (concat "🚫" tag))
          (".*" (notmuch-apply-face tag 'notmuch-tag-deleted)
           (concat "🚫" tag)))
        notmuch-tag-added-formats
        '((".*" (notmuch-apply-face tag 'notmuch-tag-added)
           (concat "✏️" tag))))

;;;; Email composition
  (setopt notmuch-mua-compose-in 'current-window
          notmuch-mua-hidden-headers nil ; TODO 2021-05-12: Review hidden headers
          notmuch-address-command 'internal
          notmuch-always-prompt-for-sender t
          notmuch-mua-cite-function 'message-cite-original-without-signature
          notmuch-mua-reply-insert-header-p-function 'notmuch-show-reply-insert-header-p-never
          notmuch-mua-user-agent-function nil
          notmuch-maildir-use-notmuch-insert t
          notmuch-crypto-process-mime t
          notmuch-crypto-get-keys-asynchronously t
          notmuch-mua-attachment-regexp   ; see `notmuch-mua-send-hook'
          (concat "\\b\\(attache\?ment\\|attached\\|attach\\|"
                  "pi[èe]ce\s+jointe?\\|"
                  "συνημμ[εέ]νο\\|επισυν[αά]πτω\\)\\b"))

;;;; Reading messages
  (setopt notmuch-show-relative-dates t
          notmuch-show-all-multipart/alternative-parts nil
          notmuch-show-indent-messages-width 0
          notmuch-show-indent-multipart nil
          notmuch-show-part-button-default-action 'notmuch-show-view-part
          notmuch-show-text/html-blocked-images "." ; block everything
          notmuch-wash-wrap-lines-length 120
          notmuch-unthreaded-show-out nil
          notmuch-message-headers '("To" "Cc" "Subject" "Date")
          notmuch-message-headers-visible t)

  (let ((count most-positive-fixnum)) ; I don't like the buttonisation of long quotes
    (setopt notmuch-wash-cqitation-lines-prefix count
            notmuch-wash-citation-lines-suffix count))

;;;; Hooks and key bindings
  (add-hook 'notmuch-mua-send-hook #'notmuch-mua-attachment-check)
  (remove-hook 'notmuch-show-hook #'notmuch-show-turn-on-visual-line-mode)
  (remove-hook 'notmuch-search-hook 'notmuch-hl-line-mode) ; Check my `lin' package
  (add-hook 'notmuch-show-hook (lambda () (setq-local header-line-format nil)))

  (let ((map global-map))
    (define-key map (kbd "C-c m") #'notmuch)
    (define-key map (kbd "C-x m") #'notmuch-mua-new-mail)) ; override `compose-mail'
  (let ((map notmuch-search-mode-map)) ; I normally don't use the tree view, otherwise check `notmuch-tree-mode-map'
    (define-key map (kbd "/") #'notmuch-search-filter) ; alias for l
    (define-key map (kbd "r") #'notmuch-search-reply-to-thread) ; easier to reply to all by default
    (define-key map (kbd "R") #'notmuch-search-reply-to-thread-sender))
  (let ((map notmuch-show-mode-map))
    (define-key map (kbd "r") #'notmuch-show-reply) ; easier to reply to all by default
    (define-key map (kbd "R") #'notmuch-show-reply-sender))
  (define-key notmuch-hello-mode-map (kbd "C-<tab>") nil))

;;; My own tweaks for notmuch (prot-notmuch.el)
(prot-emacs-builtin-package 'prot-notmuch
  ;; Those are for the actions that are available after pressing 'k'
  ;; (`notmuch-tag-jump').  For direct actions, refer to the key
  ;; bindings below.
  (setopt notmuch-tagging-keys
          `((,(kbd "d") prot-notmuch-mark-delete-tags "⛔ Mark for deletion")
            (,(kbd "f") prot-notmuch-mark-flag-tags "🚩 Flag as important")
            (,(kbd "s") prot-notmuch-mark-spam-tags "⚠️ Mark as spam")
            (,(kbd "r") ("-unread") "✅ Mark as read")
            (,(kbd "u") ("+unread") "📔 Mark as unread")))

  (add-to-list 'notmuch-tag-formats '("encrypted" (concat tag "🔒"))) ; cosmetic emoji, tag is the same
  (add-to-list 'notmuch-tag-formats '("attachment" (concat tag "📎"))) ; cosmetic emoji, tag is the same

  (dolist (fn '(prot-notmuch-check-valid-sourcehut-email
                prot-notmuch-ask-sourcehut-control-code))
    (add-hook 'notmuch-mua-send-hook fn))

  (let ((map notmuch-search-mode-map))
    (define-key map (kbd "a") nil) ; the default is too easy to hit accidentally
    (define-key map (kbd "A") #'notmuch-search-archive-thread)
    (define-key map (kbd "D") #'prot-notmuch-search-delete-thread)
    (define-key map (kbd "S") #'prot-notmuch-search-spam-thread)
    (define-key map (kbd "g") #'prot-notmuch-refresh-buffer))
  (let ((map notmuch-show-mode-map))
    (define-key map (kbd "a") nil) ; the default is too easy to hit accidentally
    (define-key map (kbd "A") #'notmuch-show-archive-message-then-next-or-next-thread)
    (define-key map (kbd "D") #'prot-notmuch-show-delete-message)
    (define-key map (kbd "S") #'prot-notmuch-show-spam-message))
  (define-key notmuch-show-stash-map (kbd "S") #'prot-notmuch-stash-sourcehut-link)
  ;; Like C-c M-h for `message-insert-headers'
  (define-key notmuch-message-mode-map (kbd "C-c M-e") #'prot-notmuch-patch-add-email-control-code))

;;; Glue code for notmuch and org-link (ol-notmuch.el)
(prot-emacs-elpa-package 'ol-notmuch)

;;; New backend for notmuch address completion (notmuch-addr.el)
;; Per the project's documentation, we need to make sure
;; `notmuch-address' is loaded before setting up the `notmuch-addr'
;; alternative.
(with-eval-after-load 'notmuch-address
  (prot-emacs-elpa-package 'notmuch-addr
    (notmuch-addr-setup)))         ; NOTE 2022-09-22: I am testing this.

;;; notmuch-indicator (another package of mine)
(prot-emacs-elpa-package 'notmuch-indicator
  ;; Just the default values...
  (setopt notmuch-indicator-args
          '((:terms "tag:unread and tag:inbox" :label "@") ; also accepts a :face, read doc string
            (:terms "to:~protesilaos/.*@lists.sr.ht tag:unread not tag:archived not tag:list" :label "📦"))
          notmuch-indicator-refresh-count (* 60 3)
          notmuch-indicator-hide-empty-counters t
          notmuch-indicator-force-refresh-commands '(notmuch-refresh-this-buffer))

  (notmuch-indicator-mode 1))

(provide 'prot-emacs-email-notmuch)
