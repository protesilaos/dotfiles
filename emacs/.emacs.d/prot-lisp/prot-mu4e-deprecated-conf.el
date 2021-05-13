;; ;; THIS IS DEPRECATED CODE THAT I KEEP HERE FOR FUTURE REFERENCE.
;; ;; Its last state was recovered from my dotfiles' commit f82b3e0 on
;; ;; 2021-01-30.

;; This is an exception because I install it from the Arch Linux
;; package archives (depends on non-Emacs code)
(add-to-list 'load-path "/usr/share/emacs/site-lisp/")
(require 'mu4e)
;; (setq mail-user-agent 'mu4e-user-agent)
(setq mu4e-use-fancy-chars nil)
(setq mu4e-headers-advance-after-mark t)
(setq mu4e-headers-auto-update t)
(setq mu4e-headers-date-format "%F")
(setq mu4e-headers-time-format "%R")
(setq mu4e-headers-long-date-format "%F, %R")

(setq mu4e-headers-fields
      '((:human-date . 12)
        (:flags . 6)
        (:mailing-list . 10)
        (:from . 22)
        (:subject)))

(setq mu4e-get-mail-command "true")
(setq mu4e-hide-index-messages t)
(setq mu4e-update-interval (* 60 5))
(setq mu4e-completing-read-function 'completing-read)
(setq mu4e-compose-signature "Protesilaos Stavrou\nprotesilaos.com\n")
(setq mu4e-compose-signature-auto-include t)
(setq mu4e-maildir "~/.mail")
(setq mu4e-attachment-dir "~/Downloads")
(setq mu4e-sent-messages-behavior 'sent)
(setq mu4e-view-show-addresses t)
(setq mu4e-context-policy 'pick-first)
(setq mu4e-compose-context-policy 'ask)
(setq mu4e-index-lazy-check t)
(setq mu4e-change-filenames-when-moving t) ; better for `mbsync'?
(setq mu4e-modeline-max-width 30)
(setq mu4e-display-update-status-in-modeline t)
(setq mu4e-view-show-images nil)
(setq mu4e-decryption-policy 'ask)

(setq mu4e-contexts
      `(,(make-mu4e-context
          :name "vrp" ; Is there no way to specify a key for switching?
          :enter-func (lambda () (mu4e-message "Entering PRV"))
          :leave-func (lambda () (mu4e-message "Leaving PRV"))
          :match-func (lambda (msg)
                        (when msg
                          (mu4e-message-contact-field-matches
                           msg :to (prot-mail-auth-get-field "prv" :user))))
          :vars `((user-mail-address . ,(prot-mail-auth-get-field "prv" :user))))
        ,(make-mu4e-context
          :name "inf"
          :match-func (lambda (msg)
                        (when msg
                          (mu4e-message-contact-field-matches
                           msg :to (prot-mail-auth-get-field "inf" :user))))
          :vars `((user-mail-address . ,(prot-mail-auth-get-field "inf" :user))))
        ,(make-mu4e-context
          :name "pub"
          :match-func (lambda (msg)
                        (when msg
                          (mu4e-message-contact-field-matches
                           msg :to (prot-mail-auth-get-field "pub" :user))))
          :vars `((user-mail-address . ,(prot-mail-auth-get-field "pub" :user))))))

(setq mu4e-bookmarks
      '((:name "Unread messages" :query "g:unread AND NOT g:trashed" :key ?u)
        (:name "Today's messages" :query "d:today..now" :key ?t)
        (:name "Last 7 days" :query "d:7d..now" :key ?w)
        (:name "PRV Unread"
               :query `,(format "to:%s %s"
                                (prot-mail-auth-get-field "prv" :user)
                                "AND g:unread AND NOT g:trashed")
               :key ?v)
        (:name "PRV Inbox"
               :query `,(format "to:%s"
                                (prot-mail-auth-get-field "prv" :user))
               :key ?V)
        (:name "INF Unread"
               :query `,(format "to:%s %s"
                                (prot-mail-auth-get-field "inf" :user)
                                "AND g:unread AND NOT g:trashed")
               :key ?i)
        (:name "INF Inbox"
               :query `,(format "to:%s"
                                (prot-mail-auth-get-field "inf" :user))
               :key ?I)
        (:name "PUB Unread"
               :query `,(format "to:%s %s"
                                (prot-mail-auth-get-field "pub" :user)
                                "AND g:unread AND NOT g:trashed")
               :key ?p)
        (:name "PUB Inbox"
               :query `,(format "to:%s"
                                (prot-mail-auth-get-field "pub" :user))
               :key ?P)))

(define-key global-map (kbd "C-c M") #'mu4e)

(let ((map mu4e-headers-mode-map))
  ;; TODO 2021-01-28: Add wrapper for `mu4e-headers-mark-for-unflag'
  (define-key map (kbd "!") (lambda (&optional arg)
                              (interactive "P")
                              (if arg
                                  (mu4e-headers-mark-for-unflag)
                                (mu4e-headers-mark-for-flag))))
  (define-key map (kbd "r") #'mu4e-headers-mark-for-read)
  (define-key map (kbd "u") #'mu4e-headers-mark-for-unread)
  (define-key map (kbd "M-u") #'mu4e-headers-mark-for-unmark)
  (define-key map (kbd "C-M-u") #'mu4e-mark-unmark-all))
