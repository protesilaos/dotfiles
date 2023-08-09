;;; Client-agnostic email settings
(prot-emacs-configure
  (:delay 1)
;;;; File with authentication credentials (`auth-source')
  (setq auth-sources '("~/.authinfo.gpg")
        user-full-name "Protesilaos Stavrou"
        user-mail-address "public@protesilaos.com")

;;;; Encoding settings (`mm-encode')
  (setq mm-encrypt-option nil ; use 'guided if you need more control
        mm-sign-option nil)  ; same

;;;; Encryption settings (`mml-sec')
  (setq mml-secure-openpgp-encrypt-to-self t
        mml-secure-openpgp-sign-with-sender t
        mml-secure-smime-encrypt-to-self t
        mml-secure-smime-sign-with-sender t)

;;;; Message composition (`message')
  (setq mail-user-agent 'message-user-agent
        mail-header-separator (purecopy "*****")
        message-elide-ellipsis "\n> [... %l lines elided]\n"
        compose-mail-user-agent-warnings nil
        message-mail-user-agent t      ; use `mail-user-agent'
        mail-signature "Protesilaos Stavrou\nhttps://protesilaos.com\n"
        message-signature "Protesilaos Stavrou\nhttps://protesilaos.com\n"
        message-citation-line-function #'message-insert-formatted-citation-line
        message-citation-line-format (concat "> From: %f\n"
                                             "> Date: %a, %e %b %Y %T %z\n"
                                             ">")
        message-ignored-cited-headers "" ; default is "." for all headers
        message-confirm-send nil
        message-kill-buffer-on-exit t
        message-wide-reply-confirm-recipients t)
  ;; (add-to-list 'mm-body-charset-encoding-alist '(utf-8 . base64))

  (add-hook 'message-setup-hook #'message-sort-headers)

;;;; Add attachments from Dired (`gnus-dired' does not require `gnus')
  (add-hook 'dired-mode-hook #'turn-on-gnus-dired-mode)

;;;; `smtpmail' (SMTP)
  ;; ;; FIXME 2023-01-26: Do I need any of this?  It seems that the
  ;; ;; contents of the `auth-sources' suffice for this case and
  ;; ;; smtpmail.el is set up to do the right thing out-of-the-box.
  ;; ;; Setting the values here seems wrong for anyone with multiple
  ;; ;; acounts from different service providers.
  (setq smtpmail-default-smtp-server "mail.gandi.net"
        smtpmail-smtp-server "mail.gandi.net"
        smtpmail-stream-type 'ssl
        smtpmail-smtp-service 465
        smtpmail-queue-mail nil)

;;;; `sendmail' (mail transfer agent)
  (setq send-mail-function 'smtpmail-send-it))

(provide 'prot-emacs-email)
