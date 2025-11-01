;;;; File with authentication credentials (`auth-source')
(prot-emacs-configure
  (setq auth-sources '("~/.authinfo.gpg")
        user-full-name "Protesilaos Stavrou"
        user-mail-address "prot@protesilaos.com"))

;;;; Encryption settings (`mm-encode' and `mml-sec')
(prot-emacs-configure
  (setq mm-encrypt-option nil ; use 'guided for both if you need more control
        mm-sign-option nil)

  (setq mml-secure-openpgp-encrypt-to-self t
        mml-secure-openpgp-sign-with-sender t
        mml-secure-smime-encrypt-to-self t
        mml-secure-smime-sign-with-sender t)

;;;; Message composition (`message')

  (add-hook 'message-setup-hook #'message-sort-headers)

  (setq mail-user-agent 'message-user-agent
        message-mail-user-agent t) ; use `mail-user-agent'
  (setq mail-header-separator "--text follows this line--")
  (setq message-elide-ellipsis "\n> [... %l lines elided]\n")
  (setq compose-mail-user-agent-warnings nil)
  (setq message-signature "Protesilaos Stavrou\nhttps://protesilaos.com\n"
        mail-signature message-signature)
  (setq message-citation-line-function #'message-insert-formatted-citation-line)
  (setq message-citation-line-format (concat "> From: %f\n"
                                             "> Date: %a, %e %b %Y %T %z\n"
                                             ">")
        message-ignored-cited-headers "") ; default is "." for all headers
  (setq message-confirm-send nil)
  (setq message-kill-buffer-on-exit t)
  ;; (add-to-list 'mm-body-charset-encoding-alist '(utf-8 . base64))
  (setq message-wide-reply-confirm-recipients nil))

;;;; Add attachments from Dired (`gnus-dired' does not require `gnus')
(prot-emacs-configure
  (add-hook 'dired-mode-hook #'turn-on-gnus-dired-mode))

;;;; `sendmail' (mail transfer agent)
(prot-emacs-configure
  (with-eval-after-load 'message
    (setq send-mail-function #'smtpmail-send-it)
    (setq smtpmail-smtp-server "mail.gandi.net")
    (setq smtpmail-smtp-service 587)
    (setq smtpmail-stream-type 'starttls)))

(when (executable-find "notmuch")
  (require 'prot-emacs-notmuch))

(provide 'prot-emacs-email)
