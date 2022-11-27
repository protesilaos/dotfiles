;;; Client-agnostic email settings
(prot-emacs-builtin-package 'auth-source
  (setq auth-sources '("~/.authinfo.gpg")
          user-full-name "Protesilaos Stavrou"
          user-mail-address "public@protesilaos.com"))

(prot-emacs-builtin-package 'mm-encode
  (setq mm-encrypt-option nil ; use 'guided if you need more control
          mm-sign-option nil))  ; same

(prot-emacs-builtin-package 'mml-sec
  (setq mml-secure-openpgp-encrypt-to-self t
          mml-secure-openpgp-sign-with-sender t
          mml-secure-smime-encrypt-to-self t
          mml-secure-smime-sign-with-sender t))

(prot-emacs-builtin-package 'message
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
  (add-to-list 'mm-body-charset-encoding-alist '(utf-8 . base64))

  (add-hook 'message-setup-hook #'message-sort-headers))

(prot-emacs-builtin-package 'gnus-dired ; does not require `gnus'
  (add-hook 'dired-mode-hook #'gnus-dired-mode))

;;; Sending email (SMTP)
(prot-emacs-builtin-package 'smtpmail
  (setq smtpmail-default-smtp-server "mail.gandi.net"
          smtpmail-smtp-server "mail.gandi.net"
          smtpmail-stream-type 'ssl
          smtpmail-smtp-service 465
          smtpmail-queue-mail nil))

(prot-emacs-builtin-package 'sendmail
  (setq send-mail-function 'smtpmail-send-it))

(provide 'prot-emacs-email)
