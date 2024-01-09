;;; Client-agnostic email settings
(prot-emacs-configure
  (:delay 1)

;;;; File with authentication credentials (`auth-source')
  (setq auth-sources '("~/.authinfo.gpg")
        user-full-name "Protesilaos Stavrou"
        user-mail-address "public@protesilaos.com")

;;;; Encryption settings (`mm-encode' and `mml-sec')
  (setq mm-encrypt-option nil ; use 'guided for both if you need more control
        mm-sign-option nil)

  (setq mml-secure-openpgp-encrypt-to-self t
        mml-secure-openpgp-sign-with-sender t
        mml-secure-smime-encrypt-to-self t
        mml-secure-smime-sign-with-sender t)

;;;; Message composition (`message')
  (setq mail-user-agent 'message-user-agent
        message-mail-user-agent t) ; use `mail-user-agent'
  (setq mail-header-separator "*****")
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
  (setq message-wide-reply-confirm-recipients t)
  ;; (add-to-list 'mm-body-charset-encoding-alist '(utf-8 . base64))

  (add-hook 'message-setup-hook #'message-sort-headers)

;;;; Add attachments from Dired (`gnus-dired' does not require `gnus')
  (add-hook 'dired-mode-hook #'turn-on-gnus-dired-mode)

;;;; `sendmail' (mail transfer agent)
  (setq send-mail-function 'sendmail-send-it
        ;; ;; NOTE 2023-08-08: We do not need this if we have the Arch
        ;; ;; Linux `msmtp-mta' package installed: it replaces the
        ;; ;; generic sendmail executable with msmtp.
        ;;
        ;; sendmail-program (executable-find "msmtp")
        message-sendmail-envelope-from 'header))

(when (executable-find "notmuch")
  (require 'prot-emacs-notmuch))

(provide 'prot-emacs-email)
