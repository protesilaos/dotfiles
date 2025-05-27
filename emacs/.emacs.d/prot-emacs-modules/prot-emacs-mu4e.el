;; This is deprecated code as I stopped using mu4e a long time ago.  I
;; keep it here for when I need to test it for my themes (`ef-themes',
;; `modus-themes', `standard-themes', `doric-themes').

(use-package mu4e
  :ensure nil
  ;; This is an exception because I install it from the system
  ;; distribution's package archives (depends on non-Emacs code)
  :load-path "/usr/share/emacs/site-lisp/elpa-src/mu4e-1.8.14/"
  :config
  (setq mu4e-maildir "~/.20240226-mail-mu4e")
  (setq mu4e-attachment-dir (expand-file-name "~/Downloads/mail-attachments/"))
  (setq mu4e-confirm-quit nil)
  (setq mu4e-mu-allow-temp-file t) ; mu 1.12.0

  (setq mu4e-use-fancy-chars t ; Cool idea, but they create misalignments
        mu4e-headers-draft-mark     '("D" . "⚒️")
        mu4e-headers-flagged-mark   '("F" . "🚩")
        mu4e-headers-new-mark       '("N" . "🔥")
        mu4e-headers-passed-mark    '("P" . "📨")
        mu4e-headers-replied-mark   '("R" . "✏️")
        mu4e-headers-seen-mark      '("S" . "👁️‍🗨️")
        mu4e-headers-trashed-mark   '("T" . "🚫")
        mu4e-headers-attach-mark    '("a" . "📎")
        mu4e-headers-encrypted-mark '("x" . "🔒")
        mu4e-headers-signed-mark    '("s" . "🔑")
        mu4e-headers-unread-mark    '("u" . "💬")
        mu4e-headers-list-mark      '("l" . "📬")
        mu4e-headers-personal-mark  '("p" . "🦚")
        mu4e-headers-calendar-mark  '("c" . "📅"))

  (setq mu4e-marks
        '((refile
           :char ("r" . "▶")
           :prompt "refile"
           :dyn-target (lambda (target msg) (mu4e-get-refile-folder msg))
           :action (lambda (docid msg target)
                     (mu4e--server-move docid (mu4e--mark-check-target target) "-N")))
          (delete
           :char ("D" . "🚫")
           :prompt "Delete"
           :show-target (lambda (target) "delete")
           :action (lambda (docid msg target) (mu4e--server-remove docid)))
          (flag
           :char ("+" . "🚩")
           :prompt "+flag"
           :show-target (lambda (target) "flag")
           :action (lambda (docid msg target)
                     (mu4e--server-move docid nil "+F-u-N")))
          (move
           :char ("m" . "▷")
           :prompt "move"
           :ask-target  mu4e--mark-get-move-target
           :action (lambda (docid msg target)
                     (mu4e--server-move docid (mu4e--mark-check-target target) "-N")))
          (read
           :char    ("!" . "👁️‍🗨️")
           :prompt "!read"
           :show-target (lambda (target) "read")
           :action (lambda (docid msg target) (mu4e--server-move docid nil "+S-u-N")))
          (trash
           :char ("d" . "🚫")
           :prompt "dtrash"
           :dyn-target (lambda (target msg) (mu4e-get-trash-folder msg))
           :action (lambda (docid msg target)
                     (mu4e--server-move docid
                                        (mu4e--mark-check-target target) "+T-N")))
          (unflag
           :char    ("-" . "➖")
           :prompt "-unflag"
           :show-target (lambda (target) "unflag")
           :action (lambda (docid msg target) (mu4e--server-move docid nil "-F-N")))
          (untrash
           :char   ("=" . "▲")
           :prompt "=untrash"
           :show-target (lambda (target) "untrash")
           :action (lambda (docid msg target) (mu4e--server-move docid nil "-T")))
          (unread
           :char    ("?" . "💬")
           :prompt "?unread"
           :show-target (lambda (target) "unread")
           :action (lambda (docid msg target) (mu4e--server-move docid nil "-S+u-N")))
          (unmark
           :char  " "
           :prompt "unmark"
           :action (mu4e-error "No action for unmarking"))
          (action
           :char ( "a" . "◯")
           :prompt "action"
           :ask-target  (lambda () (mu4e-read-option "Action: " mu4e-headers-actions))
           :action  (lambda (docid msg actionfunc)
                      (save-excursion
                        (when (mu4e~headers-goto-docid docid)
                          (mu4e-headers-action actionfunc)))))
          (something
           :char  ("*" . "✱")
           :prompt "*something"
           :action (mu4e-error "No action for deferred mark"))))

  (setq mu4e-modeline-support t
        mu4e-modeline-unread-items '("U:" . "[U]")
        mu4e-modeline-all-read '("R:" . "[R]")
        mu4e-modeline-all-clear '("C:" . "[C]")
        mu4e-modeline-max-width 42)

  (setq mu4e-notification-support t
        ;; TODO 2024-02-26: Write custom mu4e notification function.
        mu4e-notification-filter #'mu4e--default-notification-filter)

  (setq mu4e-headers-advance-after-mark nil)
  (setq mu4e-headers-auto-update t)
  (setq mu4e-headers-date-format "%F %a, %T")
  (setq mu4e-headers-time-format "%R")
  (setq mu4e-headers-long-date-format "%F, %R")
  (setq mu4e-headers-leave-behavior 'apply)

  (setq mu4e-headers-fields
        '((:date . 26)
          (:flags . 8)
          (:from . 20)
          (:subject)))

  (setq mu4e-get-mail-command "true" ; I auto-fetch with a systemd timer
        mu4e-update-interval nil)
  (setq mu4e-hide-index-messages t)

  (setq mu4e-read-option-use-builtin nil
        mu4e-completing-read-function 'completing-read)

  (setq mu4e-search-results-limit -1
        mu4e-search-sort-field :date
        mu4e-search-sort-direction 'descending)

  (setq mu4e-org-support t)

  (setq mu4e-sent-messages-behavior 'sent)
  (setq mu4e-view-show-addresses t)
  (setq mu4e-split-view 'horizontal)

  (setq mu4e-index-lazy-check t)
  (setq mu4e-change-filenames-when-moving t) ; better for `mbsync'?
  (setq mu4e-display-update-status-in-modeline nil)
  (setq mu4e-view-show-images nil)
  (setq mu4e-headers-include-related nil)
  (setq mu4e-view-auto-mark-as-read t)

  (setq mu4e-compose-complete-addresses nil
        mu4e-compose-complete-only-personal t)

  (setq mu4e-compose-signature "Protesilaos Stavrou\nprotesilaos.com\n")
  (setq mu4e-compose-signature-auto-include t)

  (setq mu4e-context-policy 'pick-first
        mu4e-compose-context-policy nil)

  (setq mu4e-contexts
        `(,@(mapcar
             (lambda (context)
               (let ((address (prot-common-auth-get-field context :user)))
                 (make-mu4e-context
                  :name context
                  :match-func `(lambda (msg)
                                 (when msg
                                   (mu4e-message-contact-field-matches msg :to ,address)))
                  :vars `((user-mail-address . ,address)
                          (mu4e-trash-folder . ,(format "/%s/Trash" context))
                          (mu4e-sent-folder . ,(format "/%s/Sent" context))))))
             '("pub" "inf" "prv"))))

  ;; 2024-02-26 10:34 +0200 WORK-IN-PROGRESS
  (setq mu4e-bookmarks
        `((:name "All unread messages" :query "g:unread AND NOT g:trashed" :key ?a)
          (:name "All messages" :query "*" :key ?A)
          ( :name "Personal unread"
            :query "contact:/@protesilaos/ or contact:protesilaos AND g:unread AND NOT contact:/@.*gnu/"
            :key ?u
            :favorite t)
          ( :name "Personal inbox"
            :query "contact:/@protesilaos/ or contact:protesilaos AND NOT contact:/@.*gnu/"
            :key ?U
            :favorite t)
          ( :name "Mailing list unread"
            :query "contact:/@.*gnu/ AND g:unread"
            :key ?m)
          ( :name "Mailing list inbox"
            :query "contact:/@.*gnu/"
            :key ?M)))

  (defun prot/mu4e (&rest args)
    (cl-letf (((symbol-function 'display-buffer-full-frame) #'display-buffer-same-window))
      (apply args)))

  (advice-add #'mu4e-display-buffer :around #'prot/mu4e)

  (defun prot/mu4e-no-header-line ()
    (setq-local header-line-format nil))

  (add-hook 'mu4e-headers-mode-hook #'prot/mu4e-no-header-line)

  (prot-emacs-keybind global-map
    "C-c m" #'mu4e
    "C-x m" #'mu4e-compose-new) ; override `compose-mail'

  (prot-emacs-keybind mu4e-headers-mode-map
    "!" #'mu4e-headers-mark-for-flag
    "?" #'mu4e-headers-mark-for-unflag
    "r" #'mu4e-headers-mark-for-read
    "d" #'mu4e-headers-mark-for-delete ; I do not use the trash
    "u" #'mu4e-headers-mark-for-unread
    "m" #'mu4e-headers-mark-for-unmark
    "M" #'mu4e-mark-unmark-all)

  (prot-emacs-keybind mu4e-headers-mode-map
    "!" #'mu4e-headers-mark-for-flag
    "?" #'mu4e-headers-mark-for-unflag
    "r" #'mu4e-headers-mark-for-read
    "u" #'mu4e-headers-mark-for-unread
    "m" #'mu4e-headers-mark-for-unmark
    "M" #'mu4e-mark-unmark-all)

  (prot-emacs-keybind mu4e-view-mode-map
    ;; NOTE 2024-02-26: with mu 1.12.0 there is a "wide" reply that I
    ;; would bind to r and the regular reply to R.
    "r" #'mu4e-compose-reply
    "w" #'mu4e-copy-thing-at-point
    "s" #'mu4e-view-save-attachments
    "S" #'mu4e-view-raw-message ; "source" mnemonic
    "u" #'mu4e-view-mark-for-unread
    "U" #'mu4e-view-mark-for-read
    "d" #'mu4e-view-mark-for-delete ; overwrite mu4e-view-mark-for-trash
    "!" #'mu4e-view-mark-for-flag
    "?" #'mu4e-view-mark-for-unflag))

(provide 'prot-emacs-mu4e)
