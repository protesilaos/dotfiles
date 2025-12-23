;;;; `project'
(prot-emacs-configure
  (prot-emacs-keybind global-map
    "C-x p ." #'project-dired
    "C-x p C-g" #'keyboard-quit
    "C-x p <return>" #'project-dired
    "C-x p <delete>" #'project-forget-project)

  (setopt project-switch-commands
          '((project-find-file "Find file")
            (project-find-regexp "Find regexp")
            (project-find-dir "Find directory")
            (project-dired "Root dired")
            (project-vc-dir "VC-Dir")
            (project-shell "Shell")
            (keyboard-quit "Quit")))
  (setq project-vc-extra-root-markers '(".project")) ; Emacs 29
  (setq project-key-prompt-style t) ; Emacs 30

  (advice-add #'project-switch-project :after #'prot-common-clear-minibuffer-message)

  (autoload #'prot-project-maybe-in-tab "prot-project")
  (define-key project-prefix-map (kbd "p") #'prot-project-maybe-in-tab))

;;;; `diff-mode'
(prot-emacs-configure
  (setq diff-default-read-only t)
  (setq diff-advance-after-apply-hunk t)
  (setq diff-update-on-the-fly t)
  ;; The following are from Emacs 27.1
  (setq diff-refine nil) ; I do it on demand, with my `agitate' package (more below)
  (setq diff-font-lock-prettify t) ; I think nil is better for patches, but let me try this for a while
  (setq diff-font-lock-syntax nil))

;;;; `ediff'
(prot-emacs-configure
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-keep-variants nil)
  (setq ediff-make-buffers-readonly-at-startup nil)
  (setq ediff-merge-revisions-with-ancestor t)
  (setq ediff-show-clashes-only t)

  (prot-emacs-autoload
    (prot-ediff-visible-buffers-2
     prot-ediff-visible-buffers-3
     prot-ediff-store-layout
     prot-ediff-restore-layout)
    "prot-ediff")
  ;; The C-x v prefix is for all "version control" commands that are
  ;; already built into Emacs.  It makes sense to extend it for this
  ;; use-case.
  (prot-emacs-keybind global-map
    "C-x v 2" #'prot-ediff-visible-buffers-2
    "C-x v 3" #'prot-ediff-visible-buffers-3)
  (add-hook 'ediff-before-setup-hook #'prot-ediff-store-layout)
  (add-hook 'ediff-quit-hook #'prot-ediff-restore-layout))

;;;; `smerge-mode'
(prot-emacs-configure
  (setq smerge-diff-buffer-name "*smerge-diff*")
  (setq smerge-refine-shadow-cursor nil)) ; Emacs 31

;;; Version control framework (vc.el, vc-git.el, and more)
(prot-emacs-configure
  (setq vc-follow-symlinks t)

  (with-eval-after-load 'vc
    ;; Those offer various types of functionality, such as blaming,
    ;; viewing logs, showing a dedicated buffer with changes to affected
    ;; files.
    (require 'vc-annotate)
    (require 'vc-dir)
    (require 'vc-git)
    (require 'add-log)
    (require 'log-view)

    ;; NOTE: I override lots of the defaults
    (prot-emacs-keybind global-map
      "C-x v B" #'vc-annotate ; Blame mnemonic
      "C-x v e" #'vc-ediff
      "C-x v k" #'vc-delete-file ; 'k' for kill==>delete is more common
      "C-x v G" #'vc-log-search  ; git log --grep
      "C-x v t" #'vc-create-tag
      "C-x v c" #'vc-clone ; Emacs 31
      "C-x v d" #'vc-diff
      "C-x v ." #'vc-dir-root ; `vc-dir-root' is from Emacs 28
      "C-x v <return>" #'vc-dir-root)
    (prot-emacs-keybind vc-dir-mode-map
      "t" #'vc-create-tag
      "O" #'vc-log-outgoing
      "o" #'vc-dir-find-file-other-window
      "d" #'vc-diff ; parallel to D: `vc-root-diff'
      "k" #'vc-dir-delete-file
      "G" #'vc-revert)
    (prot-emacs-keybind vc-git-stash-shared-map
      "a" #'vc-git-stash-apply-at-point
      "c" #'vc-git-stash ; "create" named stash
      "k" #'vc-git-stash-delete-at-point ; symmetry with `vc-dir-delete-file'
      "p" #'vc-git-stash-pop-at-point
      "s" #'vc-git-stash-snapshot)
    (prot-emacs-keybind vc-annotate-mode-map
      "M-q" #'vc-annotate-toggle-annotation-visibility
      "C-c C-c" #'vc-annotate-goto-line
      "<return>" #'vc-annotate-find-revision-at-line)
    (prot-emacs-keybind log-edit-mode-map
      "M-s" #'nil ; I use M-s for my search commands
      "M-r" #'nil) ; I use `consult-history'
    (prot-emacs-keybind log-view-mode-map
      "<tab>" #'log-view-toggle-entry-display
      "<return>" #'log-view-find-revision
      "s" #'vc-log-search
      "o" #'vc-log-outgoing
      "f" #'vc-log-incoming
      "F" #'vc-update
      "P" #'vc-push)

    ;; I only use Git.  If I ever need another, I will include it here.
    ;; This may have an effect on performance, as Emacs will not try to
    ;; check for a bunch of backends.
    (setq vc-handled-backends '(Git))

    (setq vc-dir-save-some-buffers-on-revert t) ; Emacs 31

    ;; This one is for editing commit messages.
    (require 'log-edit)
    (setq log-edit-confirm 'changed)
    (setq log-edit-keep-buffer nil)
    (setq log-edit-require-final-newline t)
    (setq log-edit-setup-add-author nil)

    (prot-emacs-hook
      log-edit-hook
      (log-edit-insert-message-template
       log-edit-maybe-show-diff)
      nil
      log-edit)

    (setq vc-find-revision-no-save t)
    (setq vc-annotate-display-mode 'scale) ; scale to oldest
    ;; I use a different account for git commits
    (setq add-log-mailing-address "info@protesilaos.com")
    (setq add-log-keep-changes-together t)
    (setq vc-git-diff-switches '("--patch-with-stat" "--histogram"))
    (setq vc-git-log-switches '("--stat"))
    (setq vc-git-print-log-follow t)
    (setq vc-git-revision-complete-only-branches nil) ; Emacs 28
    (setq vc-git-root-log-format
          `("%d %h %ai %an: %s"
            ;; The first shy group matches the characters drawn by --graph.
            ;; We use numbered groups because `log-view-message-re' wants the
            ;; revision number to be group 1.
            ,(concat "^\\(?:[*/\\|]+\\)\\(?:[*/\\| ]+\\)?"
                     "\\(?2: ([^)]+) \\)?\\(?1:[0-9a-z]+\\) "
                     "\\(?4:[0-9]\\{4\\}-[0-9-]\\{4\\}[0-9\s+:-]\\{16\\}\\) "
                     "\\(?3:.*?\\):")
            ((1 'log-view-message)
             (2 'change-log-list nil lax)
             (3 'change-log-name)
             (4 'change-log-date))))

    ;; These two are from Emacs 29
    (setq vc-git-log-edit-summary-target-len 50)
    (setq vc-git-log-edit-summary-max-len 70)))

;;; Interactive and powerful git front-end (Magit)
(prot-emacs-configure
  (setq transient-show-popup 0.5)

  (prot-emacs-install magit)

  ;; Let `display-buffer-alist' do its job
  (setq magit-display-buffer-function #'display-buffer)

  (define-key global-map (kbd "C-c g") #'magit-status)

  (with-eval-after-load 'magit
    (prot-emacs-keybind magit-mode-map
      "C-w" #'nil
      "M-w" #'nil))

  (setq magit-define-global-key-bindings nil)
  (setq magit-section-visibility-indicators
        `(;; (magit-fringe-bitmap> . magit-fringe-bitmapv)
          (" ▼" . t))) ; same as `org-ellipsis'

  ;; Show icons for files in the Magit status and other buffers.  This
  ;; is from my prot-icons.el.
  (with-eval-after-load 'prot-icons
    (setq magit-format-file-function
          (lambda (_kind file face &rest _)
            (let ((icon (prot-icons-get-file-icon file)))
              (format "%s %s" icon (propertize file 'font-lock-face face))))))

  (setq magit-diff-refine-hunk t)
  (setq magit-diff-refine-ignore-whitespace t)

  (setq magit-log-auto-more t)

  (setq magit-repository-directories
        '(("~/Git/Projects" . 1)))
  (setq magit-repolist-columns
        `(("Name" 25 ,#'magit-repolist-column-ident)
          ("Version" 15 ,#'magit-repolist-column-version
           ((:sort magit-repolist-version<)))
          ("Unpulled" 10 ,#'magit-repolist-column-unpulled-from-upstream
           ((:help-echo "Upstream changes not in branch")
            (:right-align t)
            (:sort <)))
          ("Unpushed" 10 ,#'magit-repolist-column-unpushed-to-upstream
           ((:help-echo "Local changes not in upstream")
            (:right-align t)
            (:sort <)))
          ("Path" 99 ,#'magit-repolist-column-path)))

  (setq git-commit-summary-max-length 50)
  ;; NOTE 2023-01-24: I used to also include `overlong-summary-line'
  ;; in this list, but I realised I do not need it.  My summaries are
  ;; always in check.  When I exceed the limit, it is for a good
  ;; reason.
  (setq git-commit-style-convention-checks '(non-empty-second-line))
  (setq git-commit-major-mode #'text-mode))

(provide 'prot-emacs-git)
