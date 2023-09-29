;;; General Git/Project/Diff configurations
(prot-emacs-configure
  (:delay 5)
;;;; `ediff'
  (setq ediff-keep-variants nil)
  (setq ediff-make-buffers-readonly-at-startup nil)
  (setq ediff-merge-revisions-with-ancestor t)
  (setq ediff-show-clashes-only t)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)

;;;; `project'
  (setopt project-switch-commands
          '((project-find-file "Find file")
            (project-find-regexp "Find regexp")
            (project-find-dir "Find directory")
            (project-dired "Root dired")
            (project-vc-dir "VC-Dir")
            (project-shell "Shell")
            (keyboard-quit "Quit")))
  (setq project-vc-extra-root-markers '(".project")) ; Emacs 29

  (defun prot/project--get-key (key cmd)
    "Return key binding for KEY, falling back to what CMD is bound to."
    (if key
        (vector key)
      (where-is-internal cmd (list project-prefix-map) t)))

  (defun prot/project--keymap-prompt ()
    "Return `project-switch-commands' and their fontified key bindings.
Use a newline character to separate each pair of key and command.

Based on `project--keymap-prompt' and meant to be used as an
:override advice for it."
    (mapconcat
     (pcase-lambda (`(,cmd ,label ,key))
       (let ((key (prot/project--get-key key cmd)))
         (format
          "%s	%s"
          (propertize (key-description key) 'face 'help-key-binding)
          label)))
     project-switch-commands
     "\n"))

  (advice-add #'project--keymap-prompt :override #'prot/project--keymap-prompt)

  (advice-add #'project-switch-project :after #'prot-common-clear-minibuffer-message)

  (prot-emacs-keybind global-map
    "C-x p ." project-dired
    "C-x p C-g" keyboard-quit
    "C-x p <return>" project-dired
    "C-x p <delete>" project-forget-project)

;;;; `diff-mode'
  (setq diff-default-read-only t)
  (setq diff-advance-after-apply-hunk t)
  (setq diff-update-on-the-fly t)
  ;; The following are from Emacs 27.1
  (setq diff-refine nil) ; I do it on demand, with my `agitate' package (more below)
  (setq diff-font-lock-prettify t) ; I think nil is better for patches, but let me try this for a while
  (setq diff-font-lock-syntax 'hunk-also))

;;; Version control framework (vc.el, vc-git.el, and more)
(prot-emacs-package vc
  (:delay 5)
  ;; Those offer various types of functionality, such as blaming,
  ;; viewing logs, showing a dedicated buffer with changes to affected
  ;; files.
  (require 'vc-annotate)
  (require 'vc-dir)
  (require 'vc-git)
  (require 'add-log)
  (require 'log-view)

  ;; I only use Git.  If I ever need another, I will include it here.
  ;; This may have an effect on performance, as Emacs will not try to
  ;; check for a bunch of backends.
  (setq vc-handled-backends '(Git))

  ;; This one is for editing commit messages.
  (require 'log-edit)
  (setq log-edit-confirm 'changed)
  (setq log-edit-keep-buffer nil)
  (setq log-edit-require-final-newline t)
  (setq log-edit-setup-add-author nil)
  ;; I can see the files from the Diff with C-c C-d
  (remove-hook 'log-edit-hook #'log-edit-show-files)

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
  (setq vc-git-log-edit-summary-max-len 70)

  (setq vc-follow-symlinks t)

  ;; NOTE: I override lots of the defaults
  (prot-emacs-keybind global-map
    "C-x v B" vc-annotate ; Blame mnemonic
    "C-x v e" vc-ediff
    "C-x v k" vc-delete-file ; 'k' for kill==>delete is more common
    "C-x v G" vc-log-search  ; git log --grep
    "C-x v t" vc-create-tag
    "C-x v d" vc-diff
    "C-x v ." vc-dir-root ; `vc-dir-root' is from Emacs 28
    "C-x v <return>" vc-dir-root)
  (prot-emacs-keybind vc-dir-mode-map
    "t" vc-create-tag
    "O" vc-log-outgoing
    "o" vc-dir-find-file-other-window
    "d" vc-diff         ; parallel to D: `vc-root-diff'
    "k" vc-dir-delete-file
    "G" vc-revert)
  (prot-emacs-keybind vc-git-stash-shared-map
    "a" vc-git-stash-apply-at-point
    "c" vc-git-stash ; "create" named stash
    "k" vc-git-stash-delete-at-point ; symmetry with `vc-dir-delete-file'
    "p" vc-git-stash-pop-at-point
    "s" vc-git-stash-snapshot)
  (prot-emacs-keybind vc-annotate-mode-map
    "M-q" vc-annotate-toggle-annotation-visibility
    "C-c C-c" vc-annotate-goto-line
    "<return>" vc-annotate-find-revision-at-line)
  (prot-emacs-keybind log-edit-mode-map
    "M-s" nil ; I use M-s for my search commands
    "M-r" nil) ; I use `consult-history'
  (prot-emacs-keybind log-view-mode-map
    "<tab>" log-view-toggle-entry-display
    "<return>" log-view-find-revision
    "s" vc-log-search
    "o" vc-log-outgoing
    "f" vc-log-incoming
    "F" vc-update
    "P" vc-push))

;;; Agitate
;; A package of mine to complement VC and friends.  Read the manual
;; here: <https://protesilaos.com/emacs/agitate>.
(prot-emacs-package agitate
  (:install t)
  (:delay 5)
  (add-hook 'diff-mode-hook #'agitate-diff-enable-outline-minor-mode)
  (advice-add #'vc-git-push :override #'agitate-vc-git-push-prompt-for-remote)

  (setq agitate-log-edit-informative-show-root-log nil
        agitate-log-edit-informative-show-files nil)

  (agitate-log-edit-informative-mode 1)

  (prot-emacs-keybind global-map
    "C-x v =" agitate-diff-buffer-or-file ; replace `vc-diff'
    "C-x v g" agitate-vc-git-grep ; replace `vc-annotate'
    "C-x v f" agitate-vc-git-find-revision
    "C-x v s" agitate-vc-git-show
    "C-x v w" agitate-vc-git-kill-commit-message
    "C-x v p p" agitate-vc-git-format-patch-single
    "C-x v p n" agitate-vc-git-format-patch-n-from-head)
  (prot-emacs-keybind diff-mode-map
    "C-c C-b" agitate-diff-refine-cycle ; replace `diff-refine-hunk'
    "C-c C-n" agitate-diff-narrow-dwim
    "L" vc-print-root-log
    ;; Emacs 29 can use C-x v v in diff buffers, which is great, but now I
    ;; need quick access to it...
    "v" vc-next-action)
  (prot-emacs-keybind log-view-mode-map
    "w" agitate-log-view-kill-revision
    "W" agitate-log-view-kill-revision-expanded)
  (define-key vc-git-log-view-mode-map (kbd "c") #'agitate-vc-git-format-patch-single)
  (prot-emacs-keybind log-edit-mode-map
    "C-c C-i C-n" agitate-log-edit-insert-file-name
    ;; See user options `agitate-log-edit-emoji-collection' and
    ;; `agitate-log-edit-conventional-commits-collection'.
    "C-c C-i C-e" agitate-log-edit-emoji-commit
    "C-c C-i C-c" agitate-log-edit-conventional-commit))

;;; Interactive and powerful git front-end (Magit)

;; There is no need to install the package, as transient.el is built
;; into Emacs.  By requiring it, I prevent the installation of the
;; package, which would be done by Magit.
(prot-emacs-package transient (:delay 5))

(prot-emacs-package magit
  (:install t)
  (:delay 30)
  (setq magit-define-global-key-bindings nil)
  (setq magit-section-visibility-indicator '("⮧"))

  (require 'git-commit)
  (setq git-commit-summary-max-length 50)
  ;; NOTE 2023-01-24: I used to also include `overlong-summary-line'
  ;; in this list, but I realised I do not need it.  My summaries are
  ;; always in check.  When I exceed the limit, it is for a good
  ;; reason.
  (setq git-commit-style-convention-checks '(non-empty-second-line))

  (require 'magit-diff)
  (setq magit-diff-refine-hunk t)

  (require 'magit-repos)
  (setq magit-repository-directories
        '(("~/Git/Projects" . 1)))

  (define-key global-map (kbd "C-c g") #'magit-status))

(provide 'prot-emacs-git)
