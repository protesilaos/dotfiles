;;; Projects (project.el)

(prot-emacs-package project
  (setopt project-switch-commands
          '((project-find-file "Find file")
            (project-find-regexp "Find regexp")
            (project-find-dir "Find directory")
            (project-dired "Root dired")
            (project-vc-dir "VC-Dir")
            (project-shell "Shell")
            (project-eshell "Eshell")))
  (setq project-vc-extra-root-markers '(".project"))

  (let ((map global-map))
    (define-key map (kbd "C-x p .") #'project-dired)
    (define-key map (kbd "C-x p <return>") #'project-dired)
    (define-key map (kbd "C-x p <delete>") #'project-forget-project)))

;;; Diff-mode (and prot-diff.el extensions)
(prot-emacs-package diff-mode
  (setq diff-default-read-only t)
  (setq diff-advance-after-apply-hunk t)
  (setq diff-update-on-the-fly t)
  ;; The following are from Emacs 27.1
  (setq diff-refine nil) ; I do it on demand, with my `agitate' package (more below)
  (setq diff-font-lock-prettify t) ; I think nil is better for patches, but let me try this for a while
  (setq diff-font-lock-syntax 'hunk-also)
  (let ((map diff-mode-map))
    (define-key map (kbd "L") #'vc-print-root-log)
    ;; Emacs 29 can use C-x v v in diff buffers, which is great, but now I
    ;; need quick access to it...
    (define-key map (kbd "v") #'vc-next-action)))

;;; Version control framework (vc.el, vc-git.el, and more)
(prot-emacs-package vc
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
    "C-x v B" #'vc-annotate ; Blame mnemonic
    "C-x v e" #'vc-ediff
    "C-x v k" #'vc-delete-file ; 'k' for kill==>delete is more common
    "C-x v G" #'vc-log-search  ; git log --grep
    "C-x v t" #'vc-create-tag
    "C-x v d" #'vc-diff)
  (prot-emacs-keybind vc-dir-mode-map
    "t" #'vc-create-tag
    "O" #'vc-log-outgoing
    "o" #'vc-dir-find-file-other-window
    "d" #'vc-diff         ; parallel to D: `vc-root-diff'
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
    "M-s" nil ; I use M-s for my search commands
    "M-r" nil) ; I use `consult-history'
  (prot-emacs-keybind log-view-mode-map
    "<tab>" #'log-view-toggle-entry-display
    "<return>" #'log-view-find-revision
    "s" #'vc-log-search
    "o" #'vc-log-outgoing
    "f" #'vc-log-incoming
    "F" #'vc-update
    "P" #'vc-push))

;;; Agitate
;; A package of mine to complement VC and friends.  Read the manual
;; here: <https://protesilaos.com/emacs/agitate>.
(prot-emacs-package agitate
  (:install t)
  (:delay 2)
  (add-hook 'diff-mode-hook #'agitate-diff-enable-outline-minor-mode)
  (advice-add #'vc-git-push :override #'agitate-vc-git-push-prompt-for-remote)

  (setq agitate-log-edit-informative-show-root-log nil
        agitate-log-edit-informative-show-files nil)

  (agitate-log-edit-informative-mode 1)

  (prot-emacs-keybind global-map
    "C-x v =" #'agitate-diff-buffer-or-file ; replace `vc-diff'
    "C-x v g" #'agitate-vc-git-grep ; replace `vc-annotate'
    "C-x v f" #'agitate-vc-git-find-revision
    "C-x v s" #'agitate-vc-git-show
    "C-x v w" #'agitate-vc-git-kill-commit-message
    "C-x v p p" #'agitate-vc-git-format-patch-single
    "C-x v p n" #'agitate-vc-git-format-patch-n-from-head)
  (prot-emacs-keybind diff-mode-map
    "C-c C-b" #'agitate-diff-refine-cycle ; replace `diff-refine-hunk'
    "C-c C-n" #'agitate-diff-narrow-dwim)
  (prot-emacs-keybind log-view-mode-map
    "w" #'agitate-log-view-kill-revision
    "W" #'agitate-log-view-kill-revision-expanded)
  (define-key vc-git-log-view-mode-map (kbd "c") #'agitate-vc-git-format-patch-single)
  (prot-emacs-keybind log-edit-mode-map
    "C-c C-i C-n" #'agitate-log-edit-insert-file-name
    ;; See user options `agitate-log-edit-emoji-collection' and
    ;; `agitate-log-edit-conventional-commits-collection'.
    "C-c C-i C-e" #'agitate-log-edit-emoji-commit
    "C-c C-i C-c" #'agitate-log-edit-conventional-commit))

;;; Interactive and powerful git front-end (Magit)

;; There is no need to install the package, as transient.el is built
;; into Emacs.  By requiring it, I prevent the installation of the
;; package, which would be done by Magit.
(prot-emacs-package transient)

(prot-emacs-package magit
  (:install t)
  (:delay 2)
  (setq magit-define-global-key-bindings nil)
  (define-key global-map (kbd "C-c g") #'magit-status)

  (require 'git-commit)
  (setq git-commit-summary-max-length 50)
  (setq git-commit-known-pseudo-headers
        '("Signed-off-by"
          "Acked-by"
          "Modified-by"
          "Cc"
          "Suggested-by"
          "Reported-by"
          "Tested-by"
          "Reviewed-by"))
  ;; NOTE 2023-01-24: I used to also include `overlong-summary-line'
  ;; in this list, but I realised I do not need it.  My summaries are
  ;; always in check.  When I exceed the limit, it is for a good
  ;; reason.
  (setq git-commit-style-convention-checks '(non-empty-second-line))

  (require 'magit-diff)
  (setq magit-diff-refine-hunk t)

  (require 'magit-repos)
  (setq magit-repository-directories
        '(("~/Git/Projects" . 1))))

;;; Smerge and Ediff
(prot-emacs-package smerge-mode (:delay 10))

(prot-emacs-package ediff
  (:delay 10)
  (setq ediff-keep-variants nil)
  (setq ediff-make-buffers-readonly-at-startup nil)
  (setq ediff-merge-revisions-with-ancestor t)
  (setq ediff-show-clashes-only t)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)

  ;; Tweak those for safer identification and removal
  (setq ediff-combination-pattern
        '("<<<<<<< prot-ediff-combine Variant A" A
          ">>>>>>> prot-ediff-combine Variant B" B
          "####### prot-ediff-combine Ancestor" Ancestor
          "======= prot-ediff-combine End"))

  ;; TODO automate process in a robust way, or at least offer a good key
  ;; binding.
  (defun prot/ediff-flush-combination-pattern ()
    "Remove my custom `ediff-combination-pattern' markers.

This is a quick-and-dirty way to get rid of the markers that are
left behind by `smerge-ediff' when combining the output of two
diffs.  While this could be automated via a hook, I am not yet
sure this is a good approach."
    (interactive)
    (flush-lines ".*prot-ediff.*" (point-min) (point-max) nil)))

(provide 'prot-emacs-git)
