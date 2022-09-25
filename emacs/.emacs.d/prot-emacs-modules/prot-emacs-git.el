;;; Diff-mode (and prot-diff.el extensions)
(prot-emacs-builtin-package 'diff-mode
  (setq diff-default-read-only t)
  (setq diff-advance-after-apply-hunk t)
  (setq diff-update-on-the-fly t)
  ;; The following are from Emacs 27.1
  (setq diff-refine nil)                ; I do it on demand
  (setq diff-font-lock-prettify nil)    ; better for patches
  ;; The following is further controlled by
  ;; `prot-diff-modus-themes-diffs'
  (setq diff-font-lock-syntax 'hunk-also))

(prot-emacs-builtin-package 'prot-diff
  (prot-diff-modus-themes-diffs)
  (add-hook 'modus-themes-after-load-theme-hook #'prot-diff-modus-themes-diffs)

  (prot-diff-extra-keywords 1)

  ;; `prot-diff-buffer-dwim' replaces the default for `vc-diff' (which I
  ;; bind to another key---see VC section).
  (define-key global-map (kbd "C-x v =") #'prot-diff-buffer-dwim)
  (let ((map diff-mode-map))
    (define-key map (kbd "C-c C-b") #'prot-diff-refine-cycle) ; replace `diff-refine-hunk'
    (define-key map (kbd "C-c C-n") #'prot-diff-narrow-dwim)))

;;; Version control framework (vc.el and prot-vc.el)
(prot-emacs-builtin-package 'vc
  ;; Those offer various types of functionality, such as blaming,
  ;; viewing logs, showing a dedicated buffer with changes to affected
  ;; files.
  (require 'vc-annotate)
  (require 'vc-dir)
  (require 'vc-git)
  (require 'add-log)
  (require 'log-view)

  ;; This one is for editing commit messages.
  (require 'log-edit)
  (setq log-edit-confirm 'changed)
  (setq log-edit-keep-buffer nil)
  (setq log-edit-require-final-newline t)
  (setq log-edit-setup-add-author nil)

  ;; Note that `prot-vc-git-setup-mode' will run the following when
  ;; activated:
  ;;
  ;;   (remove-hook 'log-edit-hook #'log-edit-show-files)
  ;;
  ;; If you need the window to pop back up, do it manually with C-c C-f
  ;; which calls `log-edit-show-files'.

  (setq vc-find-revision-no-save t)
  (setq vc-annotate-display-mode 'scale) ; scale to oldest
  ;; I use a different account for git commits
  (setq add-log-mailing-address "info@protesilaos.com")
  (setq add-log-keep-changes-together t)
  (setq vc-git-diff-switches '("--patch-with-stat" "--histogram"))
  (setq vc-git-print-log-follow t)
  (setq vc-git-revision-complete-only-branches nil) ; Emacs 28
  (setq vc-git-root-log-format
        '("%d %h %ad %an: %s"
          ;; The first shy group matches the characters drawn by --graph.
          ;; We use numbered groups because `log-view-message-re' wants the
          ;; revision number to be group 1.
          "^\\(?:[*/\\|]+\\)\\(?:[*/\\| ]+\\)?\
\\(?2: ([^)]+) \\)?\\(?1:[0-9a-z]+\\) \
\\(?4:[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\) \
\\(?3:.*?\\):"
          ((1 'log-view-message)
           (2 'change-log-list nil lax)
           (3 'change-log-name)
           (4 'change-log-date))))

  (setq vc-follow-symlinks t)

  (add-hook 'log-view-mode-hook #'hl-line-mode)

  ;; NOTE: I override lots of the defaults
  (let ((map global-map))

    ;; NOTE 2022-09-24: Emacs 29 repurposes 'C-x v b' to a prefix key
    ;; for branch-related commands.  I am using that now.
    
    ;; (define-key map (kbd "C-x v b") #'vc-retrieve-tag)  ; "branch" switch

    (define-key map (kbd "C-x v t") #'vc-create-tag)
    (define-key map (kbd "C-x v f") #'vc-log-incoming)  ; the actual git fetch
    (define-key map (kbd "C-x v o") #'vc-log-outgoing)
    (define-key map (kbd "C-x v F") #'vc-update)        ; "F" because "P" is push
    (define-key map (kbd "C-x v d") #'vc-diff))
  (let ((map vc-dir-mode-map))
    ;; See note above about 'C-x v b'

    ;; (define-key map (kbd "b") #'vc-retrieve-tag)

    (define-key map (kbd "t") #'vc-create-tag)
    (define-key map (kbd "O") #'vc-log-outgoing)
    (define-key map (kbd "o") #'vc-dir-find-file-other-window)
    (define-key map (kbd "f") #'vc-log-incoming) ; replaces `vc-dir-find-file' (use RET)
    (define-key map (kbd "F") #'vc-update)       ; symmetric with P: `vc-push'
    (define-key map (kbd "d") #'vc-diff)         ; parallel to D: `vc-root-diff'
    (define-key map (kbd "k") #'vc-dir-clean-files)
    (define-key map (kbd "G") #'vc-revert)
    ;; TODO 2022-09-24: The prot-vc-git-branch-map needs to be reviewed.
    
    ;; (let ((prot-vc-git-branch-map (make-sparse-keymap)))
    ;;   (define-key map "B" prot-vc-git-branch-map)
    ;;   (define-key prot-vc-git-branch-map "n" #'vc-create-tag) ; new branch/tag
    ;;   (define-key prot-vc-git-branch-map "s" #'vc-retrieve-tag) ; switch branch/tag
    ;;   (define-key prot-vc-git-branch-map "c" #'prot-vc-git-checkout-remote) ; "checkout" remote
    ;;   (define-key prot-vc-git-branch-map "l" #'vc-print-branch-log))
    (let ((prot-vc-git-stash-map (make-sparse-keymap)))
      (define-key map "S" prot-vc-git-stash-map)
      (define-key prot-vc-git-stash-map "c" 'vc-git-stash) ; "create" named stash
      (define-key prot-vc-git-stash-map "s" 'vc-git-stash-snapshot)))
  (let ((map vc-git-stash-shared-map))
    (define-key map "a" 'vc-git-stash-apply-at-point)
    (define-key map "c" 'vc-git-stash) ; "create" named stash
    (define-key map "D" 'vc-git-stash-delete-at-point)
    (define-key map "p" 'vc-git-stash-pop-at-point)
    (define-key map "s" 'vc-git-stash-snapshot))
  (let ((map vc-annotate-mode-map))
    (define-key map (kbd "M-q") #'vc-annotate-toggle-annotation-visibility)
    (define-key map (kbd "C-c C-c") #'vc-annotate-goto-line)
    (define-key map (kbd "<return>") #'vc-annotate-find-revision-at-line))
  (let ((map log-view-mode-map))
    (define-key map (kbd "<tab>") #'log-view-toggle-entry-display)
    (define-key map (kbd "<return>") #'log-view-find-revision)
    (define-key map (kbd "s") #'vc-log-search)
    (define-key map (kbd "o") #'vc-log-outgoing)
    (define-key map (kbd "f") #'vc-log-incoming)
    (define-key map (kbd "F") #'vc-update)
    (define-key map (kbd "P") #'vc-push)))

(prot-emacs-builtin-package 'prot-vc
  (setq prot-vc-log-limit 100)
  (setq prot-vc-log-bulk-action-limit 50)
  (setq prot-vc-git-log-edit-show-commits t)
  (setq prot-vc-git-log-edit-show-commit-count 10)
  (setq prot-vc-shell-output "*prot-vc-output*")
  (setq prot-vc-patch-output-dirs (list "~/" "~/Desktop/"))
  (add-to-list 'log-edit-headers-alist '("Amend"))

  ;; This refashions log view and log edit buffers
  (prot-vc-git-setup-mode 1)

  ;; NOTE: I override lots of the defaults
  (let ((map global-map))
    (define-key map (kbd "C-x v i") #'prot-vc-git-log-insert-commits)
    (define-key map (kbd "C-x v p") #'prot-vc-project-or-dir)
    (define-key map (kbd "C-x v SPC") #'prot-vc-custom-log)
    (define-key map (kbd "C-x v g") #'prot-vc-git-grep)
    (define-key map (kbd "C-x v G") #'prot-vc-git-log-grep)
    (define-key map (kbd "C-x v a") #'prot-vc-git-patch-apply)
    (define-key map (kbd "C-x v c") #'prot-vc-git-patch-create-dwim)
    (define-key map (kbd "C-x v s") #'prot-vc-git-show)
    (define-key map (kbd "C-x v r") #'prot-vc-git-find-revision)
    (define-key map (kbd "C-x v B") #'prot-vc-git-blame-region-or-file)
    (define-key map (kbd "C-x v R") #'prot-vc-git-reset))
  (let ((map vc-git-log-edit-mode-map))
    (define-key map (kbd "C-C C-n") #'prot-vc-git-log-edit-extract-file-name)
    (define-key map (kbd "C-C C-i") #'prot-vc-git-log-insert-commits)
    ;; Also done by `prot-vc-git-setup-mode', but I am putting it here
    ;; as well for visibility.
    (define-key map (kbd "C-c C-c") #'prot-vc-git-log-edit-done)
    (define-key map (kbd "C-c C-a") #'prot-vc-git-log-edit-toggle-amend)
    (define-key map (kbd "M-p") #'prot-vc-git-log-edit-previous-comment)
    (define-key map (kbd "M-n") #'prot-vc-git-log-edit-next-comment)
    (define-key map (kbd "M-s") #'prot-vc-git-log-edit-complete-comment)
    (define-key map (kbd "M-r") #'prot-vc-git-log-edit-complete-comment))
  (let ((map log-view-mode-map))
    (define-key map (kbd "<C-tab>") #'prot-vc-log-view-toggle-entry-all)
    (define-key map (kbd "a") #'prot-vc-git-patch-apply)
    (define-key map (kbd "c") #'prot-vc-git-patch-create-dwim)
    (define-key map (kbd "R") #'prot-vc-git-log-reset)
    (define-key map (kbd "w") #'prot-vc-log-kill-hash)))

;;; Interactive and powerful git front-end (Magit)

;; There is no need to install the package, as transient.el is built
;; into Emacs.  By requiring it, I prevent the installation of the
;; package, which would be done by Magit.
(prot-emacs-builtin-package 'transient)

(prot-emacs-elpa-package 'magit
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
  (setq git-commit-style-convention-checks
        '(non-empty-second-line
          overlong-summary-line))

  (require 'magit-diff)
  (setq magit-diff-refine-hunk t)

  (require 'magit-repos)
  (setq magit-repository-directories
        '(("~/Git/Projects" . 1))))

;;; Smerge and Ediff
(prot-emacs-builtin-package 'smerge-mode)

(prot-emacs-builtin-package 'ediff
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
