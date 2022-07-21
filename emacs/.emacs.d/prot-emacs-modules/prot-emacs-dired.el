;;; Dired file manager and prot-dired.el extras
(prot-emacs-builtin-package 'dired
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches
        "-AGFhlv --group-directories-first --time-style=long-iso")
  (setq dired-dwim-target t)
  (setq dired-auto-revert-buffer #'dired-directory-changed-p) ; also see `dired-do-revert-buffer'
  (setq dired-make-directory-clickable t) ; Emacs 29.1
  (setq dired-free-space nil) ; Emacs 29.1
  (setq dired-mouse-drag-files t) ; Emacs 29.1

  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  (add-hook 'dired-mode-hook #'hl-line-mode))

(prot-emacs-builtin-package 'dired-aux
  (setq dired-isearch-filenames 'dwim)
  ;; The following variables were introduced in Emacs 27.1
  (setq dired-create-destination-dirs 'ask)
  (setq dired-vc-rename-file t)
  ;; And this is for Emacs 28
  (setq dired-do-revert-buffer (lambda (dir) (not (file-remote-p dir))))

  (let ((map dired-mode-map))
    (define-key map (kbd "C-+") #'dired-create-empty-file)
    (define-key map (kbd "M-s f") #'nil)
    (define-key map (kbd "C-x v v") #'dired-vc-next-action))) ; Emacs 28

;; ;; NOTE 2021-05-10: I do not use `find-dired' and related commands
;; ;; because there are other tools that offer a better interface, such
;; ;; as `consult-find', `consult-grep', `project-find-file',
;; ;; `project-find-regexp', `prot-vc-git-grep'.
;; (prot-emacs-builtin-package 'find-dired
;;   (setq find-ls-option
;;         '("-ls" . "-AGFhlv --group-directories-first --time-style=long-iso"))
;;   (setq find-name-arg "-iname"))

(prot-emacs-builtin-package 'dired-x
  (setq dired-clean-up-buffers-too t)
  (setq dired-clean-confirm-killing-deleted-buffers t)
  (setq dired-x-hands-off-my-keys t)    ; easier to show the keys I use
  (setq dired-bind-man nil)
  (setq dired-bind-info nil)
  (define-key dired-mode-map (kbd "I") #'dired-info))

(prot-emacs-builtin-package 'prot-dired
  (setq prot-dired-image-viewers '("feh" "sxiv"))
  (setq prot-dired-media-players '("mpv" "vlc"))
  (setq prot-dired-media-extensions
        "\\.\\(mp[34]\\|ogg\\|flac\\|webm\\|mkv\\)")
  (setq prot-dired-image-extensions
        "\\.\\(png\\|jpe?g\\|tiff\\)")
  (setq dired-guess-shell-alist-user ; those are the defaults for ! and & in Dired
        `((,prot-dired-image-extensions (prot-dired-image-viewer))
          (,prot-dired-media-extensions (prot-dired-media-player))))

  (add-hook 'dired-mode-hook #'prot-dired-setup-imenu)

  (let ((map dired-mode-map))
    (define-key map (kbd "i") #'prot-dired-insert-subdir) ; override `dired-maybe-insert-subdir'
    (define-key map (kbd "/") #'prot-dired-limit-regexp)
    (define-key map (kbd "C-c C-l") #'prot-dired-limit-regexp)
    (define-key map (kbd "M-n") #'prot-dired-subdirectory-next)
    (define-key map (kbd "C-c C-n") #'prot-dired-subdirectory-next)
    (define-key map (kbd "M-p") #'prot-dired-subdirectory-previous)
    (define-key map (kbd "C-c C-p") #'prot-dired-subdirectory-previous)
    (define-key map (kbd "M-s G") #'prot-dired-grep-marked-files))) ; M-s g is `prot-search-grep'

(prot-emacs-elpa-package 'dired-subtree
  (setq dired-subtree-use-backgrounds nil)
  (let ((map dired-mode-map))
    (define-key map (kbd "<tab>") #'dired-subtree-toggle)
    (define-key map (kbd "<backtab>") #'dired-subtree-remove))) ; S-TAB

(prot-emacs-builtin-package 'wdired
  (setq wdired-allow-to-change-permissions t)
  (setq wdired-create-parent-directories t))

(prot-emacs-builtin-package 'image-dired
  (setq image-dired-external-viewer "xdg-open")
  (setq image-dired-thumb-size 80)
  (setq image-dired-thumb-margin 2)
  (setq image-dired-thumb-relief 0)
  (setq image-dired-thumbs-per-row 4)
  (define-key image-dired-thumbnail-mode-map
    (kbd "<return>") #'image-dired-thumbnail-display-external))

;;; dired-like mode for the trash (trashed.el)
(prot-emacs-elpa-package 'trashed
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))

;;; Ibuffer (dired-like buffer list manager)
(prot-emacs-builtin-package 'ibuffer
  (setq ibuffer-expert t)
  (setq ibuffer-display-summary nil)
  (setq ibuffer-use-other-window nil)
  (setq ibuffer-show-empty-filter-groups nil)
  (setq ibuffer-movement-cycle nil)
  (setq ibuffer-default-sorting-mode 'filename/process)
  (setq ibuffer-use-header-line t)
  (setq ibuffer-default-shrink-to-minimum-size nil)
  (setq ibuffer-formats
        '((mark modified read-only locked " "
                (name 40 40 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " " filename-and-process)
          (mark " "
                (name 16 -1)
                " " filename)))
  (setq ibuffer-saved-filter-groups nil)
  (setq ibuffer-old-time 48)
  (add-hook 'ibuffer-mode-hook #'hl-line-mode)
  (define-key global-map (kbd "C-x C-b") #'ibuffer)
  (let ((map ibuffer-mode-map))
    (define-key map (kbd "* f") #'ibuffer-mark-by-file-name-regexp)
    (define-key map (kbd "* g") #'ibuffer-mark-by-content-regexp) ; "g" is for "grep"
    (define-key map (kbd "* n") #'ibuffer-mark-by-name-regexp)
    (define-key map (kbd "s n") #'ibuffer-do-sort-by-alphabetic)  ; "sort name" mnemonic
    (define-key map (kbd "/ g") #'ibuffer-filter-by-content)))

(provide 'prot-emacs-dired)
