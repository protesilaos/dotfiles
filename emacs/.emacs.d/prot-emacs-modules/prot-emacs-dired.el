;;; Dired file manager and prot-dired.el extras
(prot-emacs-configure
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t))

(setq dired-listing-switches "-AGFhlv --group-directories-first --time-style=long-iso")

(setq dired-dwim-target t)

(setq dired-guess-shell-alist-user ; those are the suggestions for ! and & in Dired
      '(("\\.\\(png\\|jpe?g\\|tiff\\)" "feh *" "feh" "xdg-open")
        ("\\.\\(mp[34]\\|m4a\\|ogg\\|flac\\|webm\\|mkv\\)" "mpv *" "mpv" "xdg-open")
        (".*" "xdg-open")))

(prot-emacs-configure
  (setq dired-auto-revert-buffer #'dired-directory-changed-p) ; also see `dired-do-revert-buffer'
  (setq dired-make-directory-clickable t) ; Emacs 29.1
  (setq dired-free-space nil) ; Emacs 29.1
  (setq dired-mouse-drag-files t) ; Emacs 29.1

  (prot-emacs-hook dired-mode-hook (dired-hide-details-mode hl-line-mode))

  ;; In Emacs 29 there is a binding for `repeat-mode' which lets you
  ;; repeat C-x C-j just by following it up with j.  For me, this is a
  ;; problem as j calls `dired-goto-file', which I often use.
  (define-key dired-jump-map (kbd "j") nil))

(prot-emacs-configure
  (with-eval-after-load 'dired
    (require 'dired-aux)

    (prot-emacs-keybind dired-mode-map
      "C-+" #'dired-create-empty-file
      "M-s f" #'nil
      "C-<return>" #'dired-do-open ; Emacs 30
      "C-x v v" #'dired-vc-next-action) ; Emacs 28

    (setq dired-isearch-filenames 'dwim)
    (setq dired-create-destination-dirs 'ask) ; Emacs 27
    (setq dired-vc-rename-file t)             ; Emacs 27
    (setq dired-do-revert-buffer (lambda (dir) (not (file-remote-p dir)))) ; Emacs 28
    (setq dired-create-destination-dirs-on-trailing-dirsep t) ; Emacs 29

    (require 'dired-x)

    (define-key dired-mode-map (kbd "I") #'dired-info)

    (setq dired-clean-up-buffers-too t)
    (setq dired-clean-confirm-killing-deleted-buffers t)
    (setq dired-x-hands-off-my-keys t)    ; easier to show the keys I use
    (setq dired-bind-man nil)
    (setq dired-bind-info nil)))

(prot-emacs-configure
  (with-eval-after-load 'dired
    (require 'prot-dired)
    (add-hook 'dired-mode #'prot-dired-setup-imenu)
    (prot-emacs-keybind dired-mode-map
      "i" #'prot-dired-insert-subdir ; override `dired-maybe-insert-subdir'
      "/" #'prot-dired-limit-regexp
      "C-c C-l" #'prot-dired-limit-regexp
      "M-n" #'prot-dired-subdirectory-next
      "C-c C-s" #'prot-dired-search-flat-list
      "C-c C-n" #'prot-dired-subdirectory-next
      "C-c C-p" #'prot-dired-subdirectory-previous
      "M-s G" #'prot-dired-grep-marked-files ; M-s g is `prot-search-grep'
      "M-p" #'prot-dired-subdirectory-previous)))

(prot-emacs-configure
  (prot-emacs-install dired-subtree)
  (with-eval-after-load 'dired
    (prot-emacs-keybind dired-mode-map
      "<tab>" #'dired-subtree-toggle
      "TAB" #'dired-subtree-toggle
      "<backtab>" #'dired-subtree-remove
      "S-TAB" #'dired-subtree-remove)
    (setq dired-subtree-use-backgrounds nil)))

(prot-emacs-configure
  (setq wdired-allow-to-change-permissions t)
  (setq wdired-create-parent-directories t))

(prot-emacs-configure
  (with-eval-after-load 'image-dired
    (define-key image-dired-thumbnail-mode-map (kbd "<return>") #'image-dired-thumbnail-display-external)
    (setq image-dired-thumbnail-storage 'standard)
    (setq image-dired-external-viewer "xdg-open")
    (setq image-dired-thumb-size 80)
    (setq image-dired-thumb-margin 2)
    (setq image-dired-thumb-relief 0)
    (setq image-dired-thumbs-per-row 4)))

;;; Automatically preview Dired file at point (dired-preview.el)
;; One of my packages: <https://protesilaos.com/emacs>
(prot-emacs-configure
  (prot-emacs-install dired-preview)
  (with-eval-after-load 'dired
    (require 'dired-preview)
    (add-hook 'dired-mode-hook (lambda ()
                                 (when (string-match-p "Pictures" default-directory)
                                   (dired-preview-mode 1))))
    (define-key dired-mode-map (kbd "V") #'dired-preview-mode)

    (setq dired-preview-trigger-on-start nil)
    (setq dired-preview-max-size (* (expt 2 20) 10))
    (setq dired-preview-delay 0.5)
    (setq dired-preview-ignored-extensions-regexp
          (concat "\\."
                  "\\(gz\\|"
                  "zst\\|"
                  "tar\\|"
                  "xz\\|"
                  "rar\\|"
                  "zip\\|"
                  "iso\\|"
                  "epub"
                  "\\)"))

    (setq dired-preview-display-action-alist #'dired-preview-display-action-alist-below)))

;;; dired-like mode for the trash (trashed.el)
(prot-emacs-configure
  (prot-emacs-install trashed)
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))

;;; Play back media with Dired (mandoura.el)
;; This is yet another package of mine: <https://protesilaos.com/emacs>
(prot-emacs-configure
  (prot-emacs-install mandoura "https://github.com/protesilaos/mandoura")
  (with-eval-after-load 'dired
    (require 'mandoura)
    (prot-emacs-keybind global-map
      "M-<AudioPlay>" #'mandoura-return-track-title-and-time
      "M-<XF86AudioPlay>" #'mandoura-return-track-title-and-time)
    (prot-emacs-keybind dired-mode-map
      "M-<return>" #'mandoura-play-playlist
      "M-RET" #'mandoura-play-playlist)
    (setq mandoura-saved-playlist-directory "~/Music/playlists/")))

(provide 'prot-emacs-dired)
