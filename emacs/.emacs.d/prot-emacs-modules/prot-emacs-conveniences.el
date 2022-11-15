;;; Bongo music or media manager (and prot-bongo.el)
(prot-emacs-elpa-package 'bongo
  (setopt bongo-default-directory "~/Music/"
          bongo-prefer-library-buffers nil
          bongo-insert-whole-directory-trees t
          bongo-logo nil
          bongo-display-track-icons nil
          bongo-display-track-lengths nil
          bongo-display-header-icons nil
          bongo-display-playback-mode-indicator t
          bongo-display-inline-playback-progress nil ; t slows down the playlist buffer
          bongo-join-inserted-tracks nil
          bongo-field-separator (propertize " · " 'face 'shadow)
          bongo-mark-played-tracks t
          bongo-vlc-program-name "cvlc")

  (bongo-mode-line-indicator-mode -1)
  (bongo-header-line-mode -1)

  (let ((map global-map))
    (define-key map (kbd "C-c b") #'bongo)
    (define-key map (kbd "<C-XF86AudioPlay>") #'bongo-pause/resume)
    (define-key map (kbd "<C-XF86AudioNext>") #'bongo-next)
    (define-key map (kbd "<C-XF86AudioPrev>") #'bongo-previous)
    (define-key map (kbd "<C-M-XF86AudioPlay>") #'bongo-play-random)
    (define-key map (kbd "<M-XF86AudioPlay>") #'bongo-show)
    (define-key map (kbd "<S-XF86AudioNext>") #'bongo-seek-forward-10)
    (define-key map (kbd "<S-XF86AudioPrev>") #'bongo-seek-backward-10)
    ;; Same as above for the pgtk build of Emacs 29.  Only tested it
    ;; with SwayWM.  GNOME 42 may have its own bindings for the
    ;; multimedia keys that require changes at the level of the desktop
    ;; environment.
    (define-key map (kbd "C-<AudioPlay>") #'bongo-pause/resume)
    (define-key map (kbd "C-<AudioNext>") #'bongo-next)
    (define-key map (kbd "C-<AudioPrev>") #'bongo-previous)
    (define-key map (kbd "C-M-<AudioPlay>") #'bongo-play-random)
    ;; NOTE 2022-09-22: The above four do not work on GNOME 42
    ;; (Wayland).  The key binding is intercepted by the desktop
    ;; environment.  I did not try Xorg because the reason to use GNOME
    ;; is for testing Wayland.  I thus define the function
    ;; `prot-bongo-emacsclient-act' and then set those to key bindings
    ;; at the DE level:
    ;;
    ;; emacsclient -e "(prot-bongo-emacsclient-act 'bongo-pause/resume)"
    ;; emacsclient -e "(prot-bongo-emacsclient-act 'bongo-play-next)"
    ;; emacsclient -e "(prot-bongo-emacsclient-act 'bongo-play-previous)"
    ;; emacsclient -e "(prot-bongo-emacsclient-act 'bongo-play-random)"
    (define-key map (kbd "M-<AudioPlay>") #'bongo-show)
    (define-key map (kbd "S-<AudioNext>") #'bongo-seek-forward-10)
    (define-key map (kbd "S-<AudioPrev>") #'bongo-seek-backward-10))
  (let ((map bongo-playlist-mode-map))
    (define-key map (kbd "n") #'bongo-next-object)
    (define-key map (kbd "p") #'bongo-previous-object)
    (define-key map (kbd "R") #'bongo-rename-line)
    (define-key map (kbd "j") #'bongo-dired-line)       ; Jump to dir of file at point
    (define-key map (kbd "J") #'dired-jump)             ; Jump to library buffer
    (define-key map (kbd "I") #'bongo-insert-special)))

(with-eval-after-load 'bongo
  ;; NOTE 2022-09-29: I plan to rewrite `prot-bongo.el'.  Do not copy
  ;; those as most of them will be rewritten from scratch.
  (prot-emacs-builtin-package 'prot-bongo
    (setopt prot-bongo-enabled-backends '(mpv vlc)
            prot-bongo-playlist-section-delimiter (make-string 30 ?*)
            prot-bongo-playlist-heading-delimiter "§"
            prot-bongo-playlist-directory
            (concat
             (file-name-as-directory bongo-default-directory)
             (file-name-as-directory "playlists")))
    ;; Those set up a few extras: read each function's doc string.  Pass
    ;; an argument to undo their effects.
    (prot-bongo-enabled-backends)
    (prot-bongo-remove-headers)
    (prot-bongo-imenu-setup)
    (add-hook 'dired-mode-hook #'prot-bongo-dired-library-enable)
    (add-hook 'wdired-mode-hook #'prot-bongo-dired-library-disable)
    (add-hook 'prot-bongo-playlist-change-track-hook #'prot-bongo-playlist-recenter)
    (let ((map bongo-playlist-mode-map))
      (define-key map (kbd "C-c C-n") #'prot-bongo-playlist-heading-next)
      (define-key map (kbd "C-c C-p") #'prot-bongo-playlist-heading-previous)
      (define-key map (kbd "M-n") #'prot-bongo-playlist-section-next)
      (define-key map (kbd "M-p") #'prot-bongo-playlist-section-previous)
      (define-key map (kbd "M-h") #'prot-bongo-playlist-mark-section)
      (define-key map (kbd "M-d") #'prot-bongo-playlist-kill-section)
      (define-key map (kbd "g") #'prot-bongo-playlist-reset)
      (define-key map (kbd "D") #'prot-bongo-playlist-terminate)
      (define-key map (kbd "r") #'prot-bongo-playlist-random-toggle)
      (define-key map (kbd "i") #'prot-bongo-playlist-insert-playlist-file))
    (let ((map bongo-dired-library-mode-map))
      (define-key map (kbd "<C-return>") #'prot-bongo-dired-insert)
      (define-key map (kbd "C-c SPC") #'prot-bongo-dired-insert)
      (define-key map (kbd "C-c +") #'prot-bongo-dired-make-playlist-file))))

(prot-emacs-elpa-package 'osm
  (let ((map global-map))
    (define-key map (kbd "C-c O h") #'osm-home)
    (define-key map (kbd "C-c O s") #'osm-search)
    (define-key map (kbd "C-c O t") #'osm-server)
    (define-key map (kbd "C-c O g") #'osm-goto)
    (define-key map (kbd "C-c O j") #'osm-bookmark-jump))

  ;; Load Org link support
  (with-eval-after-load 'org
    (require 'osm-ol)))

(provide 'prot-emacs-conveniences)
