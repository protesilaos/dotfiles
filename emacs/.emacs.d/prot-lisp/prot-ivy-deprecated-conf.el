;; ;; THIS IS DEPRECATED CODE THAT I KEEP HERE FOR FUTURE REFERENCE.
;; ;; Its last state was recovered from my dotfiles' commit ebd6094 on
;; ;; 2020-02-04.

(use-package ivy
  :ensure t
  :delight
  :config
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-height-alist '((t lambda (_caller) (/ (window-height) 4))))
  (setq ivy-use-virtual-buffers t)
  (setq ivy-wrap nil)
  (setq ivy-re-builders-alist
        '((counsel-M-x . ivy--regex-fuzzy)
          (ivy-switch-buffer . ivy--regex-fuzzy)
          (ivy-switch-buffer-other-window . ivy--regex-fuzzy)
          (counsel-rg . ivy--regex-or-literal)
          (t . ivy--regex-plus)))
  (setq ivy-display-style 'fancy)
  (setq ivy-use-selectable-prompt t)
  (setq ivy-fixed-height-minibuffer nil)
  (setq ivy-initial-inputs-alist
        '((counsel-M-x . "^")
          (ivy-switch-buffer . "^")
          (ivy-switch-buffer-other-window . "^")
          (counsel-describe-function . "^")
          (counsel-describe-variable . "^")
          (t . "")))

  (ivy-set-occur 'counsel-fzf 'counsel-fzf-occur)
  (ivy-set-occur 'counsel-rg 'counsel-ag-occur)
  (ivy-set-occur 'ivy-switch-buffer 'ivy-switch-buffer-occur)
  (ivy-set-occur 'swiper 'swiper-occur)
  (ivy-set-occur 'swiper-isearch 'swiper-occur)
  (ivy-set-occur 'swiper-multi 'counsel-ag-occur)
  :hook ((after-init . ivy-mode)
         (ivy-occur-mode . hl-line-mode))
  :bind (("<s-up>" . ivy-push-view)
		 ("<s-down>" . ivy-switch-view)
         ("C-S-r" . ivy-resume)
         :map ivy-occur-mode-map
         ("f" . forward-char)
         ("b" . backward-char)
         ("n" . ivy-occur-next-line)
         ("p" . ivy-occur-previous-line)
         ("<C-return>" . ivy-occur-press)))

(use-package prescient
  :ensure t
  :config
  (setq prescient-history-length 200)
  (setq prescient-save-file "~/.emacs.d/prescient-items")
  (setq prescient-filter-method '(literal regexp))
  (prescient-persist-mode 1))

(use-package ivy-prescient
  :ensure t
  :after (prescient ivy)
  :config
  (setq ivy-prescient-sort-commands
        '(:not counsel-grep
               counsel-rg
               counsel-switch-buffer
               ivy-switch-buffer
               swiper
               swiper-multi))
  (setq ivy-prescient-retain-classic-highlighting t)
  (setq ivy-prescient-enable-filtering nil)
  (setq ivy-prescient-enable-sorting t)
  (ivy-prescient-mode 1))

(use-package counsel
  :ensure t
  :after ivy
  :config
  (setq counsel-yank-pop-preselect-last t)
  (setq counsel-yank-pop-separator "\n—————————\n")
  (setq counsel-rg-base-command
        "rg -SHn --no-heading --color never --no-follow --hidden %s")
  (setq counsel-find-file-occur-cmd; TODO Simplify this
        "ls -a | grep -i -E '%s' | tr '\\n' '\\0' | xargs -0 ls -d --group-directories-first")

  (defun prot/counsel-fzf-rg-files (&optional input dir)
    "Run `fzf' in tandem with `ripgrep' to find files in the
present directory.  If invoked from inside a version-controlled
repository, then the corresponding root is used instead."
    (interactive)
    (let* ((process-environment
            (cons (concat "FZF_DEFAULT_COMMAND=rg -Sn --color never --files --no-follow --hidden")
                  process-environment))
           (vc (vc-root-dir)))
      (if dir
          (counsel-fzf input dir)
        (if (eq vc nil)
            (counsel-fzf input default-directory)
          (counsel-fzf input vc)))))

  (defun prot/counsel-fzf-dir (arg)
    "Specify root directory for `counsel-fzf'."
    (prot/counsel-fzf-rg-files ivy-text
                               (read-directory-name
                                (concat (car (split-string counsel-fzf-cmd))
                                        " in directory: "))))

  (defun prot/counsel-rg-dir (arg)
    "Specify root directory for `counsel-rg'."
    (let ((current-prefix-arg '(4)))
      (counsel-rg ivy-text nil "")))

  ;; TODO generalise for all relevant file/buffer counsel-*?
  (defun prot/counsel-fzf-ace-window (arg)
    "Use `ace-window' on `prot/counsel-fzf-rg-files' candidate."
    (ace-window t)
    (let ((default-directory (if (eq (vc-root-dir) nil)
                                 counsel--fzf-dir
                               (vc-root-dir))))
      (if (> (length (aw-window-list)) 1)
          (find-file arg)
        (find-file-other-window arg))
      (balance-windows (current-buffer))))

  ;; Pass functions as appropriate Ivy actions (accessed via M-o)
  (ivy-add-actions
   'counsel-fzf
   '(("r" prot/counsel-fzf-dir "change root directory")
     ("g" prot/counsel-rg-dir "use ripgrep in root directory")
     ("a" prot/counsel-fzf-ace-window "ace-window switch")))

  (ivy-add-actions
   'counsel-rg
   '(("r" prot/counsel-rg-dir "change root directory")
     ("z" prot/counsel-fzf-dir "find file with fzf in root directory")))

  (ivy-add-actions
   'counsel-find-file
   '(("g" prot/counsel-rg-dir "use ripgrep in root directory")
     ("z" prot/counsel-fzf-dir "find file with fzf in root directory")))

  ;; Remove commands that only work with key bindings
  (put 'counsel-find-symbol 'no-counsel-M-x t)
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("s-f" . counsel-find-file)
         ("s-F" . find-file-other-window)
         ("C-x b" . ivy-switch-buffer)
         ("s-b" . ivy-switch-buffer)
         ("C-x B" . counsel-switch-buffer-other-window)
         ("s-B" . counsel-switch-buffer-other-window)
         ("C-x d" . counsel-dired)
         ("s-d" . counsel-dired)
         ("s-D" . dired-other-window)
         ("C-x C-r" . counsel-recentf)
         ("s-m" . counsel-mark-ring)
         ("s-r" . counsel-recentf)
         ("s-y" . counsel-yank-pop)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("M-s r" . counsel-rg)
         ("M-s g" . counsel-git-grep)
         ("M-s l" . counsel-find-library)
         ("M-s z" . prot/counsel-fzf-rg-files)
         :map ivy-minibuffer-map
         ("C-r" . counsel-minibuffer-history)
         ("s-y" . ivy-next-line)        ; Avoid 2× `counsel-yank-pop'
         ("C-SPC" . ivy-restrict-to-matches)))

(use-package projectile
  :ensure t
  ;; :delight '(:eval (concat " " (projectile-project-name)))
  :delight
  :config
  (setq projectile-project-search-path '("~/Git/Projects/"))
  (setq projectile-indexing-method 'alien)
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'ivy))

(use-package counsel-projectile
  :ensure t
  :config
  (add-to-list 'ivy-initial-inputs-alist '(counsel-projectile-switch-project . ""))
  :hook (after-init . counsel-projectile-mode)
  ;; :bind-keymap ("M-s p" . projectile-command-map)
  :bind (("M-s b" . counsel-projectile-switch-to-buffer)
         ("M-s d" . counsel-projectile-find-dir)
         ("M-s p" . (lambda ()
                      (interactive)
                      (counsel-projectile-switch-project 4)))))

(use-package swiper
  :ensure t
  :after ivy
  :config
  (setq swiper-action-recenter t)
  (setq swiper-goto-start-of-match t)
  (setq swiper-include-line-number-in-search t)
  :bind (("C-S-s" . swiper)
         ("M-s s" . swiper-multi)
         ("M-s w" . swiper-thing-at-point)
         :map swiper-map
         ("M-%" . swiper-query-replace)))

(use-package ivy-rich
  :ensure t
  :config
  (setq ivy-rich-path-style 'abbreviate)

  (setcdr (assq t ivy-format-functions-alist)
          #'ivy-format-function-line)
  :hook (after-init . ivy-rich-mode))

(use-package ivy-posframe
  :ensure t
  :delight
  :config
  (setq ivy-posframe-parameters
        '((left-fringe . 2)
          (right-fringe . 2)
          (internal-border-width . 2)
          ;; (font . "Iosevka-10.75:hintstyle=hintfull")
))
  (setq ivy-posframe-height-alist
        '((swiper . 15)
          (swiper-isearch . 15)
          (t . 10)))
  (setq ivy-posframe-display-functions-alist
        '((complete-symbol . ivy-posframe-display-at-point)
          (swiper . nil)
          (swiper-isearch . nil)
          (t . ivy-posframe-display-at-frame-center)))
  :hook (after-init . ivy-posframe-mode))
