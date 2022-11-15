;;; Outline mode and outline-minor-mode
(prot-emacs-builtin-package 'outline
  (setq outline-minor-mode-highlight 'override) ; emacs28
  (setq outline-minor-mode-cycle t)             ; emacs28
  (setq outline-minor-mode-use-buttons nil) ; emacs29---bless you for the nil option!
  (setq outline-minor-mode-use-margins nil) ; as above
  (define-key global-map (kbd "<f10>") #'outline-minor-mode))

;;; Denote (simple note-taking)
;; Read the manual: <https://protesilaos.com/emacs/denote>.
(prot-emacs-elpa-package 'denote
  ;; Remember to check the doc strings of those variables.
  (setq denote-directory (expand-file-name "~/Documents/notes/"))
  (setq denote-known-keywords '("emacs" "philosophy" "politics" "economics"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-file-type 'text) ; Org is the default, set others here like I do
  (setq denote-excluded-directories-regexp nil)

  ;; We allow multi-word keywords by default.  The author's personal
  ;; preference is for single-word keywords for a more disciplined
  ;; workflow.
  (setq denote-allow-multi-word-keywords nil)

  (setq denote-date-format nil) ; read its doc string

  ;; By default, we fontify backlinks in their bespoke buffer.
  (setq denote-link-fontify-backlinks t)

  ;; Also see `denote-link-backlinks-display-buffer-action' which is a bit
  ;; advanced.

  ;; If you use Markdown or plain text files you want to buttonise
  ;; existing buttons upon visiting the file (Org renders links as
  ;; buttons right away).
  (add-hook 'find-file-hook #'denote-link-buttonize-buffer)

  ;; We use different ways to specify a path for demo purposes.
  (setq denote-dired-directories
        (list denote-directory
              (thread-last denote-directory (expand-file-name "attachments"))
              (expand-file-name "~/Documents/books")))

  ;; Generic (great if you rename files Denote-style in lots of places):
  (add-hook 'dired-mode-hook #'denote-dired-mode)
  ;;
  ;; OR if only want it in `denote-dired-directories':
  ;; (add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)

  ;; Here is a custom, user-level command from one of the examples we
  ;; show in this manual.  We define it here and add it to a key binding
  ;; below.  The manual: <https://protesilaos.com/emacs/denote>.
  (defun prot/denote-journal ()
    "Create an entry tagged 'journal', while prompting for a title."
    (interactive)
    (denote
     (denote--title-prompt)
     '("journal")))

  ;; Denote DOES NOT define any key bindings.  This is for the user to
  ;; decide.  For example:
  (let ((map global-map))
    (define-key map (kbd "C-c n j") #'prot/denote-journal)
    (define-key map (kbd "C-c n n") #'denote)
    (define-key map (kbd "C-c n N") #'denote-type)
    (define-key map (kbd "C-c n d") #'denote-date)
    (define-key map (kbd "C-c n s") #'denote-subdirectory)
    ;; If you intend to use Denote with a variety of file types, it is
    ;; easier to bind the link-related commands to the `global-map', as
    ;; shown here.  Otherwise follow the same pattern for `org-mode-map',
    ;; `markdown-mode-map', and/or `text-mode-map'.
    (define-key map (kbd "C-c n i") #'denote-link) ; "insert" mnemonic
    (define-key map (kbd "C-c n I") #'denote-link-add-links)
    (define-key map (kbd "C-c n b") #'denote-link-backlinks)
    (define-key map (kbd "C-c n f f") #'denote-link-find-file)
    (define-key map (kbd "C-c n f b") #'denote-link-find-backlink)
    ;; Note that `denote-rename-file' can work from any context, not
    ;; just Dired buffers.  That is why we bind it here to the
    ;; `global-map'.
    ;;
    ;; Also see `denote-rename-file-using-front-matter' further below.
    (define-key map (kbd "C-c n r") #'denote-rename-file))

  ;; Key bindings specifically for Dired.
  (let ((map dired-mode-map))
    (define-key map (kbd "C-c C-d C-i") #'denote-link-dired-marked-notes)
    (define-key map (kbd "C-c C-d C-r") #'denote-dired-rename-marked-files))

  ;; Also see `denote-rename-file' further above.
  (define-key text-mode-map (kbd "C-c n R") #'denote-rename-file-using-front-matter)

  (with-eval-after-load 'org-capture
    (setq denote-org-capture-specifiers "%l\n%i\n%?")
    (add-to-list 'org-capture-templates
                 '("n" "New note (with denote.el)" plain
                   (file denote-last-path)
                   #'denote-org-capture
                   :no-save t
                   :immediate-finish nil
                   :kill-buffer t
                   :jump-to-captured t))))

;;; Custom extensions for "focus mode" (logos.el)
;; Read the manual: <https://protesilaos.com/emacs/logos>.
(prot-emacs-elpa-package 'olivetti
  (setq olivetti-body-width 0.7)
  (setq olivetti-minimum-body-width 80)
  (setq olivetti-recall-visual-line-mode-entry-state t))

(prot-emacs-elpa-package 'logos
  (setq logos-outlines-are-pages t)
  (setq logos-outline-regexp-alist
        `((emacs-lisp-mode . ,(format "\\(^;;;+ \\|%s\\)" logos--page-delimiter))
          (org-mode . ,(format "\\(^\\*+ +\\|^-\\{5\\}$\\|%s\\)" logos--page-delimiter))
          (markdown-mode . ,(format "\\(^\\#+ +\\|^[*-]\\{5\\}$\\|^\\* \\* \\*$\\|%s\\)" logos--page-delimiter))
          (conf-toml-mode . "^\\[")))

  ;; These apply when `logos-focus-mode' is enabled.  Their value is
  ;; buffer-local.
  (setq-default logos-hide-mode-line t)
  (setq-default logos-hide-buffer-boundaries t)
  (setq-default logos-hide-fringe t)
  (setq-default logos-variable-pitch t) ; see my `fontaine' configurations
  (setq-default logos-buffer-read-only nil)
  (setq-default logos-scroll-lock nil)
  (setq-default logos-olivetti t)

  ;; I don't need to do `with-eval-after-load' for the `modus-themes' as
  ;; I always load them before other relevant potentially packages.
  (add-hook 'modus-themes-after-load-theme-hook #'logos-update-fringe-in-buffers)

  (let ((map global-map))
    (define-key map [remap narrow-to-region] #'logos-narrow-dwim)
    (define-key map [remap forward-page] #'logos-forward-page-dwim)
    (define-key map [remap backward-page] #'logos-backward-page-dwim)
    ;; I don't think I ever saw a package bind M-] or M-[...
    (define-key map (kbd "M-]") #'logos-forward-page-dwim)
    (define-key map (kbd "M-[") #'logos-backward-page-dwim)
    (define-key map (kbd "<f9>") #'logos-focus-mode))

;;;; Extra tweaks
  ;; place point at the top when changing pages, but not in `prog-mode'
  (defun prot/logos--recenter-top ()
    "Use `recenter' to reposition the view at the top."
    (unless (derived-mode-p 'prog-mode)
      (recenter 1))) ; Use 0 for the absolute top

  (add-hook 'logos-page-motion-hook #'prot/logos--recenter-top))

;;; Emoji input
(prot-emacs-builtin-package 'emoji
  (defun prot/emoji-insert (&optional transient)
    "Thin wrapper for `emoji-insert' and `emoji-search'.
When called with optional TRANSIENT as a prefix argument, use the
transient interface (transient.el), else pick an emoji with
minibuffer completion."
    (interactive "P")
    (let ((cmd (if transient 'emoji-insert 'emoji-search)))
      (call-interactively cmd)))

  ;; The default key bindings for Emoji are behind the C-x 8 e prefix.
  (define-key global-map (kbd "C-.") #'prot/emoji-insert))

(provide 'prot-emacs-write)
