;;; Outline mode and outline-minor-mode
(prot-emacs-package outline
  (:delay 5)
  (setq outline-minor-mode-highlight nil) ; emacs28
  (setq outline-minor-mode-cycle t)             ; emacs28
  (setq outline-minor-mode-use-buttons nil) ; emacs29---bless you for the nil option!
  (setq outline-minor-mode-use-margins nil) ; as above
  (define-key global-map (kbd "<f10>") #'outline-minor-mode))

;;; aLtCaPs
;; Read the manual: <https://protesilaos.com/emacs/altcaps>.
(prot-emacs-package altcaps
  (:install t)
  (:delay 10)
  ;; Force letter casing for certain characters (for legibility).
  (setq altcaps-force-character-casing
        '((?i . downcase)
          (?l . upcase)
          (?θ . downcase)))

  ;; The available commands: `altcaps-word', `altcaps-region',
  ;; `altcaps-dwim'.  The tilde is wavy, a bit like this effect.  The
  ;; default command that I am overriding here is `not-modified'
  ;; (never used it).
  (define-key global-map (kbd "M-~") #'altcaps-dwim))

;;; Denote (simple note-taking)
;; Read the manual: <https://protesilaos.com/emacs/denote>.
(prot-emacs-package denote
  (:install t)
  (:delay 2)
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
  (prot-emacs-keybind global-map
    "C-c n j" #'prot/denote-journal
    "C-c n n" #'denote
    "C-c n N" #'denote-type
    "C-c n d" #'denote-date
    "C-c n z" #'denote-signature ; "zettelkasten" mnemonic
    "C-c n s" #'denote-subdirectory
    ;; If you intend to use Denote with a variety of file types, it is
    ;; easier to bind the link-related commands to the `global-map', as
    ;; shown here.  Otherwise follow the same pattern for `org-mode-map',
    ;; `markdown-mode-map', and/or `text-mode-map'.
    "C-c n i" #'denote-link ; "insert" mnemonic
    "C-c n I" #'denote-link-add-links
    "C-c n b" #'denote-link-backlinks
    "C-c n f f" #'denote-link-find-file
    "C-c n f b" #'denote-link-find-backlink
    ;; Note that `denote-rename-file' can work from any context, not
    ;; just Dired buffers.  That is why we bind it here to the
    ;; `global-map'.
    ;;
    ;; Also see `denote-rename-file-using-front-matter' further below.
    "C-c n r" #'denote-rename-file)

  ;; Key bindings specifically for Dired.
  (prot-emacs-keybind dired-mode-map
    "C-c C-d C-i" #'denote-link-dired-marked-notes
    "C-c C-d C-r" #'denote-dired-rename-marked-files)

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

;;; Bibliography
(prot-emacs-package citar
  (:install t)
  (:delay 10)
  ;; See `denote' key bindings above.  The "C-c n" prefix is a
  ;; mnemonic for "note".
  (define-key global-map (kbd "C-c n c") #'citar-insert-citation)
  (setopt citar-bibliography '("~/Documents/protesilaos-stavrou-bibliography.bib"))

  (with-eval-after-load 'embark
    (prot-emacs-elpa-package 'citar-embark
      (setopt citar-at-point-function 'embark-act)
      (citar-embark-mode 1))))

(prot-emacs-package citar-denote
  (:install t)
  (:delay 10)
  (citar-denote-mode 1))

;;; Custom extensions for "focus mode" (logos.el)
;; Read the manual: <https://protesilaos.com/emacs/logos>.
(prot-emacs-package olivetti
  (:install t)
  (:delay 5)
  (setq olivetti-body-width 0.7)
  (setq olivetti-minimum-body-width 80)
  (setq olivetti-recall-visual-line-mode-entry-state t))

(prot-emacs-package logos
  (:install t)
  (:delay 5)
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

  (prot-emacs-keybind global-map
    "C-x n n" #'logos-narrow-dwim
    "C-x ]" #'logos-forward-page-dwim
    "C-x [" #'logos-backward-page-dwim
    ;; I don't think I ever saw a package bind M-] or M-[...
    "M-]" #'logos-forward-page-dwim
    "M-[" #'logos-backward-page-dwim
    "<f9>" #'logos-focus-mode)

;;;; Extra tweaks
  ;; place point at the top when changing pages, but not in `prog-mode'
  (defun prot/logos--recenter-top ()
    "Use `recenter' to reposition the view at the top."
    (unless (derived-mode-p 'prog-mode)
      (recenter 1))) ; Use 0 for the absolute top

  (add-hook 'logos-page-motion-hook #'prot/logos--recenter-top))

;;; Dictionary
(prot-emacs-package dictionary
  (:delay 10)
  (setq dictionary-server "dict.org"
        dictionary-default-popup-strategy "lev" ; read doc string
        dictionary-create-buttons nil
        dictionary-use-single-buffer t)
  (define-key global-map (kbd "C-c d") #'dictionary-search))

(provide 'prot-emacs-write)
