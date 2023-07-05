;;; Vertical completion layout
(prot-emacs-package vertico
  (:install t)
  (:delay 5)
  ;; Those are the default values, but check the user option
  ;; `vertico-multiform-categories' for per-category tweaks.  That
  ;; variable is in the file vertico-multiform.el and will work once
  ;; `vertico-multiform-mode' is enabled.
  (setq vertico-scroll-margin 0)
  (setq vertico-count 5)
  (setq vertico-resize nil)
  (setq vertico-cycle t)

  (vertico-mode 1)

  (defvar prot-vertico-minimal
    '(unobtrusive
      (vertico-flat-format . ( :multiple  ""
                               :single    ""
                               :prompt    ""
                               :separator ""
                               :ellipsis  ""
                               :no-match  "")))
    "List of configurations for minimal Vertico multiform.
The minimal view is intended to be more private or less
revealing.  This is important when, for example, a prompt shows
names of people.  Of course, such a view also provides a minimal
style for general usage.

Toggle the vertical view with the `vertico-multiform-vertical'
command or use the commands `prot-vertico-private-next' and
`prot-vertico-private-previous', which toggle the vertical view
automatically.")

  (defvar prot-vertico-maximal
    '((vertico-count . 10)
      (vertico-resize . t))
    "List of configurations for maximal Vertico multiform.")

  ;; Sort directories before files.  From the Consult documentation.
  (defun contrib/sort-directories-first (files)
    (setq files (vertico-sort-history-length-alpha files))
    (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
           (seq-remove (lambda (x) (string-suffix-p "/" x)) files)))

  (setq vertico-multiform-categories
        `(;; Maximal
          (embark-keybinding ,@prot-vertico-maximal)
          (multi-category ,@prot-vertico-maximal)
          (consult-location ,@prot-vertico-maximal)
          (imenu ,@prot-vertico-maximal)
          (unicode-name ,@prot-vertico-maximal)
          ;; Minimal
          (file ,@prot-vertico-minimal
                (vertico-preselect . prompt)
                (vertico-sort-function . contrib/sort-directories-first))
          (t ,@prot-vertico-minimal)))

  (vertico-multiform-mode 1)

  (defun prot-vertico-private-next ()
    "Like `vertico-next' but toggle vertical view if needed.
This is done to accommodate `prot-vertico-minimal'."
    (interactive)
    (if vertico-unobtrusive-mode
        (let ((vertico--index 0))
          (vertico-multiform-vertical)
          (vertico-next 1))
      (vertico-next 1)))

  (defun prot-vertico-private-previous ()
    "Like `vertico-previous' but toggle vertical view if needed.
This is done to accommodate `prot-vertico-minimal'."
    (interactive)
    (if vertico-unobtrusive-mode
        (progn
          (vertico-multiform-vertical)
          (vertico-previous 1))
      (vertico-previous 1)))

  (prot-emacs-keybind vertico-map
    "DEL" #'vertico-directory-delete-char
    "M-DEL" #'vertico-directory-delete-word
    "M-," #'vertico-quick-insert
    "M-." #'vertico-quick-exit)

  (prot-emacs-keybind vertico-multiform-map
    "C-n" #'prot-vertico-private-next
    "<down>" #'prot-vertico-private-next
    "C-p" #'prot-vertico-private-previous
    "<up>" #'prot-vertico-private-previous
    "C-l" #'vertico-multiform-vertical)

  ;; This works with `file-name-shadow-mode' enabled.  When you are in
  ;; a sub-directory and use, say, `find-file' to go to your home '~/'
  ;; or root '/' directory, Vertico will clear the old path to keep
  ;; only your current input.
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))

(provide 'prot-emacs-completion-vertico)
