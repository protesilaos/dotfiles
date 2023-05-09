;;; Minibuffer and Completions in Tandem
;; Read the manual: <https://protesilaos.com/emacs/mct>.
(prot-emacs-package mct
  (:install t)
  (:delay 5)
  (setq completions-format 'one-column)
  (setq mct-hide-completion-mode-line t)
  ;; The blocklist and passlist accept either commands/functions or
  ;; completion categories.
  (setq mct-completion-blocklist '(notmuch-mua-new-mail notmuch-mua-prompt-for-sender))
  (setq mct-completion-passlist '(consult-buffer consult-location embark-keybinding imenu))
  (setq mct-remove-shadowed-file-names t)
  (setq mct-completion-window-size (cons #'mct-frame-height-third 1))
  (setq mct-persist-dynamic-completion nil)
  (setq mct-live-completion 'visible)

  (mct-minibuffer-mode 1)

  (defvar prot/mct-commands-with-line-numbers
    '( consult-line consult-line-multi consult-mark
       consult-outline consult-grep consult-ripgrep)
    "List of commands that are known to show contextual line numbers.")

  (defun prot/mct-display-line-numbers ()
    "Call `display-line-numbers-mode' when it is not confusing.

Check if the current command is among the list of
`prot/mct-commands-with-line-numbers'.  The elements of that list
are known to display their own contextual lines numbers.

Add this to `completion-list-mode-hook'."
    (unless (memq this-command prot/mct-commands-with-line-numbers)
      (display-line-numbers-mode)))

  (add-hook 'completion-list-mode-hook #'prot/mct-display-line-numbers)

  (defun prot/mct-sort-by-alpha-length (elems)
    "Sort ELEMS first alphabetically, then by length."
    (sort elems (lambda (c1 c2)
                  (or (string-version-lessp c1 c2)
                      (< (length c1) (length c2))))))

  (defun prot/mct-sort-by-history (elems)
    "Sort ELEMS by minibuffer history.
Use `mct-sort-sort-by-alpha-length' if no history is available."
    (if-let ((hist (and (not (eq minibuffer-history-variable t))
                        (symbol-value minibuffer-history-variable))))
        (minibuffer--sort-by-position hist elems)
      (prot/mct-sort-by-alpha-length elems)))

  (defun prot/mct-sort-multi-category (elems)
    "Sort ELEMS per completion category."
    (pcase (mct--completion-category)
      ('kill-ring elems) ; no sorting
      ('file (prot/mct-sort-by-alpha-length elems))
      (_ (prot/mct-sort-by-history elems))))

  ;; Specify the sorting function.
  (setq completions-sort #'prot/mct-sort-multi-category))

(provide 'prot-emacs-completion-mct)
