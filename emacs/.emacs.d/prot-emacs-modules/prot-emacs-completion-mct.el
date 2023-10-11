;;; Minibuffer and Completions in Tandem
;; Read the manual: <https://protesilaos.com/emacs/mct>.
(prot-emacs-package mct
  (:install t)
  (:delay 1)
  (setq completions-format 'one-column)
  (setq mct-hide-completion-mode-line nil)
  ;; The blocklist and passlist accept either commands/functions or
  ;; completion categories.
  (setq mct-completion-blocklist '(notmuch-mua-new-mail notmuch-mua-prompt-for-sender))
  (setq mct-completion-passlist '(consult-buffer consult-location embark-keybinding imenu))
  (setq mct-remove-shadowed-file-names t)
  (setq mct-completion-window-size (cons #'mct-frame-height-third 1))
  (setq mct-persist-dynamic-completion nil)
  (setq mct-live-completion 'visible)

  (mct-mode 1)

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

  ;; Specify the sorting function.
  (setq completions-sort #'mct-sort-multi-category)

  (defun prot/mct-next-line-or-completion (n)
    "Select next completion or move to next line N times.
Select the next completion if `completion-in-region-mode' is
active and the Completions window is on display."
    (interactive "p")
    (if (and completion-in-region-mode (mct--get-completion-window))
        (minibuffer-next-completion n)
      (next-line n)))

  (defun prot/mct-previous-line-or-completion (n)
    "Select previous completion or move to previous line N times.
Select the previous completion if `completion-in-region-mode' is
active and the Completions window is on display."
    (interactive "p")
    (if (and completion-in-region-mode (mct--get-completion-window))
        (minibuffer-previous-completion n)
      (previous-line n)))

  (defun prot/mct-return-or-choose-completion (n)
    "Choose current completion or create N newlines.
Choose the current completion if `completion-in-region-mode' is
active and the Completions window is on display."
    (interactive "p")
    (if (and completion-in-region-mode (mct--get-completion-window))
        (minibuffer-choose-completion)
      (newline n :interactive)))

  (prot-emacs-keybind completion-in-region-mode-map
    "C-n" prot/mct-next-line-or-completion
    "C-p" prot/mct-previous-line-or-completion
    "RET" prot/mct-return-or-choose-completion))

(provide 'prot-emacs-completion-mct)
