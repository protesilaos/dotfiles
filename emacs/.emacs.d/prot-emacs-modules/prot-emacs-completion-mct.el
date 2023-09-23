;;; Minibuffer and Completions in Tandem
;; Read the manual: <https://protesilaos.com/emacs/mct>.
(prot-emacs-package mct
  (:install t)
  (:delay 5)
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

  (prot-emacs-keybind completion-in-region-mode-map
    "C-n" minibuffer-next-completion
    "C-p" minibuffer-previous-completion
    "RET" minibuffer-choose-completion))

(provide 'prot-emacs-completion-mct)
