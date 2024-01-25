;;; Common command alternatives using the Super key
(prot-emacs-configure
  (:delay 2)
  (defvar-keymap prot-emacs-super-keys-map
    :doc "Common command alternatives using the Super key."
    "s-f" #'find-file
    "s-b" #'switch-to-buffer
    "s-j" #'dired-jump
    "s-o" #'other-window
    "s-i" #'indent-rigidly
    "s-1" #'delete-other-windows
    "s-2" #'split-window-below
    "s-3" #'split-window-right
    "s-0" #'delete-window
    "s-m" #'bookmark-jump
    "s-s" #'copy-to-register
    "s-p" #'project-switch-project
    "s-k" #'prot-simple-kill-buffer-current)

  (define-minor-mode prot-emacs-super-keys-mode
    "Provide Super key alternatives to common command."
    :global t
    :init-value t
    :keymap prot-emacs-super-keys-map))

(provide 'prot-emacs-super-keys)
