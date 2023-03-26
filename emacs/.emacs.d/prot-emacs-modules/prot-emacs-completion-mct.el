;;; Minibuffer and Completions in Tandem
(setq completion-show-help nil)
(setq completion-auto-help nil)
(setq completion-auto-select nil)
(setq completions-detailed t)
(setq completions-format 'one-column)
(setq completion-show-inline-help nil)
;; (setq completions-max-height 10)
;; (setq completions-header-format (propertize "%s possible completions:\n" 'face 'shadow))
;; (setq completions-highlight-face 'completions-highlight)

(prot-emacs-vc-package 'mct
  (:url "https://git.sr.ht/~protesilaos/mct")
  (setq mct-hide-completion-mode-line t)
  ;; The blocklist and passlist accept either commands/functions or
  ;; completion categories.
  (setq mct-completion-blocklist '(notmuch-mua-new-mail notmuch-mua-prompt-for-sender))
  (setq mct-completion-passlist
        '(consult-location
          embark-keybinding
          imenu))
  (setq mct-remove-shadowed-file-names t)
  (setq mct-completion-window-size (cons #'mct-frame-height-third 1))

  (mct-minibuffer-mode 1)

  ;; From the `mct' manual: <https://protesilaos.com/emacs/mct>.
  (defun my-sort-by-alpha-length (elems)
    "Sort ELEMS first alphabetically, then by length."
    (sort elems (lambda (c1 c2)
                  (or (string-version-lessp c1 c2)
                      (< (length c1) (length c2))))))

  (defun my-sort-by-history (elems)
    "Sort ELEMS by minibuffer history.
Use `mct-sort-sort-by-alpha-length' if no history is available."
    (if-let ((hist (and (not (eq minibuffer-history-variable t))
                        (symbol-value minibuffer-history-variable))))
        (minibuffer--sort-by-position hist elems)
      (my-sort-by-alpha-length elems)))

  (defun my-sort-multi-category (elems)
    "Sort ELEMS per completion category."
    (pcase (mct--completion-category)
      ('nil elems) ; no sorting
      ('kill-ring elems)
      ('file (my-sort-by-alpha-length elems))
      (_ (my-sort-by-history elems))))

  ;; Specify the sorting function.
  (setq completions-sort #'my-sort-multi-category))

(provide 'prot-emacs-completion-mct)
