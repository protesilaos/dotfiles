;;; Minibuffer and Completions in Tandem or Minibuffer Confines Transcended (mct)
;; Read the manual: <https://protesilaos.com/emacs/mct>.
(use-package mct
  :ensure t
  :hook (after-init . mct-mode)
  :config
  (setq mct-hide-completion-mode-line t)
  (setq mct-completing-read-multiple-indicator t)
  ;; The blocklist and passlist accept either commands/functions or
  ;; completion categories.
  (setq mct-completion-blocklist '(notmuch-mua-new-mail notmuch-mua-prompt-for-sender))
  (setq mct-completion-passlist
        '(;; Some commands
          prot-search-outline
          select-frame-by-name
          Info-goto-node
          Info-index
          Info-menu
          vc-retrieve-tag
          ;; Some completion categories
          consult-buffer
          consult-location
          embark-keybinding
          imenu
          file
          project-file
          buffer
          kill-ring
          consult-location))
  (setq mct-remove-shadowed-file-names t)
  (setq mct-completion-window-size (cons #'mct-frame-height-third 1))
  (setq mct-live-completion 'visible)
  (setq completions-sort #'mct-sort-multi-category)

  ;; Adaptation of `icomplete-fido-backward-updir'.
  (defun prot/mct-backward-updir ()
    "Delete char before point or go up a directory."
    (interactive nil mct-mode)
    (cond
     ((and (eq (char-before) ?/)
           (eq (mct--completion-category) 'file))
      (when (string-equal (minibuffer-contents) "~/")
        (delete-minibuffer-contents)
        (insert (expand-file-name "~/"))
        (goto-char (line-end-position)))
      (save-excursion
        (goto-char (1- (point)))
        (when (search-backward "/" (minibuffer-prompt-end) t)
          (delete-region (1+ (point)) (point-max)))))
     (t (call-interactively 'backward-delete-char))))

  (define-key minibuffer-local-filename-completion-map (kbd "DEL") #'prot/mct-backward-updir))

(provide 'prot-emacs-mct)
