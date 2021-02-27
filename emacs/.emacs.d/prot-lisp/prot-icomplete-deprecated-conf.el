;; ;; THIS IS DEPRECATED CODE THAT I KEEP HERE FOR FUTURE REFERENCE.
;; ;; Its last state was recovered from my dotfiles' commit 5e39d14 on
;; ;; 2020-11-26.

(use-package icomplete
  :demand
  :after minibuffer                     ; Read that section as well
  :config
  (setq icomplete-delay-completions-threshold 100)
  (setq icomplete-max-delay-chars 2)
  (setq icomplete-compute-delay 0.2)
  (setq icomplete-show-matches-on-no-input t)
  (setq icomplete-hide-common-prefix nil)
  (setq icomplete-prospects-height 1)
  (setq icomplete-separator (propertize " · " 'face 'shadow))
  ;; (setq icomplete-separator " │ ")
  ;; (setq icomplete-separator " ┆ ")
  ;; (setq icomplete-separator " ¦ ")
  ;; (setq icomplete-separator " ┆ ")
  (setq icomplete-with-completion-tables t)
  (setq icomplete-tidy-shadowed-file-names t)

  (fido-mode -1)                        ; Emacs 27.1
  (icomplete-mode 1)

  (defun prot/icomplete-minibuffer-truncate ()
    "Truncate minibuffer lines in `icomplete-mode'.
This should only affect the horizontal layout and is meant to
enforce `icomplete-prospects-height' being set to 1, which is
what I always want.

Hook it to `icomplete-minibuffer-setup-hook'."
    (when (and (minibufferp)
               (bound-and-true-p icomplete-mode))
      (setq truncate-lines t)))

;;; Minibuffer actions
  ;; For a fully fledged package that covers this niche and offers lots
  ;; of added functionality, check Omar Antolín Camarena's "embark"
  ;; library: https://github.com/oantolin/embark
  ;;
  ;; My idea here is to implement the three actions I had always relied
  ;; on, because they are the only ones I ever use.
  (defmacro prot/minibuffer-completion-act (name doc &rest body)
    `(defun ,name ()
       (interactive)
       (let ((candidate (car completion-all-sorted-completions)))
         (when (and (minibufferp)
                    (bound-and-true-p icomplete-mode))
           ,@body))))

  (prot/minibuffer-completion-act
   prot/minibuffer-kill-completion
   "Place minibuffer candidate to the top of the `kill-ring'."
   (kill-new `,candidate)
   (message "Copied %s to kill-ring" (propertize `,candidate 'face 'success)))

  (prot/minibuffer-completion-act
   prot/minibuffer-insert-completion
   "Insert minibuffer candidate in last active window."
   (with-minibuffer-selected-window (insert `,candidate)))

  (prot/minibuffer-completion-act
   prot/minibuffer-insert-completion-exit
   "Like `prot/minibuffer-insert-completion' but exit minibuffer."
   (prot/minibuffer-insert-completion)
   (top-level))

  (define-prefix-command 'prot/minibuffer-completion-map)

  (define-key prot/minibuffer-completion-map
    (kbd "w") 'prot/minibuffer-kill-completion)
  (define-key prot/minibuffer-completion-map
    (kbd "i") 'prot/minibuffer-insert-completion)
  (define-key prot/minibuffer-completion-map
    (kbd "j") 'prot/minibuffer-insert-completion-exit)

  (define-key minibuffer-local-completion-map
    (kbd "M-o") prot/minibuffer-completion-map)

  ;; Note that the the syntax for `use-package' hooks is controlled by
  ;; the `use-package-hook-name-suffix' variable.  The "-hook" suffix is
  ;; not an error of mine.
  :hook (icomplete-minibuffer-setup-hook . prot/icomplete-minibuffer-truncate)
  :bind (:map icomplete-minibuffer-map
              ("<tab>" . icomplete-force-complete)
              ("<return>" . icomplete-force-complete-and-exit) ; exit with completion
              ("C-j" . exit-minibuffer) ; force input unconditionally
              ("C-n" . icomplete-forward-completions)
              ("<right>" . icomplete-forward-completions)
              ("<down>" . icomplete-forward-completions)
              ("C-p" . icomplete-backward-completions)
              ("<left>" . icomplete-backward-completions)
              ("<up>" . icomplete-backward-completions)
              ;; The following command is from Emacs 27.1
              ("<C-backspace>" . icomplete-fido-backward-updir)))

(use-package icomplete-vertical
  :ensure
  :demand
  :after (minibuffer icomplete) ; do not forget to check those as well
  :config
  (setq icomplete-vertical-prospects-height (/ (frame-height) 6))
  (icomplete-vertical-mode -1)

  (defun prot/kill-ring-yank-complete ()
    "Insert the selected `kill-ring' item directly at point.
When region is active, `delete-region'.

Sorting of the `kill-ring' is disabled.  Items appear as they
normally would when calling `yank' followed by `yank-pop'."
    (interactive)
    (let ((kills                    ; do not sort items
           (lambda (string pred action)
             (if (eq action 'metadata)
                 '(metadata (display-sort-function . identity)
                            (cycle-sort-function . identity))
               (complete-with-action
                action kill-ring string pred)))))
      (icomplete-vertical-do
          (:separator 'dotted-line :height (/ (frame-height) 4))
        (when (use-region-p)
          (delete-region (region-beginning) (region-end)))
        (insert
         (completing-read "Yank from kill ring: " kills nil t)))))

  :bind (("s-y" . prot/kill-ring-yank-complete)
         :map icomplete-minibuffer-map
         ("C-v" . icomplete-vertical-toggle)))
