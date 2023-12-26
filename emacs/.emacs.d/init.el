;; For those who use my dotfiles and need an easy way to write their
;; own extras on top of what I already load: search below for the files
;; prot-emacs-pre-custom.el and prot-emacs-post-custom.el
(defgroup prot-emacs nil
  "User options for my dotemacs.
These produce the expected results only when set in a file called
prot-emacs-pre-custom.el.  This file must be in the same
directory as the init.el."
  :group 'file)

(defcustom prot-emacs-load-theme-family 'modus
  "Set of themes to load.
Valid values are the symbols `ef', `modus', and `standard', which
reference the `ef-themes', `modus-themes', and `standard-themes',
respectively.

A nil value does not load any of the above (use Emacs without a
theme).

This user option must be set in the `prot-emacs-pre-custom.el'
file.  If that file exists in the Emacs directory, it is loaded
before all other modules of my setup."
  :group 'prot-emacs
  :type '(choice :tag "Set of themes to load" :value modus
                 (const :tag "The `ef-themes' module" ef)
                 (const :tag "The `modus-themes' module" modus)
                 (const :tag "The `standard-themes' module" standard)
                 (const :tag "Do not load a theme module" nil)))

(defcustom prot-emacs-completion-ui 'vertico
  "Choose minibuffer completion UI between `mct' or `vertico'
This user option must be set in the `prot-emacs-pre-custom.el'
file.  If that file exists in the Emacs directory, it is loaded
before all other modules of my setup."
  :group 'prot-emacs
  :type '(choice :tag "Minibuffer user interface"
                 (const :tag "The `mct' module" mct)
                 (const :tag "The `vertico' module" vertico)))

(defcustom prot-emacs-completion-extras t
  "When non-nil load extras for minibuffer completion.
These include packages such as `consult' and `embark'."
  :group 'prot-emacs
  :type 'boolean)

  :group 'prot-emacs
  :type 'boolean)

(defcustom prot-emacs-load-evil nil
  "When non-nil, load Vim style key bindings as well as `devil-mode'.
This user option must be set in the `prot-emacs-pre-custom.el'
file.  If that file exists in the Emacs directory, it is loaded
before all other modules of my setup."
  :group 'prot-emacs
  :type 'boolean)

(defcustom prot-emacs-load-which-key nil
  "When non-nil, display key binding hints after a short delay.
This user option must be set in the `prot-emacs-pre-custom.el'
file.  If that file exists in the Emacs directory, it is loaded
before all other modules of my setup."
  :group 'prot-emacs
  :type 'boolean)

(defcustom prot-emacs-load-icons nil
  "When non-nil, enable iconography in various contexts.
This installs and uses the `nerd-icons' package and its variants.
NOTE that you still need to invoke `nerd-icons-install-fonts'
manually to first get the icon files.

This user option must be set in the `prot-emacs-pre-custom.el'
file.  If that file exists in the Emacs directory, it is loaded
before all other modules of my setup."
  :group 'prot-emacs
  :type 'boolean)

(defcustom prot-emacs-omit-packages nil
  "List of package names to not load.
This instructs the relevant macros to not `require' the given
package.  In the case of `prot-emacs-elpa-package', the package
will not be installed if it is not already available on the
system.

This user option must be set in the `prot-emacs-pre-custom.el'
file.  If that file exists in the Emacs directory, it is loaded
before all other modules of my setup."
  :group 'prot-emacs
  :type '(repeat symbol))

(setq make-backup-files nil)
(setq backup-inhibited nil) ; Not sure if needed, given `make-backup-files'
(setq create-lockfiles nil)

;; Make native compilation silent and prune its cache.
(when (native-comp-available-p)
  (setq native-comp-async-report-warnings-errors 'silent) ; Emacs 28 with native compilation
  (setq native-compile-prune-cache t)) ; Emacs 29

;; Disable the damn thing by making it disposable.
(setq custom-file (make-temp-file "emacs-custom-"))

(setq default-input-method "greek") ; also check "greek-postfix"
(setq default-transient-input-method "greek")

;; Enable these
(mapc
 (lambda (command)
   (put command 'disabled nil))
 '(list-timers narrow-to-region narrow-to-page upcase-region downcase-region))

;; And disable these
(mapc
 (lambda (command)
   (put command 'disabled t))
 '(eshell project-eshell overwrite-mode iconify-frame diary))

;; Always start with *scratch*
(setq initial-buffer-choice t)

(mapc
 (lambda (string)
   (add-to-list 'load-path (locate-user-emacs-file string)))
 '("prot-lisp" "prot-emacs-modules"))

;;;; Packages

(require 'package)

(setq package-vc-register-as-project nil) ; Emacs 30

(add-hook 'package-menu-mode-hook #'hl-line-mode)

;; Also read: <https://protesilaos.com/codelog/2022-05-13-emacs-elpa-devel/>
(setq package-archives
      '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
        ("gnu-elpa-devel" . "https://elpa.gnu.org/devel/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

;; Highest number gets priority (what is not mentioned has priority 0)
(setq package-archive-priorities
      '(("gnu-elpa" . 3)
        ("melpa" . 2)
        ("nongnu" . 1)))

;; NOTE 2023-08-21: I build Emacs from source, so I always get the
;; latest version of built-in packages.  However, this is a good
;; solution to set to non-nil if I ever switch to a stable release.
(setq package-install-upgrade-built-in nil)

(defvar prot-emacs-my-packages
  '(agitate
    altcaps
    beframe
    cursory
    denote
    dired-preview
    ef-themes
    fontaine
    lin
    logos
    mct
    modus-themes
    notmuch-indicator
    pulsar
    spacious-padding
    standard-themes
    substitute
    sxhkdrc-mode
    theme-buffet
    tmr)
  "List of symbols representing the packages I develop/maintain.")

;; Also read: <https://protesilaos.com/codelog/2022-05-13-emacs-elpa-devel/>
(setq package-pinned-packages
      `(,@(mapcar
           (lambda (package)
             (cons package "gnu-elpa-devel"))
           prot-emacs-my-packages)))

(setq custom-safe-themes t)

(defun prot-emacs-package-install (package &optional method)
  "Install PACKAGE with optional METHOD.

If METHOD is nil or the `builtin' symbol, PACKAGE is not
installed as it is considered part of Emacs.

If METHOD is a string, it must be a URL pointing to the version
controlled repository of PACKAGE.  Installation is done with
`package-vc-install'.

If METHOD is a quoted list, it must have a form accepted by
`package-vc-install' such as:

\\='(denote :url \"https://git.sr.ht/~protesilaos/denote\" :branch \"main\")

If METHOD is any other non-nil value, install PACKAGE using
`package-install'."
  (unless (or (eq method 'builtin) (null method))
    (unless (package-installed-p package)
      (when (or (stringp method) (listp method))
        (package-vc-install method))
      (unless package-archive-contents
        (package-refresh-contents))
      (package-install package))))

(defvar prot-emacs-loaded-packages nil)

(defmacro prot-emacs-package (package &rest body)
  "Require PACKAGE with BODY configurations.

PACKAGE is an unquoted symbol that is passed to `require'.  It
thus conforms with `featurep'.

BODY consists of ordinary Lisp expressions.  There are,
nevertheless, two unquoted plists that are treated specially:

1. (:install METHOD)
2. (:delay NUMBER)

These plists can be anywhere in BODY and are not part of its
final expansion.

The :install property is the argument passed to
`prot-emacs-package-install' and has the meaning of METHOD
described therein.

The :delay property makes the evaluation of PACKAGE with the
expanded BODY happen with `run-with-timer'.

Also see `prot-emacs-configure'."
  (declare (indent 1))
  (unless (memq package prot-emacs-omit-packages)
    (let (install delay)
      (dolist (element body)
        (when (plistp element)
          (pcase (car element)
            (:install (setq install (cdr element)
                            body (delq element body)))
            (:delay (setq delay (cadr element)
                          body (delq element body))))))
      (let ((common `(,(when install
                         `(prot-emacs-package-install ',package ,@install))
                      (require ',package)
                      (add-to-list 'prot-emacs-loaded-packages ',package)
                      ,@body
                      ;; (message "Prot Emacs loaded package: %s" ',package)
                      )))
        (cond
         ((featurep package)
          `(progn ,@body))
         (delay
          `(run-with-timer ,delay nil (lambda () ,@(delq nil common))))
         (t
          `(progn ,@(delq nil common))))))))

;; Samples of `prot-emacs-package' (expand them with `pp-macroexpand-last-sexp').

;; (prot-emacs-package denote
;;   (setq denote-directory "path/to/dir")
;;   (define-key global-map (kbd "C-c n") #'denote)
;;   (:install '(denote . (:url "https://git.sr.ht/~protesilaos/denote" :branch "main")))
;;   (:delay 5)
;;   (setq denote-file-type nil))
;;
;; (prot-emacs-package denote
;;   (setq denote-directory "path/to/dir")
;;   (define-key global-map (kbd "C-c n") #'denote)
;;   (:install "https://git.sr.ht/~protesilaos/denote")
;;   (:delay 5)
;;   (setq denote-file-type nil))
;;
;; (prot-emacs-package denote
;;   (:delay 5)
;;   (setq denote-directory "path/to/dir")
;;   (define-key global-map (kbd "C-c n") #'denote)
;;   (:install "https://git.sr.ht/~protesilaos/denote")
;;   (setq denote-file-type nil))
;;
;; (prot-emacs-package denote
;;   (:install "https://git.sr.ht/~protesilaos/denote")
;;   (:delay 5)
;;   (setq denote-directory "path/to/dir")
;;   (define-key global-map (kbd "C-c n") #'denote)
;;   (setq denote-file-type nil))
;;
;; (prot-emacs-package denote
;;   (:delay 5)
;;   (setq denote-directory "path/to/dir")
;;   (define-key global-map (kbd "C-c n") #'denote)
;;   (setq denote-file-type nil))
;;
;; (prot-emacs-package denote
;;   (setq denote-directory "path/to/dir")
;;   (define-key global-map (kbd "C-c n") #'denote)
;;   (setq denote-file-type nil))

(defmacro prot-emacs-configure (&rest body)
  "Evaluate BODY as a `progn'.
BODY consists of ordinary Lisp expressions.  The sole exception
is an unquoted plist of the form (:delay NUMBER) which evaluates
BODY with NUMBER seconds of `run-with-timer'.

Note that `prot-emacs-configure' does not try to autoload
anything.  Use it only for forms that evaluate regardless.

Also see `prot-emacs-package'."
  (declare (indent 0))
  (let (delay)
    (dolist (element body)
      (when (plistp element)
        (pcase (car element)
          (:delay (setq delay (cadr element)
                        body (delq element body))))))
    (if delay
        `(run-with-timer ,delay nil (lambda () ,@body))
      `(progn ,@body))))

(defmacro prot-emacs-keybind (keymap &rest definitions)
  "Expand key binding DEFINITIONS for the given KEYMAP.
DEFINITIONS is a sequence of string and command pairs."
  (declare (indent 1))
  (unless (zerop (% (length definitions) 2))
    (error "Uneven number of key+command pairs"))
  (let ((keys (seq-filter #'stringp definitions))
        ;; We do accept nil as a definition: it unsets the given key.
        (commands (seq-remove #'stringp definitions)))
    `(when-let (((keymapp ,keymap))
                (map ,keymap))
       ,@(mapcar
          (lambda (pair)
            (let* ((key (car pair))
                   (command (cdr pair)))
              (unless (and (null key) (null command))
                `(define-key map (kbd ,key) ,command))))
          (cl-mapcar #'cons keys commands)))))

;; Sample of `prot-emacs-keybind'

;; (prot-emacs-keybind global-map
;;   "C-z" nil
;;   "C-x b" #'switch-to-buffer
;;   "C-x C-c" nil
;; ;; Notice the -map as I am binding keymap here, not a command:
;;   "C-c b" beframe-prefix-map
;;   "C-x k" #'kill-buffer)

(defmacro prot-emacs-abbrev (table &rest definitions)
  "Expand abbrev DEFINITIONS for the given TABLE.
DEFINITIONS is a sequence of string pairs mapping the
abbreviation to its expansion."
  (declare (indent 1))
  (unless (zerop (% (length definitions) 2))
    (error "Uneven number of key+command pairs"))
  `(when-let (((abbrev-table-p ,table))
              (table ,table))
     ,@(mapcar
        (lambda (pair)
          (when-let ((abbrev (car pair))
                     (expansion (cadr pair)))
            `(define-abbrev table ,abbrev ,expansion)))
        (seq-split definitions 2))))

(defun prot-emacs-return-loaded-packages ()
  "Return a list of all loaded packages.
Here packages include both `prot-emacs-loaded-packages' and
`package-activated-list'.  The latter only covers what is found
in the `package-archives', whereas the former is for anything
that is expanded with the `prot-emacs-package' macro."
  (delete-dups (append prot-emacs-loaded-packages package-activated-list)))

(defvar prot-emacs-package-form-regexp
  "^(\\(prot-emacs-package\\|prot-emacs-keybind\\|prot-emacs-abbrev\\|require\\) +'?\\([0-9a-zA-Z-]+\\)"
  "Regexp to add packages to `lisp-imenu-generic-expression'.")

(eval-after-load 'lisp-mode
  `(add-to-list 'lisp-imenu-generic-expression
                (list "Packages" ,prot-emacs-package-form-regexp 2)))

(defconst prot-emacs-font-lock-keywords
  '(("(\\(prot-emacs-package\\)\\_>[ \t']*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
     (2 font-lock-constant-face nil t))
    ("(\\(prot-emacs-\\(keybind\\|abbrev\\)\\)\\_>[ \t']*\\(\\(\\sw\\|\\s_\\)+\\)?"
     (3 font-lock-variable-name-face nil t))))

(font-lock-add-keywords 'emacs-lisp-mode prot-emacs-font-lock-keywords)

;; For those who use my dotfiles and need an easy way to write their
;; own extras on top of what I already load.  The file must exist at
;; ~/.emacs.d/prot-emacs-pre-custom.el
;;
;; The purpose of this file is for the user to define their
;; preferences BEFORE loading any of the modules.  For example, the
;; user option `prot-emacs-omit-packages' lets the user specify which
;; packages not to load.  Search for all `defcustom' forms in this
;; file for other obvious customisations.
(load (locate-user-emacs-file "prot-emacs-pre-custom.el") :no-error :no-message)

(require 'prot-emacs-theme)
(require 'prot-emacs-essentials)
(require 'prot-emacs-modeline)
(require 'prot-emacs-completion)
(require 'prot-emacs-search)
(require 'prot-emacs-dired)
(require 'prot-emacs-window)
(require 'prot-emacs-git)
(require 'prot-emacs-org)
(require 'prot-emacs-langs)
(require 'prot-emacs-email)
(require 'prot-emacs-web)
(when prot-emacs-load-which-key
  (require 'prot-emacs-which-key))
(when prot-emacs-load-icons
  (require 'prot-emacs-icons))
;; We load it last to override any other keys.
(when prot-emacs-load-evil
  (require 'prot-emacs-evil))

;; For those who use my dotfiles and need an easy way to write their
;; own extras on top of what I already load.  The file must exist at
;; ~/.emacs.d/prot-emacs-post-custom.el
;;
;; The purpose of the "post customisations" is to make tweaks to what
;; I already define, such as to change the default theme.  See above
;; for the `prot-emacs-pre-custom.el' to make changes BEFORE loading
;; any of my other configurations.
(load (locate-user-emacs-file "prot-emacs-post-custom.el") :no-error :no-message)
