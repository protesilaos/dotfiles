;;; pulsar.el --- Pulse highlight on demand or after select functions -*- lexical-binding: t -*-

;; Copyright (C) 2022  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos.com/emacs/pulsar
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, pulse, highlight

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This is a small package that temporarily highlights the current line
;; either on demand or after a given function is invoked.  The affected
;; functions are defined in the user option `pulsar-pulse-functions'.
;; 
;; The duration of the highlight is determined by `pulsar-delay'.  The
;; steps of the pulse effect are controlled by `pulsar-iterations'.
;; While the applicable face is specified in `pulsar-face'.
;; 
;; To pulse highlight the current line on demand, use
;; `pulsar-pulse-line'.  To highlight the current line without pulsing
;; it, use `pulsar-highlight-line' instead.
;;
;; To help users differentiate between the momentary pulse and transient
;; highlight effects, the user option `pulsar-highlight-face' controls
;; the presentation of the `pulsar-highlight-line' command.  By default,
;; this that variable is the same as `pulsar-face'.
;; 
;; Pulsar depends on the built-in `pulse.el' library.
;;
;; Why the name "pulsar"?  It sounds like "pulse" and is a recognisable
;; word.  Though if you need a backronym, consider "Pulsar
;; Unquestionably Luminates, Strictly Absent the Radiation".

;;; Code:

(require 'pulse)

(defgroup pulsar ()
  "Pulse highlight line on demand or after running select functions.
Extension of `pulse.el'."
  :group 'editing)

;;;; User options

(defcustom pulsar-pulse-functions
  '(recenter-top-bottom
    move-to-window-line-top-bottom
    reposition-window
    bookmark-jump
    other-window
    delete-window
    delete-other-windows
    forward-page
    backward-page
    scroll-up-command
    scroll-down-command
    windmove-right
    windmove-left
    windmove-up
    windmove-down
    windmove-swap-states-right
    windmove-swap-states-left
    windmove-swap-states-up
    windmove-swap-states-down
    tab-new
    tab-close
    tab-next
    org-next-visible-heading
    org-previous-visible-heading
    org-forward-heading-same-level
    org-backward-heading-same-level
    outline-backward-same-level
    outline-forward-same-level
    outline-next-visible-heading
    outline-previous-visible-heading
    outline-up-heading)
  "Functions that highlight the current line after invocation.
This only takes effect when `pulsar-setup' is invoked (e.g. while
setting up `pulsar.el').

Any update to this user option outside of Custom (e.g. with
`setq') requires a re-run of `pulsar-setup'.  Whereas functions
such as `customize-set-variable' do that automatically."
  :type '(repeat function)
  :initialize #'custom-initialize-default
  :set (lambda (symbol value)
         (if (eq value (default-value symbol))
             (set-default symbol value)
           (pulsar-setup 'reverse)
           (set-default symbol value)
           (pulsar-setup)))
  :group 'pulsar)

(defcustom pulsar-face 'pulsar-generic
  "Face of the regular pulse line effect (`pulsar-pulse-line').
The default is `pulsar-generic' which reuses the standard face
from the underlying pulse library.  Users can select one among
`pulsar-red', `pulsar-green', `pulsar-yellow', `pulsar-blue',
`pulsar-magenta', `pulsar-cyan', or any other face that has a
background attribute."
  :type '(radio (face :tag "Generic pulse.el face" pulsar-generic)
                (face :tag "Red style" pulsar-red)
                (face :tag "Green style" pulsar-green)
                (face :tag "Yellow style" pulsar-yellow)
                (face :tag "Blue style" pulsar-blue)
                (face :tag "Magenta style" pulsar-magenta)
                (face :tag "Cyan style" pulsar-cyan)
                (face :tag "Other face (must have a background)"))
  :group 'pulsar)

(defcustom pulsar-highlight-face 'pulsar-face
  "Face used in `pulsar-highlight-line'."
  :type '(choice (variable pulsar-face)
                 (radio (face :tag "Generic pulse.el face" pulsar-generic)
                        (face :tag "Red style" pulsar-red)
                        (face :tag "Green style" pulsar-green)
                        (face :tag "Yellow style" pulsar-yellow)
                        (face :tag "Blue style" pulsar-blue)
                        (face :tag "Magenta style" pulsar-magenta)
                        (face :tag "Cyan style" pulsar-cyan)
                        (face :tag "Other face (must have a background)")))
  :group 'pulsar)

(defcustom pulsar-pulse t
  "When non-nil enable pulsing.
Otherwise the highlight stays on the current line until another
command is invoked."
  :type 'boolean
  :group 'pulsar)

(defcustom pulsar-delay 0.05
  "Duration in seconds of the active pulse highlight.
Only applies when `pulsar-pulse' is non-nil."
  :type 'number
  :group 'pulsar)

(defcustom pulsar-iterations pulse-iterations
  "Number of iterations in a pulse highlight.
Only applies when `pulsar-pulse' is non-nil."
  :type 'number
  :group 'pulsar)

;;;; Faces

(defgroup pulsar-faces ()
  "Faces for `pulsar.el'."
  :group 'pulsar)

(defface pulsar-generic
  '((t :inherit pulse-highlight-start-face :extend t))
  "Default value of `pulsar-face'."
  :group 'pulsar-faces)

(defface pulsar-red
  '((default :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#ffcccc")
    (((class color) (min-colors 88) (background dark))
     :background "#77002a")
    (t :inverse-video t))
  "Alternative red face for `pulsar-face'."
  :group 'pulsar-faces)

(defface pulsar-green
  '((default :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#aceaac")
    (((class color) (min-colors 88) (background dark))
     :background "#00422a")
    (t :inverse-video t))
  "Alternative green face for `pulsar-face'."
  :group 'pulsar-faces)

(defface pulsar-yellow
  '((default :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#fff29a")
    (((class color) (min-colors 88) (background dark))
     :background "#693200")
    (t :inverse-video t))
  "Alternative yellow face for `pulsar-face'."
  :group 'pulsar-faces)

(defface pulsar-blue
  '((default :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#8fcfff")
    (((class color) (min-colors 88) (background dark))
     :background "#242679")
    (t :inverse-video t))
  "Alternative blue face for `pulsar-face'."
  :group 'pulsar-faces)

(defface pulsar-magenta
  '((default :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#ffccff")
    (((class color) (min-colors 88) (background dark))
     :background "#71206a")
    (t :inverse-video t))
  "Alternative magenta face for `pulsar-face'."
  :group 'pulsar-faces)

(defface pulsar-cyan
  '((default :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#8eecf4")
    (((class color) (min-colors 88) (background dark))
     :background "#004065")
    (t :inverse-video t))
  "Alternative cyan face for `pulsar-face'."
  :group 'pulsar-faces)

;;;; Pulse functions

(defun pulsar--indentation-only-line-p ()
  "Return non-nil if current line has only indentation."
  (save-excursion
    (goto-char (point-at-bol))
    (and (not (bobp))
	     (or (beginning-of-line 1) t)
	     (save-match-data
	       (looking-at "^[\s\t]+")))))

(defun pulsar--buffer-end-p ()
  "Return non-nil if point is at the end of the buffer."
  (unless (pulsar--indentation-only-line-p)
    (or (eobp) (eq (point) (point-max)))))

(defun pulsar--start ()
  "Return appropriate line start."
  (if (and (pulsar--buffer-end-p) (eq (char-before) ?\n))
      (line-beginning-position 0)
    (line-beginning-position)))

(defun pulsar--end ()
  "Return appropriate line end."
  (if (and (pulsar--buffer-end-p) (eq (char-before) ?\n))
      (line-beginning-position 1)
    (line-beginning-position 2)))

(defun pulsar--pulse (&optional no-pulse face)
  "Highlight the current line.
With optional NO-PULSE keep the highlight until another command
is invoked.  Otherwise use whatever `pulsar-pulse' entails.

With optional FACE, use it instead of `pulsar-face'."
  (let ((pulse-flag (if no-pulse nil pulsar-pulse))
        (pulse-delay pulsar-delay)
        (pulse-iterations pulsar-iterations)
        (f (if (facep face) face pulsar-face)))
    (pulse-momentary-highlight-region (pulsar--start) (pulsar--end) f)))

;;;###autoload
(defun pulsar-pulse-line ()
  "Temporarily highlight the current line.
When `pulsar-pulse' is non-nil (the default) make the highlight
pulse before fading away.  The pulse effect is controlled by
`pulsar-delay' and `pulsar-iterations'.

Also see `pulsar-highlight-line' for a highlight without the
pulse effect."
  (interactive)
  (pulsar--pulse))

;;;###autoload
(defun pulsar-highlight-line ()
  "Temporarily highlight the current line.
Unlike `pulsar-pulse-line', never pulse the current line.  Keep
the highlight in place until another command is invoked.

Use `pulsar-highlight-face' (it is the same as `pulsar-face' by
default)."
  (interactive)
  (pulsar--pulse :no-pulse pulsar-highlight-face))

;;;; Advice setup

(defun pulsar--post-command-pulse ()
  "Run `pulsar-pulse-line' for `pulsar-pulse-functions'."
  (when (memq this-command pulsar-pulse-functions)
    (pulsar-pulse-line)))

;;;###autoload
(defun pulsar-setup (&optional reverse)
  "Set up pulsar for each function in `pulsar-pulse-functions'.
With optional non-nil REVERSE argument, remove the effect."
  (if reverse
      (remove-hook 'post-command-hook #'pulsar--post-command-pulse)
    (add-hook 'post-command-hook #'pulsar--post-command-pulse)))

;;;; Recentering commands

(defmacro pulsar-recenter (name doc arg)
  "Produce command to pulse and recenter.
The symbol is NAME, DOC for the doc string, and ARG is passed to
`recenter'."
  (declare (indent defun))
  `(defun ,name ()
     ,doc
     (interactive)
     (recenter ,arg)
     (pulsar-pulse-line)))

(pulsar-recenter
  pulsar-recenter-top
  "Reposition point at the top of the window and pulse line."
  0)

(pulsar-recenter
  pulsar-recenter-middle
  "Reposition point at the center of the window and pulse line."
  nil)

;;;; Reveal contents of Org or Outline headings

(declare-function org-at-heading-p "org" (&optional _))
(declare-function org-show-entry "org")
(declare-function outline-on-heading-p "outline" (&optional invisible-ok))
(declare-function outline-show-entry "outline")

(defun pulsar-reveal-entry ()
  "Reveal Org or Outline entry.
Use this in combination with `pulsar-recenter-top' or
`pulsar-recenter-middle'."
  (cond
   ((and (eq major-mode 'org-mode)
         (org-at-heading-p))
    (org-show-entry))
   ((and (or (eq major-mode 'outline-mode)
             (bound-and-true-p outline-minor-mode))
         (outline-on-heading-p))
    (outline-show-entry))))

(provide 'pulsar)
;;; pulsar.el ends here
