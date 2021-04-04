;;; prot-ibuffer.el --- Extensions to ibuffer.el for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2020-2021  Protesilaos Stavrou

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos.com/dotemacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
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
;; This covers my ibuffer.el extensions, for use in my Emacs setup:
;; https://protesilaos.com/dotemacs.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

(require 'ibuffer)

;;;###autoload
(defun prot-ibuffer-buffers-major-mode (&optional arg)
  "Select buffers that match the current buffer's major mode.
With optional prefix ARG (\\[universal-argument]) produce an
`ibuffer' filtered accordingly.  Else use standard completion."
  (interactive "P")
  (let* ((major major-mode)
         (prompt "Buffers for"))
    (if arg
        (ibuffer t (format "*%s %s*" prompt major)
                 (list (cons 'used-mode major)))
      (switch-to-buffer
       (read-buffer
        (format "%s %s:" prompt major) nil t
        (lambda (pair) ; pair is (name-string . buffer-object)
          (with-current-buffer (cdr pair) (derived-mode-p major))))))))

;;;###autoload
(defun prot-ibuffer-buffers-vc-root (&optional arg)
  "Select buffers that belong to the version controlled directory.
With optional prefix ARG (\\[universal-argument]) produce an
`ibuffer' filtered accordingly.  Else use standard completion."
  (interactive "P")
  (let* ((root (or (vc-root-dir)
                   (locate-dominating-file "." ".git")))
         (prompt "Buffers for VC"))
    (if root
        (if arg
            (ibuffer t (format "*%s %s*" prompt root)
                     (list (cons 'filename (expand-file-name root))))
          (switch-to-buffer
           (read-buffer
            (format "%s %s:" prompt root) nil t
            (lambda (pair) ; pair is (name-string . buffer-object)
              (with-current-buffer (cdr pair) (string= (vc-root-dir) root))))))
      (user-error "Not in a version-controlled directory"))))

(provide 'prot-ibuffer)
;;; prot-ibuffer.el ends here
