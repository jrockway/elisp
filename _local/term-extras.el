;;; term-extras.el --- extras that make term nicer

;; Copyright (C) 2009  Jonathan Rockway

;; Author: Jonathan Rockway <jon@jrock.us>
;; Keywords: terminals

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'cl)
(require 'lisp-mode)
(require 'haskell-mode)
(require 'ielm)

(defvar snap-history nil
  "The window we snapped from.
Shared between all snappers... I think this makes sense.")

(defmacro make-snapper (name test-form)
  `(defun ,(intern (format "snap-to-%s" name)) ()
     (interactive)
     (cond ((and ,test-form snap-history)
            (select-window snap-history))
           (,test-form
            (error "Didn't snap here, don't know what to do."))
           (t
            (setq snap-history (selected-window))
            (let ((win (find-if (lambda (x)
                                   (with-selected-window x ,test-form))
                                 (window-list (selected-frame)))))
              (if win (progn (select-window win) (goto-char (point-max)))
                (error "Nothing to snap to.  Make something, then re-run.")))))))


(make-snapper terminal (or (eq major-mode 'term-mode) (eq major-mode 'eshell-mode)))
(make-snapper slime-repl (eq major-mode 'slime-repl-mode))
(make-snapper ghci (eq major-mode 'inferior-haskell-mode))
(make-snapper ielm (equal (buffer-name) "*ielm*"))

(global-set-key (kbd "C-x x") 'snap-to-terminal)

(define-key lisp-mode-map             (kbd "C-x x") 'snap-to-slime-repl)
(define-key emacs-lisp-mode-map       (kbd "C-x x") 'snap-to-ielm)
(define-key ielm-map                  (kbd "C-x x") 'snap-to-ielm)
(define-key haskell-mode-map          (kbd "C-x x") 'snap-to-ghci)
(define-key inferior-haskell-mode-map (kbd "C-x x") 'snap-to-ghci)

(provide 'term-extras)

;;; So long, and thanks for reminding me of fish.
