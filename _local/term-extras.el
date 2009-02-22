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

(defvar snap-to-terminal-history nil
  "The window that was active before we snapped to the terminal.")

(defun snap-to-terminal ()
  (interactive)
  (cond ((and (eq major-mode 'term-mode) snap-to-terminal-history)
         (select-window snap-to-terminal-history))
        ((eq major-mode 'term-mode)
         (error "Didn't snap to this terminal, don't know what to do."))
        (t
         (setq snap-to-terminal-history (selected-window))
         (let ((term (find-if (lambda (x)
                                (with-selected-window x
                                  (eq major-mode 'term-mode)))
                              (window-list (selected-frame)))))
           (if term (select-window term)
             (error "No terminal to snap to.  Make one, then re-run."))))))

(global-set-key (kbd "C-x x") 'snap-to-terminal)

(provide 'term-extras)
;;; term-extras.el ends here
