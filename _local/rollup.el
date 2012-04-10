;;; rollup.el --- roll multiple levels of indentation into a single statement

;; Copyright (C) 2012 Jonathan Rockway

;; Author: Jonathan Rockway <jon@jrock.us>
;; Keywords: lisp

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

;; Sometimes you end up nested pretty deeply for implementation
;; reasons rather than for semantic clarity:
;;
;; (with-current-buffer (buf)
;;   (save-excursion
;;     (save-match-data
;;       (save-restriction
;;         (let ((foo 42))
;;           (code-goes-here))))))
;;
;;  This module lets you write this with one level of indentation:
;;
;; (rollup ((with-current-buffer (buf))
;;          save-excursion
;;          save-match-data
;;          save-restriction
;;          (let ((foo 42))))
;;   code-goes-here)

;;; Code:

(defmacro rollup (wrappers &rest body)
  "Expand WRAPPERS to wrap around the code block BODY."
  (declare (indent defun))
  (labels ((nest        (x y)   (append x (list y)))
           (ensure-list (x)     (if (listp x) x (list x)))
           (to-progn    (forms) (list (cons 'progn forms)))
           (right-fold  (f xs)  (reduce f xs :from-end t)))
    (right-fold #'nest (append (mapcar #'ensure-list wrappers)
                               (to-progn body)))))

;; Example / test:
;;
;; (defmacro fixup-y (&rest body)
;;   `(let ((y -40)) ,@body))

;; (defun test-rollup ()
;;   (rollup ((let ((x 42)))
;;            (let ((y 123)))
;;            fixup-y
;;            (message "result is %s")) ;; 2
;;     (+ x y)))

(provide 'rollup)
;;; rollup.el ends here
