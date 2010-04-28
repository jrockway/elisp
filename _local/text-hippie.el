;;; text-hippie.el --- hippie expansions for plain text

;; Copyright (C) 2010  Jonathan Rockway

;; Author: Jonathan Rockway <jon@jrock.us>
;; Keywords: convenience

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

(defun try-complete-regular-word (old)
  "A function for `hippie-expand-try-functions-list' that understands English.
Argument OLD is automatically provided by `hippie-expand'; nil
the first time the function is called, t every other time."
  (when (not old)
    (he-init-string (he-lisp-symbol-beg) (point))
    (setf he-expand-list
          (lookup-words
           (concat
            (buffer-substring-no-properties (he-lisp-symbol-beg) (point))
            "*"))))
  (if (null he-expand-list)
      (progn (he-reset-string) nil)
    (he-substitute-string (car he-expand-list))
    (setq he-expand-list (cdr he-expand-list))
    t))

(provide 'text-hippie)
;;; text-hippie.el ends here
