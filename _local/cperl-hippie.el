;;; cperl-hippie.el --- hippie-expand functions that understand perl

;; Copyright (C) 2010 Jonathan Rockway

;; Author: Jonathan Rockway <jon@jrock.us>
;; Keywords:

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
(require 'hippie-exp)

(defun cperl-hippie-get-moose-attributes ()
  "Return a list of Moose attributes declared in this class."
  (save-excursion
    (goto-char (point-min))
    (let (result)
      (while (re-search-forward
              "^[[:space:]]*has[^[:space:]]*[[:space:]]+\\(['\"]?\\)\\([^'\"]+\\)" nil t)
        (setq result
              (cons (buffer-substring-no-properties (match-beginning 2) (match-end 2))
                    result)))
      result)))

(defvar cperl-hippie-moose-prefixes '("_build_" "clear_" "has_"))

;; this algorithm is bad.
(defun cperl-hippie-possible-moose-methods (text)
  "Return a list of possible Moose methods starting with TEXT for this class."
  (delete-if-not
   (lambda (x) (string-match (concat "^" text) x))
   (loop for attribute in (cperl-hippie-get-moose-attributes)
         nconc (loop for prefix in cperl-hippie-moose-prefixes
                     collect (concat prefix attribute)))))

(defun try-complete-moose-method (old)
  "A function for `hippie-expand-try-functions-list' that understands Moose.
Argument OLD is automatically provided by `hippie-expand'; nil
the first time the function is called, t every other time."
  (when (not old)
    (he-init-string (he-lisp-symbol-beg) (point))
    (setf he-expand-list
          (cperl-hippie-possible-moose-methods
           (buffer-substring-no-properties (he-lisp-symbol-beg) (point)))))

  (if (null he-expand-list)
      (progn (he-reset-string) nil)
    (he-substitute-string (car he-expand-list))
    (setq he-expand-list (cdr he-expand-list))
    t))

(provide 'cperl-hippie)
;;; cperl-hippie.el ends here
