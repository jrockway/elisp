(require 'cperl-use)

(defun perl-insert-debug-statement (comment)
  "Insert 'use Data::Printer; p `var'' where `var' is the variable near the
   point.  If invoked with an argument, comments out the line where `var'
   is found."
  (interactive "P")
  (let ((var (or (thing-at-point 'perl-variable)
                 (read-from-minibuffer "Expression to dump: "))))
    (if comment (progn
                  (beginning-of-line)
                  (cperl-indent-command)
                  (insert "# ")))
    (save-excursion (unless (re-search-backward
                             "^[[:space:]]*[^#]*[[:space:]]*use[[:space:]]+Data::Printer;" nil t)
                      (add-use "Data::Printer")
                      ))
    (save-excursion
      (back-to-indentation)
      (split-line)
      (insert (format "p %s;" var))
      (comment-indent)
      (insert "DEBUG")
      )))

(provide 'cperl-dump)
