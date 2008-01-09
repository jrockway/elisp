
(defun prepare-var-for-dump (var)
  (let* ((type (substring var 0 1)))
    (if (equal type "$") var (concat "\\" var))))

(defun perl-insert-debug-statement (comment)
  "Insert 'use YAML; die Dump(`var')' where `var' is the variable near the
   point.  If invoked with an argument, comments out the line where `var' 
   is found."
  (interactive "P")
  (let ((var (or (thing-at-point 'perl-variable)
                 (read-from-minibuffer "Expression to dump: "))))
    (if comment (progn 
                  (beginning-of-line)
                  (cperl-indent-command)
                  (insert "# ")))
    (end-of-line)
    (insert (format "\nuse YAML; die Dump(%s);" 
                           (prepare-var-for-dump var)))
    (cperl-indent-command)))

(provide 'cperl-dump)
