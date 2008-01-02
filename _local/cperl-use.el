
(defun add-semicolon (string)
  (if (string-match ";" string)
      string
    (concat string ";")))

(defun add-use ()
  "Add a new perl use statement after the existing use statements."
  (interactive)
  (let ((module (read-with-default "Module" (thing-at-point 'perl-module)
                                   "You must specify a module to use!"))
        after statement before)
    (save-excursion-rewind
      (condition-case nil
          (while (re-search-forward "^\\(use [A-Z][[:alnum:]:]+\;\\)" nil t)
            (setq after (match-string 1)))
        (error (goto-char (point-min))))
      (end-of-line)
      (setq statement (concat "use " (add-semicolon module)))
      (insert (concat "\n" statement))
      (message (format "Added '%s' after '%s'" statement after)))))

(provide 'cperl-use)
