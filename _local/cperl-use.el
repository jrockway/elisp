
(defun add-semicolon (string)
  (if (string-match ";" string)
      string
    (concat string ";")))

(defun add-use (&optional module)
  "Add a new perl use statement after the existing use statements."
  (interactive  (list (read-with-default "Module" (thing-at-point 'perl-module)
                                         "You must specify a module to use!")))
  (let (after statement before space)
    (save-excursion-rewind
     (condition-case nil
         (progn
           (while (re-search-forward "^\\(?:class\\|role\\)[^{]+{" nil t)
             (setq after (match-string 0)))
           (while (re-search-forward "^\\([[:space:]]*\\)use [A-Za-z][[:alnum:]:]+\;" nil t)
             (setq space (match-string 1))
             (setq after (match-string 0))))
       (error (goto-char (point-min))))
     (end-of-line)
     (setq statement (concat space "use " (add-semicolon module)))
     (insert (concat "\n" statement))
     (indent-region (line-beginning-position)
                    (line-end-position))
     (message (format "Added '%s' after '%s'" statement after)))))

(provide 'cperl-use)
