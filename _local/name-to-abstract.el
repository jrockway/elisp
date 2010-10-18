(defun name-to-abstract ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((found-package (re-search-forward "package[[:space:]]+[A-Za-z:0-9]+;" nil t)))
      (message "found-package: %d" found-package)
      (when found-package
        (let* ((package-end (point))
               (found-name (when (re-search-forward "^=head1 NAME" nil t)
                             (line-beginning-position))))
          (when found-name
            (message "found-name: %d" found-name)
            (re-search-forward "^[A-Za-z0-9]" nil)
            (when (re-search-forward "-[[:space:]]*\\(.+\\)$" (line-end-position) t)
              (let ((abstract (match-string 1)))
                (message "abstract: %s" abstract)
                (delete-region found-name (progn (re-search-forward "^=") (line-beginning-position)))
                (goto-char package-end)
                (insert (format "\n# ABSTRACT: %s\n" abstract))))))))))


               