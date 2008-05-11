(require 'sql)

(defun sqlite-connect (sql-database)
  (interactive "fDatabase: ")
  (let ((sql-database (expand-file-name sql-database))
        (product 'sqlite))
    (if (comint-check-proc "*SQL*")
	(switch-to-buffer (get-buffer "*SQL*")))

      ;; Connect to database.
      (message "Login...")
      (funcall (sql-product-feature :sqli-connect product))

      ;; Set SQLi mode.
      (setq sql-interactive-product product)
      (setq sql-buffer (current-buffer))
      (sql-interactive-mode)

      ;; All done.
      (message "Login...done")
      (pop-to-buffer sql-buffer)))

(provide 'sql-extras)

    
