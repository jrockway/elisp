
(defvar last-nopaste-url nil
  "The last URL from the paste server")

(defun nopaste-buffer ()
  (interactive)
  (if (or (not (buffer-modified-p)) (y-or-n-p "Save buffer? "))
      (save-buffer)
    (error "Not pasting modified buffer"))
  (let ((fn (or (buffer-file-name) (error "No buffer filename!"))))
    (setq last-nopaste-url
          (let ((old (getenv "NOPASTE_SERVICES")))
            (protect-unwind (setenv "NOPASTE_SERVICES" old)
                (setenv "NOPASTE_SERVICES" "Shadowcat")
                (with-temp-buffer
                  (shell-command (format "nopaste %s" fn) 
                                 (current-buffer))
                  (goto-char (point-min))
                  (if (looking-at ".+")
                      (match-string 0)
                    (error "No URL returned!")))))))
  (message last-nopaste-url))

(defun nopaste-yank-url ()
  (interactive)
  (insert last-nopaste-url))

(global-set-key (kbd "C-c n k") 'nopaste-buffer)
(global-set-key (kbd "C-c n y") 'nopaste-yank-url)

(provide 'nopaste)