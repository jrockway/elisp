
(defvar last-nopaste-url nil
  "The last URL from the paste server")

(defun nopaste-buffer ()
  (interactive)
  (if (or (not (buffer-modified-p)) (y-or-n-p "Save buffer? "))
      (save-buffer)
    (error "Not pasting modified buffer"))
  (let ((fn (or (buffer-file-name) (error "No buffer filename!"))))
    (setq last-nopaste-url
          (with-temp-buffer
            (shell-command (format "cat %s | /home/jon/utils/pbotutil.pl" fn) 
                           (current-buffer))
            (goto-char (point-min))
            (if (looking-at ".+")
                (match-string 0)
              (error "No URL returned!")))))
  (message last-nopaste-url))

(defun nopaste-yank-url ()
  (interactive)
  (insert last-nopaste-url))

(global-set-key (kbd "C-c n k") 'nopaste-buffer)
(global-set-key (kbd "C-c n y") 'nopaste-yank-url)

(provide 'nopaste)