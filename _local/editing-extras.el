(defun move-line-up ()
  (interactive)
  (let (start end)
    (save-excursion
      (beginning-of-line)
      ; get first non-space character, only look on this line
      (let ((search-end (save-excursion (end-of-line) (point))))
        (re-search-forward "[^[:space:]]" search-end))
      (setq end (1- (point)))
      (previous-line)
      (end-of-line)
      (setq start (point))
      (delete-region start end))
    (goto-char start)))
    
(global-set-key (kbd "C-x ,") 'move-line-up)

(provide 'editing-extras)
