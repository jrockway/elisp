(defun move-line-up ()
  "Removes leading spaces from the current line, and then moves
the current line to the end of the previous line."
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

(defun move-next-line-up ()
  "Moves the next line to the end of the current line"
  (interactive)
  (next-line)
  (move-line-up))

(defun erase-current-word ()
  (interactive)
  (save-excursion (kill-word 1) (backward-kill-word 1)))

(global-set-key (kbd "C-x ,") 'move-line-up)
(global-set-key (kbd "C-x .") 'move-next-line-up)
(global-set-key (kbd "M-D") 'erase-current-word)

(provide 'editing-extras)
