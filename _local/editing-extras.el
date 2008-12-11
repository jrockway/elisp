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

(defun rename-this-file ()
  (interactive)
  (let ((filename (buffer-file-name))
        (modified-p (buffer-modified-p)))
    (when (not filename) (error "Not visiting a file!"))
    (let* ((new-name (read-file-name
                      (format "Rename %s to: " filename)
                      (file-name-directory filename)))
           (new-directory (file-name-directory new-name)))
      (when (get-buffer new-name) (error "Already visting the destination!"))
      (make-directory new-directory t)
      (rename-file filename new-name)
      (rename-buffer new-name)
      (set-visited-file-name new-name)
      (set-buffer-modified-p modified-p))))

(defun kill-and-close ()
  (interactive)
  (kill-buffer)
  (delete-window))

(require 'eproject) ;; this is really bad "refactoring"
(defun iswitch-windows ()
  (interactive)
  (let* ((names-map (loop for win in (window-list (selected-frame) t)
                          collect (cons (buffer-name (window-buffer win)) win)))
         (names (mapcar #'car names-map)))
    (select-window
     (cdr (assoc (eproject--icompleting-read "Window: " names) names-map)))))

(global-set-key (kbd "C-x w") 'iswitch-windows)
(global-set-key (kbd "C-x ,") 'move-line-up)
(global-set-key (kbd "C-x .") 'move-next-line-up)
(global-set-key (kbd "M-D") 'erase-current-word)

(provide 'editing-extras)
