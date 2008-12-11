
(defun xmms-jump-to-song (song)
  (interactive "sSearch term: ")
  (shell-command-to-string (concat "/home/jon/utils/xmmsjump " song)))

(defun xmms-seek (time)
  (interactive "NPosition: ")
  (shell-command-to-string (format "xmms2 seek %d" time)))

(defun xmms-next ()
  (interactive)
  (shell-command-to-string "xmms2 next"))

(defun xmms-prev ()
  (interactive)
  (shell-command-to-string "xmms2 prev"))

(global-set-key (kbd "C-c a n") 'xmms-next)
(global-set-key (kbd "C-c a p") 'xmms-prev)
(global-set-key (kbd "C-c a s") 'xmms-seek)
(global-set-key (kbd "C-c a a") 'xmms-jump-to-song)

(provide 'xmms)
