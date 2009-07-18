
(defun xmms-jump-to-song (song which)
  "Jump to a song named SONG; if there is more than one, the `prefix-arg' WHICH will disambiguate them."
  (interactive "sSearch term: \np")
  (shell-command-to-string (format "xmmsjump %s %d" song which)))

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
