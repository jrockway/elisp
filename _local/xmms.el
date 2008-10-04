
(defun xmms-jump-to-song (song)
  (interactive "sSearch term: ")
  (shell-command-to-string (concat "/home/jon/utils/xmmsjump " song)))

(defun xmms-seek (time)
  (interactive "NPosition: ")
  (shell-command-to-string (format "xmms2 seek %d" time)))

(provide 'xmms)
