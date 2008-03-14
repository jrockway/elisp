(defadvice erc-iswitchb (around no-ignore-iswitchb)
  (let (iswitchb-buffer-ignore)
    ad-do-it))
(ad-activate 'erc-iswitchb)

(defun reset-erc-track-mode ()
  (interactive)
  (setq erc-modified-channels-alist nil)
  (erc-modified-channels-update))

(defun erc-go-away nil
  (interactive)
  (loop for buf in (buffer-list) 
        when (with-current-buffer buf (eq 'erc-mode major-mode))
        do (bury-buffer buf)))
(erc-go-away)

(provide 'erc-extras)
