(defadvice erc-iswitchb (around no-ignore-iswitchb)
  (let (iswitchb-buffer-ignore)
    ad-do-it))
(ad-activate 'erc-iswitchb)
;; Clears out annoying erc-track-mode stuff for when we don't care.
;; Useful for when ChanServ restarts :P
(defun reset-erc-track-mode ()
  (interactive)
  (setq erc-modified-channels-alist nil)
  (erc-modified-channels-update))

(provide 'erc-extras)
