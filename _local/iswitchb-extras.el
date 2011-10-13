(defun iswitchb-exclude-nonmatching()
  "Make iswitchb work on only the currently matching names."
  (interactive)
  (setq iswitchb-buflist iswitchb-matches)
  (setq iswitchb-rescan t)
  (delete-minibuffer-contents))

(add-hook 'iswitchb-define-mode-map-hook
	  '(lambda () (define-key
			iswitchb-mode-map "\C-o"
			'iswitchb-exclude-nonmatching)))

(defadvice iswitchb-kill-buffer (after rescan-after-kill activate)
  "*Regenerate the list of matching buffer names after a kill.
    Necessary if using `uniquify' with `uniquify-after-kill-buffer-p'
    set to non-nil."
  (setq iswitchb-buflist iswitchb-matches)
  (iswitchb-rescan))

(defun iswitchb-rescan ()
  "*Regenerate the list of matching buffer names."
  (interactive)
  (iswitchb-make-buflist iswitchb-default)
  (setq iswitchb-rescan t))

(ad-activate 'iswitchb-kill-buffer)

(provide 'iswitchb-extras)