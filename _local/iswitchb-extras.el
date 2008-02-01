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
(provide 'iswitchb-extras)