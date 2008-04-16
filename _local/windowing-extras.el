(require 'window-number)

(defun in-other-window (window key)
  "Works like `C-x 4', but accepts a window number for executing
the following keybinding in."
  (interactive "cWindow number: \nkCommand: ")
  (setq window (- window #x30))
  (when (or (< window 1) (> window 9))
    (error "Window number out of range!"))
  (message "Window number: %d" window)
  (window-number-select window)
  (call-interactively (lookup-key (current-global-map) key)))

(global-set-key (kbd "C-c 4") 'in-other-window)

(provide 'windowing-extras)