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

(defun first-matching-buffer (predicate)
  "Return PREDICATE applied to the first buffer where PREDICATE applied to the buffer yields a non-nil value."
  (loop for buf in (buffer-list)
        when (with-current-buffer buf (funcall predicate buf))
        return (with-current-buffer buf (funcall predicate buf))))

(defun fix-windows ()
  "Setup my window config."
  (interactive)
  (let ((current-project
         (first-matching-buffer (lambda (x) (ignore-errors (eproject-name)))))
        (current-irc-window
         (first-matching-buffer (lambda (x) (and (eq major-mode 'rcirc-mode)
                                                 x))))
        (current-shell
         (or (first-matching-buffer (lambda (x)
                                      (and (or (eq major-mode 'eshell-mode)
                                               (eq major-mode 'term-mode))
                                           x)))
             (eshell))))

    (delete-other-windows)
    (split-window-horizontally)
    (split-window-horizontally)
    (window-number-select 1)
    (split-window-vertically)
    (labels ((show (x) (set-window-buffer nil (or x (get-buffer-create "*scratch*")))))
      (window-number-select 1)
      (show current-irc-window)
      (window-number-select 2)
      (show current-shell)
      (let ((cur))
        (loop for i in '(3 4)
              do
              (window-number-select i)
              (show (first-matching-buffer
                     (lambda (x) (and (equal (ignore-errors (eproject-name))
                                             current-project)
                                      (not (equal cur (buffer-name x)))
                                      x))))
              (setf cur (buffer-name (current-buffer))))))
    (balance-windows)))

(provide 'windowing-extras)
