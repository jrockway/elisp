
(require 'w3m)

(defun find-w3m-window ()
  (loop for buffer in (buffer-list)
        do (if (and
                (string-match "^[*]w3m[*]" (buffer-name buffer))
                (get-buffer-window buffer))
               (return (get-buffer-window buffer)))))

(defun my-w3m-browse-url (&rest args)
  (let ((w (find-w3m-window)))
    (when w (select-window w)))
  (apply #'w3m-browse-url args))

(provide 'w3m-extras)
