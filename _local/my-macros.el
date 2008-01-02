
(defmacro save-excursion-rewind (&rest body)
  "Like save-excursion, but rewinds to `point-min' before executing BODY"
  `(save-excursion
     (goto-char (point-min))
     ,@body))

(defmacro protect-unwind (unwindform &rest bodyforms)
  "Like unwind-protect, but with arguments reversed"
  `(unwind-protect (progn ,@bodyforms) ,unwindform))

; XXX not really a macro
(defun read-with-default (string &optional default error)
  (let ((read (read-string
               (if default 
                   (format "%s (default %s): " string default)
                 (format "%s: " string)))))
    (if (equal read "") (setq read nil))
    (if (and (not read) (not default)) (error error))
    (if (not read) (setq read default))
    read))

(provide 'my-macros)
