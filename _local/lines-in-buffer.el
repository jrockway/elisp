; experiment
(require 'cl)

(defun get-iterator-over-words-in (buffer)
  (lexical-let ((buf buffer)(pos 1))
    (lambda ()
      (save-excursion
        (let ((cur (current-buffer)) result)
          (switch-to-buffer buf)
          (goto-char pos)
          (forward-word)
          (let ((pt (point)))
            (if (not (eq pos pt))
                (progn 
                  (setq result (buffer-substring-no-properties pos pt))
                  (setq pos pt))))
          (switch-to-buffer cur) result)))))

;(setq f (get-iterator-over-words-in (get-buffer "test buffer")))
;(funcall f)
