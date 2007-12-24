(defun macroexpand-last-sexp ()
  (interactive)
  (prin1 (macroexpand 
          (read 
           (save-excursion
             (re-search-backward ")")
             (thing-at-point 'sexp))))))

(add-hook 'emacs-lisp-mode-hook
          (function 
           (lambda ()
             (local-set-key "\C-x\C-w" 'macroexpand-last-sexp))))

(provide 'elisp-extras)

