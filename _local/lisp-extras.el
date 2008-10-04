(loop for mode in '(lisp-mode-hook emacs-lisp-mode-hook) do
      (add-hook mode
                (lambda nil (interactive)
                  (local-set-key (kbd "9") (lambda nil (interactive) (insert "(")))
                  (local-set-key (kbd "0") (lambda nil (interactive) (insert ")")))
                  (local-set-key (kbd ")") (lambda nil (interactive) (insert "0")))
                  (local-set-key (kbd "(") (lambda nil (interactive) (insert "9"))))))

(provide 'lisp-extras)