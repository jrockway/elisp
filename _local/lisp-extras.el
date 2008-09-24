(add-hook 'lisp-mode-hook
          (lambda nil (interactive)
            (local-set-key (kbd "(") (lambda nil (interactive) (insert "[")))
            (local-set-key (kbd ")") (lambda nil (interactive) (insert "]")))
            (local-set-key (kbd "[") (lambda nil (interactive) (insert "(")))
            (local-set-key (kbd "]") (lambda nil (interactive) (insert ")")))))

