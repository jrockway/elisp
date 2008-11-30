
(defalias 'clhs 'hyperspec-lookup "CLHS is easier to type")

(defun lisp-remap-keys ()
  (interactive)
  (local-set-key (kbd "[")
                 (lambda () (interactive) (insert "(")))
  (local-set-key (kbd "]")
                 (lambda () (interactive) (insert ")"))))

(add-hook 'lisp-mode-hook #'lisp-remap-keys)
(add-hook 'emacs-lisp-mode-hook #'lisp-remap-keys)
(add-hook 'ielm-mode-hook #'lisp-remap-keys)

(define-project-type lisp (generic)
  (eproject--scan-parents-for file
    (lambda (directory)
      (let ((dname (file-name-nondirectory directory)))
        (file-exists-p (format "%s/%s.asd" directory dname)))))
  ("\\.lisp$" "\\.asd"))

(provide 'lisp-extras)
