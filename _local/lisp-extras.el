
(defalias 'clhs 'hyperspec-lookup "CLHS is easier to type")

(define-project-type lisp (generic)
  (eproject--scan-parents-for file
    (lambda (directory)
      (let ((dname (file-name-nondirectory directory)))
        (file-exists-p (format "%s/%s.asd" directory dname)))))
  :relevant-files ("\\.lisp$" "\\.asd")
  :main-file "package.lisp")

(provide 'lisp-extras)
