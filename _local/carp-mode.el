(defvar carp-font-lock-keywords nil)
(setq carp-font-lock-keywords
      '(;; stack trace function name
        ("^[\t ]+\\([[:alnum:]:_]+\\)"
         (1 font-lock-function-name-face nil))

        ;; HASH(0x123456)
        ("\\(HASH\\|ARRAY\\|SCALAR\\|GLOB\\|REF\\)\\((0x[[:digit:]abcdef]+)\\)"
         ;(1 font-lock-builtin-face nil)
         (1 font-lock-type-face nil)
         (2 font-lock-type-face nil))
        
        ;; Foo::Bar=HASH(0x654321)
        ;("\\(\\(?:[[:alnum:]_]+\\(?:::\\)?\\)+\\)="
        ; (1 font-lock-builtin-face t))

        ;;blah called at foo.pl line 42
        ("\\(?:called\\)? at \\(.+\\) line \\([[:digit:]]+\\)"
         (1 font-lock-keyword-face t)
         (2 font-lock-variable-name-face t))
        
        ; YOUR PROGRAM BROKE at ...
        ("^\\([[:alnum:]]+.+\\) at"
         (1 font-lock-warning-face t))))

(define-derived-mode carp-mode fundamental-mode "Carp" 
  "Syntax-highlight the output of Perl's `confess' and `cluck' stacktraces"
  (auto-revert-mode t)
  (setq font-lock-defaults '(carp-font-lock-keywords nil t)))
