(require 'thingatpt)
(require 'perl-things)
(require 'my-macros)
(require 'cperl-test-increment)
(require 'cperl-dump)
(require 'cperl-use)
(require 'cperl-misc)
(require 'cperl-project)
(require 'cperl-moose)
(provide 'cperl-extras)

(add-hook 'cperl-mode-hook 
          (lambda ()
            (local-set-key "\C-ct" 'increment-test-counter)
            (local-set-key "\C-cu" 'add-use)
            (local-set-key "\C-cmu" 'add-Makefile.PL-requires)
            (local-set-key "\C-cmv" 'visit-Makefile.PL)
            (local-set-key "\C-cd" 'perl-insert-debug-statement)
            (local-set-key "\C-cs" 'insert-self-shift)
            (local-set-key "\C-cT" 'find-tests)
            (local-set-key "\C-cw" 'swap-strict-and-moose)
            (local-set-key "\C-c\C-f" 'ifind-perl-project-file)
            ; some fucktard overwrites this binding.  fuckers.
            (local-set-key "\C-c\C-p" 'ifind-perl-projects)
            (local-set-key "\C-cr" 'cperl-repl)))

(global-set-key "\C-c\C-p" 'ifind-perl-projects)



