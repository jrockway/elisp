
(require 'eproject)
(require 'icomplete-read)

(define-project-type perl (generic)
  (or (look-for "Makefile.PL") (look-for "Build.PL"))
  ("\\.pm$" "\\.t$" "\\.pl$" "\\.PL$"))

(defun cperl--tests ()
  (eproject-assert-type 'perl)
  (concat eproject-root "/t"))

(defun cperl--base-find-tests (find-function)
  (funcall find-function (cperl--tests)))

(defun cperl-find-tests ()
  (interactive)
  (cperl--base-find-tests 'find-file))

(defun cperl-find-tests-other-window ()
  (interactive)
  (cperl--base-find-tests 'find-file-other-window))

(defun perl-project-includes ()
  "Return list of -I flags to pass to perl."
  (eproject-assert-type 'perl)
  (list (concat eproject-root "/lib")))

(defun look-for-Makefile.PL ()
  (eproject-assert-type 'perl)
  (concat eproject-root "/Makefile.PL"))

(defun perl-module-test-file-p (filename)
  (if (string-match "/t/.+[.]t$" filename) t nil))

(defun perl-module-lib-file-p (filename)
  (if (string-match "/lib/.+[.]pm$" filename) t nil))

(provide 'cperl-project)
(require 'cperl-makefile)
