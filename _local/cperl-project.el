
(require 'eproject)
(require 'icomplete-read)

(define-project-type perl (generic)
  (or (look-for "Makefile.PL") (look-for "Build.PL"))
  :relevant-files ("\\.pm$" "\\.t$" "\\.pl$" "\\.PL$")
  :main-file "Makefile.PL")

(defun cperl-mxdeclare-project-p ()
  "Determine if this project should use MooseX::Declare class definitions."
  (ignore-errors
    (eproject-assert-type 'perl)
    (let ((root (eproject-root)))
      (file-exists-p (concat root ".mxdeclare_project")))))

(defun cperl-convert-moose-to-mxdeclare ()
  "Convert a regular 'use Moose' class to a MX::Declare class."
  (interactive)
  (save-match-data
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "package \\(.+\\);\n*" nil t)
        (let (start (module (match-string 1)))
          (replace-match "use MooseX::Declare;\n\n")
          (cond ((re-search-forward "use Moose::Role;\n*" nil t)
                 (replace-match (format "role %s {\n" module)))
                ((re-search-forward "use Moose;\n*" nil t)
                 (replace-match (format "class %s {\n" module)))
                (t (error "No Moose class found!")))
          (setq start
                (save-excursion
                  (goto-char (car (match-data 0)))
                  (line-beginning-position)))
          (cond ((re-search-forward "\n*1;\n*")
                 (replace-match "\n};\n\n1;\n"))
                (t
                 (goto-char (point-max))
                 (insert "\n\n};\n\n1;\n")))
          (indent-region start (point-max)))))))

(defun cperl--tests ()
  (eproject-assert-type 'perl)
  (concat (eproject-root) "/t"))

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
  (list (concat (eproject-root) "/lib")))

(defun look-for-Makefile.PL ()
  (eproject-assert-type 'perl)
  (concat (eproject-root) "/Makefile.PL"))

(defun perl-module-test-file-p (filename)
  (if (string-match "/t/.+[.]t$" filename) t nil))

(defun perl-module-lib-file-p (filename)
  (if (string-match "/lib/.+[.]pm$" filename) t nil))

(add-hook 'perl-project-file-visit-hook
          (lambda ()
            (ignore-errors
              (stylish-repl-eval-perl
               (format "use lib '%s'" (car (perl-project-includes)))))))

(provide 'cperl-project)
(require 'cperl-makefile)
