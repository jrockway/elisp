;;; auto-inserts.el --- my auto-inserts
(require 'cperl-extras)
(require 'cperl-project)
(require 'autoinsert)

(defadvice after-find-file (before ad-mkdir-after-find-file activate)
  "Make the directory containing the visited file."
  (make-directory (file-name-directory (buffer-file-name)) t))

(defun my-perl-module-autoinsert ()
  "Create a project and delegates to other helper functions to fill in this file."
  (ignore-errors (maybe-init-perl-project))
  (eproject-maybe-turn-on)
  (let ((package-name ;;; cut-n-pasted from my template.el stuff; FIXME
         (let ((filename (buffer-file-name)))
           (cond ((or (string-match "/lib/\\(.+\\)[.]pm$" filename)
                      (string-match "/\\(t/.+\\)[.]pm$" filename)
                      (string-match "\\([^/]+\\)[.]pm$" filename))
                  (let ((mod (match-string 1 filename)))
                    (while (string-match "/" mod)
                      (setq mod (replace-match "::" nil nil mod)))
                    mod))
                  (t "UNKNOWN")))))
    (funcall
     (cond ((string-match-p "::Types$" package-name) #'autoinsert-perl-typelibrary)
           ((cperl-mxdeclare-project-p) #'autoinsert-perl-mxdeclare-library)
           (t #'autoinsert-perl-library))
     package-name)))

(defun autoinsert-perl-library (package-name)
  (insert (format "package %s;
use Moose;

use true;
use namespace::autoclean;

" package-name)))

(defun autoinsert-perl-mxdeclare-library (package-name)
  (insert (format "use MooseX::Declare;

%s %s {

}
" (if (string-match-p "Role::" package-name) "role" "class") package-name)))

(defun autoinsert-perl-typelibrary (package-name)
  (insert (format "package %s;
use strict;
use warnings;

use MooseX::Types -declare => [''];
use true;

" package-name)))

(defun my-perl-script-autoinsert ()
  (insert
"#!/usr/bin/env perl

use strict;
use warnings;
use feature ':5.10';
")
  (when (y-or-n-p "Add FindBin boilerplate? ")
    (insert
"
use FindBin qw($Bin);
use lib \"$Bin/../lib\";
")))


(defun my-perl-test-autoinsert ()
  (insert "use strict;\nuse warnings;\nuse Test::More;\n")
  (let ((p (point)))
    (insert "\ndone_testing;")
    (goto-char (1+ p))))

(defun my-lisp-asd-autoinsert ()
  (let ((asd-pkg (format "%s-asd"
                         (file-name-sans-extension
                          (file-name-nondirectory (buffer-file-name)))))
        (pkg (file-name-sans-extension
              (file-name-nondirectory (buffer-file-name)))))
    (insert (format
";;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

(defpackage #:%s
  (:use :cl :asdf))

(in-package #:%s)

(defsystem %s
  :name \"%s\"
  :depends-on ()
  :components ((:file \"package\")
               ))
" asd-pkg asd-pkg pkg pkg))))


;; (setq auto-insert-alist nil)
(define-auto-insert '("[.]pm$" . "Perl class") #'my-perl-module-autoinsert)
(define-auto-insert '("[.]pl$" . "Perl script") #'my-perl-script-autoinsert)
(define-auto-insert '("[.]t$" . "Perl test") #'my-perl-test-autoinsert)
(define-auto-insert '("[.]asd$" . "Lisp system") #'my-lisp-asd-autoinsert)

(provide 'auto-inserts)
;;; auto-insert.el ends here
