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
           ((string-match-p "\\<Role\\>" package-name) #'autoinsert-perl-role)
           (t #'autoinsert-perl-class))
     package-name)))

(defun autoinsert-perl-class (package-name)
  (insert (format "package %s;
# ABSTRACT:
use Moose;
use namespace::autoclean;

__PACKAGE__->meta->make_immutable;

1;
" package-name)))

(defun autoinsert-perl-role (package-name)
  (insert (format "package %s;
# ABSTRACT:
use Moose::Role;
use namespace::autoclean;

1;
" package-name)))

(defun autoinsert-perl-mxdeclare-library (package-name)
  (insert (format "use MooseX::Declare;

%s %s {

}
" (if (string-match-p "Role::" package-name) "role" "class") package-name)))

(defun autoinsert-perl-typelibrary (package-name)
  (insert (format "package %s;
# ABSTRACT:
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

(defun java-get-package-name (file)
  (when (string-match
         "java\\(?:tests\\)?/\\(.+\\)/\\([^/]+\\)[.]java$" file)
    (let ((package (match-string 1 file))
          (class (match-string 2 file)))
      (while (string-match "/" package)
        (setq package (replace-match "." nil nil package)))

      (list :package package
            :test-p (string-match "Test$" class)
            :class-name class))))

(defun my-java-autoinsert ()
  (let* ((java-state (java-get-package-name (buffer-file-name (current-buffer))))
         (class-name (getf java-state :class-name))
         (package (getf java-state :package))
         (test-p (getf java-state :test-p)))
    (if test-p
        (insert (format
"package %s;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.junit.Assert.*;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;

@RunWith(JUnit4.class)
public class %s {

  public %s() {}
}
" package class-name class-name))
      (insert (format
"package %s;

public class %s {

  public %s() {}
}
" package class-name class-name)))))

;; (setq auto-insert-alist nil)
(define-auto-insert '("[.]pm$" . "Perl class") #'my-perl-module-autoinsert)
(define-auto-insert '("[.]pl$" . "Perl script") #'my-perl-script-autoinsert)
(define-auto-insert '("[.]t$" . "Perl test") #'my-perl-test-autoinsert)
(define-auto-insert '("[.]asd$" . "Lisp system") #'my-lisp-asd-autoinsert)
(define-auto-insert '("[.]java$" . "Java class") #'my-java-autoinsert)

(provide 'auto-inserts)
;;; auto-insert.el ends here
