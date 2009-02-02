;;; auto-inserts.el --- my auto-inserts
(require 'cperl-extras)
(require 'autoinsert)

(defadvice after-find-file (before ad-mkdir-after-find-file activate)
  "Make the directory containing the visited file."
  (make-directory (file-name-directory (buffer-file-name)) t))

(defun my-perl-module-autoinsert ()
  (eproject-maybe-turn-on)
  (let ((package-name ;;; cut-n-pasted from my template.el stuff; FIXME
         (let ((filename (buffer-file-name)))
           (if (string-match "lib/\\(.+\\)[.]pm$" filename)
               (let ((mod (match-string 1 filename)))
                 (while (string-match "/" mod)
                   (setq mod (replace-match "::" nil nil mod)))
                 mod)
             (if (string-match "\\([^/]+\\)[.]pm$" filename)
                 (match-string 1 filename)
               "UNKNOWN")))))
    (if (not (cperl-mxdeclare-project-p))
        (insert (format "package %s;
use Moose;

1;
" package-name))
      (insert (format "use MooseX::Declare;

class %s {

};

1;" package-name)))))

(defun my-perl-script-autoinsert ()
  (insert
"#!/usr/bin/env perl

use strict;
use warnings;
use feature ':5.10';
"))

(defun my-perl-test-autoinsert ()
  (insert
"use strict;
use warnings;
use Test::More tests => 10;
"))

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


(setq auto-insert-alist nil)
(define-auto-insert '("[.]pm$" . "Perl class") #'my-perl-module-autoinsert)
(define-auto-insert '("[.]pl$" . "Perl script") #'my-perl-script-autoinsert)
(define-auto-insert '("[.]t$" . "Perl test") #'my-perl-test-autoinsert)
(define-auto-insert '("[.]asd$" . "Lisp system") #'my-lisp-asd-autoinsert)

(provide 'auto-inserts)
;;; auto-insert.el ends here
