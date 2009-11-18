;;; cperl-project-starter.el --- auto-create a perl project when the first file is visited

;; Copyright (C) 2009  Jonathan Rockway

;; Author: Jonathan Rockway <jon@jrock.us>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'magit) ;; <3
(require 'eproject)

(defun already-have-perl-project-p (root)
  (or (string-equal (ignore-errors (eproject-root)) root)
      (file-exists-p (concat root ".git"))))

(defun maybe-init-perl-project ()
  "Create a git + cpan project for the file visited by the current buffer."
  (let* ((filename (buffer-file-name))
         (root (and (string-match "^\\(.+/\\)lib/.+[.]pm$" filename)
                    (match-string 1 filename)))
         (project-name (file-name-nondirectory (directory-file-name root)))
         (default-directory root))

    (when (not (already-have-perl-project-p root))
      (magit-init root)
      (cperl-project-starter-build-gitignore :root root :name project-name)
      (cperl-project-starter-build-Makefile.PL :root root :all-from filename
                                               :name project-name)
      (cperl-project-starter-build-MANIFEST.SKIP :root root :name project-name)
      (cperl-project-starter-build-Changes :root root :name project-name)

      (magit-run-git "commit" "-m" "project boilerplate added")))

      (ignore-errors (eproject-reinitialize-project))
      (eproject-maybe-turn-on))

(defun cperl-project-starter-git-init (root)
  (shell-command (format "cd %s; git init" root)))

(defmacro* cperl-project-starter-make-file ((root file) &body forms)
  (declare (indent 1))
  `(with-temp-buffer
     ,@forms
     (write-file (concat ,root ,file))
     (magit-run-git "add" (concat ,root ,file))))

(defun* cperl-project-starter-build-Makefile.PL (&key root all-from name)
  (cperl-project-starter-make-file (root "Makefile.PL")
    (insert (format "use inc::Module::Install;
use strict;

name '%s';
all_from '%s';

WriteAll;
" name (file-relative-name all-from root)))))

(defun* cperl-project-starter-build-gitignore (&key root name)
  (cperl-project-starter-make-file (root ".gitignore")
    (insert (format "cover_db
META.yml
Makefile
blib
inc
pm_to_blib
MANIFEST
Makefile.old
%s*" name))))

(defun* cperl-project-starter-build-MANIFEST.SKIP (&key root name)
  (cperl-project-starter-make-file (root "MANIFEST.SKIP")
    (insert (format ".git/
blib
pm_to_blib
MANIFEST.bak
MANIFEST.SKIP~
cover_db
Makefile$
Makefile.old$
%s-.*/
%s.*.tar.gz" name name))))

(defun* cperl-project-starter-build-Changes (&key root name)
  (cperl-project-starter-make-file (root "Changes")
    (insert (format "Change history for %s\n" name))))

(provide 'cperl-project-starter)
;;; cperl-project-starter.el ends here
