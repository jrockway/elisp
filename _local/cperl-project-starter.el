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

(defvar do-not-want-perl-project nil
  "Remember `n' answers to 'do you want a project?'.")

(defun already-have-perl-project-p (root)
  (or (string-equal (ignore-errors (eproject-root)) root)
      (or (member root do-not-want-perl-project))
      (and (file-exists-p (concat root ".git"))
           (or (file-exists-p (concat root "dist.ini"))
               (file-exists-p (concat root "Makefile.PL"))
               (file-exists-p (concat root "Build.PL"))))))

(defun maybe-init-perl-project ()
  "Create a git + cpan project for the file visited by the current buffer."
  (let* ((filename (buffer-file-name))
         ;; seeing lib/ ensures that this is really
         ;; a project and not a one-off thing
         (root (and (string-match "^\\(.+/\\)lib/.+[.]pm$" filename)
                    (match-string 1 filename))))
  (when (and root (not (already-have-perl-project-p root)))
    (if (y-or-n-p "Create a new project here? ")
        (init-perl-project root)
      (when (not (member root do-not-want-perl-project))
        (setq do-not-want-perl-project (cons root do-not-want-perl-project)))))))

(defun init-perl-project (root)
  (interactive "DRoot: ")
  (let ((default-directory root)
        (project-name (file-name-nondirectory (directory-file-name root))))
    (when (not (file-exists-p (concat root ".git")))
      (magit-init root))
    (cperl-project-starter-build-gitignore :root root :name project-name)
    (cperl-project-starter-build-dist.ini :root root :name project-name)
    (cperl-project-starter-build-Changes :root root :name project-name)
    (cperl-project-starter-build-eproject :root root)

    (magit-run-git "commit" "-m" "project boilerplate added")

    (ignore-errors (eproject-reinitialize-project))
    (eproject-maybe-turn-on)))

(defun cperl-project-starter-git-init (root)
  (shell-command (format "cd %s; git init" root)))

(defmacro* cperl-project-starter-make-file ((root file) &body forms)
  (declare (indent 1))
  `(when (not (file-exists-p (concat ,root ,file)))
     (with-temp-buffer
       ,@forms
       (write-file (concat ,root ,file))
       (magit-run-git "add" (concat ,root ,file)))))

(defun* cperl-project-starter-build-dist.ini (&key root name)
  (cperl-project-starter-make-file (root "dist.ini")
    (insert (format "name = %s\n[@JROCKWAY]\n" name))))

(defun* cperl-project-starter-build-gitignore (&key root name)
  (cperl-project-starter-make-file (root ".gitignore")
    (insert (format "cover_db\nTAGS\n%s*\n.build\n" name))))

(defun* cperl-project-starter-build-Changes (&key root name)
  (cperl-project-starter-make-file (root "Changes")
    (insert (format "Change history for %s\n{{$NEXT}}\n" name))))

(defun* cperl-project-starter-build-eproject (&key root)
  (cperl-project-starter-make-file (root ".eproject")
    (insert ":mxdeclare-project-p nil\n")))

(provide 'cperl-project-starter)
;;; cperl-project-starter.el ends here
