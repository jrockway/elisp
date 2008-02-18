;;; sepia-w3m.el --- The add-on program to view Perl documents.

;; Copyright (C) 2001 TSUCHIYA Masatoshi <tsuchiya@namazu.org>
;; Modified 2004 by Sean O'Rourke to work with Sepia and operate on
;; buffer.

;; Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>
;; Keywords: w3m, perldoc

;; This file is a part of emacs-w3m.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.


;;; Commentary:

;; w3m-perldoc.el is the add-on program of emacs-w3m to view Perl
;; documents.  For more detail about emacs-w3m, see:
;;
;;    http://emacs-w3m.namazu.org/

;;; Code:
(eval-when-compile
  (require 'w3m-perldoc))

;;;###autoload
(defun w3m-about-perldoc-buffer (url &optional no-decode no-cache &rest args)
  "Handle about://perldoc-buffer/ links."
  (when (string-match "\\`about://perldoc-buffer/" url)
    (let ((buf (get-buffer (w3m-url-decode-string
			    (substring url (match-end 0)))))
	  (default-directory w3m-profile-directory)
	  (process-environment (copy-sequence process-environment)))
      ;; To specify the place in which pod2html generates its cache files.
      (setenv "HOME" (expand-file-name w3m-profile-directory))
      (insert-buffer-substring buf)
      (when (zerop (apply #'call-process-region
			  (point-min) (point-max)
			  w3m-perldoc-pod2html-command
			  t '(t nil) nil
			  (append w3m-perldoc-pod2html-arguments
				  '("--htmlroot=about://perldoc-buffer"))))
	(let ((case-fold-search t))
	  (goto-char (point-min))
	  (while (re-search-forward
		  "<a href=\"about://perldoc\\(-buffer\\)?/\\([^\"]*\\)\\(\\.html\\)\">" nil t)
	    (delete-region (match-beginning 3) (match-end 3))
	    (save-restriction
	      (narrow-to-region (match-beginning 2) (match-end 2))
	      (while (search-backward "/" nil t)
		(delete-char 1)
		(insert "::"))
	      (goto-char (point-max))))
	  "text/html")))))

;;;###autoload
(defun sepia-w3m-view-pod (&optional buffer)
  (require 'w3m)
  (w3m-goto-url (concat "about://perldoc-buffer/"
			(w3m-url-encode-string (buffer-name buffer)))))

;;;###autoload
(defun sepia-module-list ()
  "List installed modules with links to their documentation.

This lists not just top-level packages appearing in packlist
files, but all documented modules on the system, organized by
package."
  (interactive)
  (let ((file "/tmp/modlist.html"))
    (unless (file-exists-p file)
      (sepia-eval (format "Sepia::html_module_list(\"%s\")" file)))
    (w3m-find-file file)))

;;;###autoload
(defun sepia-package-list ()
  "List installed packages with links to their documentation.

This lists only top-level packages appearing in packlist files.
For modules within packages, see `sepia-module-list'."
  (interactive)
  (let ((file "/tmp/packlist.html"))
    (unless (file-exists-p file)
      (sepia-eval (format "Sepia::html_package_list(\"%s\")" file)))
    (w3m-find-file file)))

(defun sepia-w3m-create-imenu ()
  "Create imenu index from pod2html output."
  (save-excursion
    (goto-char (point-min))
    (when (looking-at "Location: \\(about://perldoc/[^#]+\\)")
      (let ((base (match-string 1))
            beg end
            list)
        (w3m-view-source)
        (search-forward "<!-- INDEX BEGIN -->")
        (setq beg (point))
        (search-forward "<!-- INDEX END -->")
        (setq end (point))
        (goto-char beg)
        (while (re-search-forward "<a href=\"\\(#[^\"]+\\)\">\\([^<]+\\)" end t)
          (push (cons (match-string 2) (match-string 1)) list))
        (w3m-view-source)
        (nreverse list)))))

(defun sepia-w3m-goto-function (name anchor)
  (if (string-match "^about://perldoc/" w3m-current-url)
      (w3m-goto-url (concat w3m-current-url anchor))
    (imenu-default-goto-function name anchor)))

(defun sepia-w3m-install-imenu ()
  (setq imenu-create-index-function 'sepia-w3m-create-imenu
        imenu-default-goto-function 'sepia-w3m-goto-function))

(provide 'sepia-w3m)

;;; sepia-w3m.el ends here.
