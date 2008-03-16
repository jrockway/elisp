;;; html-imenu --- imneu suport for html modes
;;
;; This is a very slightly modified version of
;; html-helper-imenu.el. This version comes with nXhtml.
(defconst html-imenu:version "0.9") ;;Version:
;;
;; ~/share/emacs/pkg/html/html-helper-imenu.el ---
;;
;; $Id: html-helper-imenu.el,v 1.11 2004/03/23 07:39:37 harley Exp $
;;

;; Author:    Harley Gorrell <harley@panix.com>
;; URL:       http://www.mahalito.net/~harley/elisp/html-helper-imenu.el
;; License:   GPL v2
;; Keywords:  html-helper, imenu, html, table of contents

;;; Commentary:
;; * Adds an indented table of contents to the menubar
;; * The regexp only matches headers on a single line
;;   and well formed tags.  (Which is pretty common.)
;;
;; Put somthing like the following in your .emacs:
;; (autoload 'html-helper-imenu-setup "html-helper-imenu")
;; (add-hook 'html-helper-mode-hook 'html-helper-imenu-setup)
;;
;; While this was originaly written for html-helper,
;; It will work with sgml-mode and others.
;;
;; http://www.santafe.edu/~nelson/hhm-beta/html-helper-mode.el

;;; History:
;;
;; 1998-06-25 : added regexp
;; 2003-03-18 : updated contact info
;; 2004-03-22 : minor clean up
;; 2007-11-23 : changed setup function to do nothing if done already

;;; Code:

(defvar html-imenu-title "TOC"
  "*Title of the menu which will be added to the menubar.")

(defvar html-imenu-regexp
  "\\s-*<h\\([1-9]\\)[^\n<>]*>\\(<[^\n<>]*>\\)*\\s-*\\([^\n<>]*\\)"
  "*A regular expression matching a head line to be added to the menu.
The first `match-string' should be a number from 1-9.
The second `match-string' matches extra tags and is ignored.
The third `match-string' will be the used in the menu.")

;; Make an index for imenu
(defun html-imenu-index ()
  "Return an table of contents for an html buffer for use with Imenu."
  (let ((space ?\ ) ; a char
	(toc-index '())
	toc-str)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward html-imenu-regexp nil t)
	(setq toc-str
	      (concat
	       (make-string
		(* 6 (- (string-to-number (match-string 1)) 1))
		space)
	       (match-string 3)))
	(beginning-of-line)
	(setq toc-index (cons (cons toc-str (point)) toc-index))
	(end-of-line) ))
    (nreverse toc-index)))

(defun html-imenu-setup ()
  "Setup the variables to support imenu."
  (interactive)
  ;; Fix-me: It looks like this function has to be called every time
  ;; switching to some html mode in mumamo. Values are "survived" by
  ;; mumamo, but the menu item disappears.
  (setq imenu-create-index-function 'html-imenu-index)
  (setq imenu-sort-function nil) ; sorting the menu defeats the purpose
  (imenu-add-to-menubar html-imenu-title))

(provide 'html-imenu)

;;; html-imenu ends here
