;; tt-mode.el --- Emacs major mode for editing Template Toolkit files
;;
;; Copyright (c) 2002 Dave Cross, all rights reserved.
;;
;; This file may be distributed under the same terms as GNU Emacs.
;;
;; $Id: tt-mode.el,v 1.1.1.1 2006/01/10 21:49:53 dave Exp $
;;
;; This file adds simple font highlighting of TT directives when you are
;; editing Template Toolkit files.
;;
;; I usually give these files an extension of .tt and in order to automatically
;; invoke this mode for these files, I have the following in my .emacs file.
;;
;; (setq load-path
;;      (cons "/home/dave/xemacs" load-path))
;; (autoload 'tt-mode "tt-mode")
;; (setq auto-mode-alist
;;  (append '(("\\.tt$" . tt-mode))  auto-mode-alist ))
;;
;; Something similar may well work for you.
;;
;; Author: Dave Cross <dave@dave.org.uk>
;;
;;
;; $Log: tt-mode.el,v $
;; Revision 1.1.1.1  2006/01/10 21:49:53  dave
;; dave.org.uk web site
;;
;; Revision 1.6  2004/01/30 12:32:50  dave
;; Added (previously missing) FOR directive to list of keywords.
;; Added support for TT comments.
;; (Thanks to Sam Vilian for these fixes)
;;
;; Revision 1.5  2002/06/16 10:01:24  dave
;; A final fix to the [% ... %] regex. It now seems to to everything
;; I want :)
;;
;; Revision 1.4  2002/06/15 20:00:13  dave
;; Added list of TT keywords
;;
;; Revision 1.3  2002/06/15 15:08:03  dave
;; Added a bit more complexity to the regex
;;
;; Revision 1.2  2002/06/15 14:35:26  dave
;; Improved regex to match [% ... %]
;;
;; Revision 1.1.1.1  2002/06/15 13:51:56  dave
;; Initial Version
;;
;;

(require 'font-lock)

(defvar tt-mode-hook nil
  "List of functions to call when entering TT mode")

(defvar tt-keywords
  (concat "\\b\\(?:"
          (regexp-opt (list "GET" "CALL" "SET" "DEFAULT" "INSERT" "INCLUDE"
                            "BLOCK" "END" "PROCESS" "WRAPPER" "IF" "UNLESS"
                            "ELSIF" "ELSE" "SWITCH" "CASE" "FOR" "FOREACH"
                            "WHILE" "FILTER" "USE" "MACRO" "PERL" "RAWPERL"
                            "TRY" "THROW" "CATCH" "FINAL" "LAST" "RETURN"
                            "STOP" "CLEAR" "META" "TAGS"))
          "\\)\\b"))

(defvar tt-start-tag "\\[%[+-]")
(defvar tt-end-tag "[+-]%\\]")
(defvar tt-body "\\(?:.+?\\|\n\\)+")

(defun tt-find-full-expression
  (re-search-forward
   (format "\\(%s\\) \\(%s\\) \\(%s\\)" tt-start-tag tt-body tt-end-tag) nil t))

(defvar tt-font-lock-keywords
  (list
    ;; Fontify [% ... %] expressions
   (list tt-find-full-expression
         '(1 font-lock-string-face t)
         '(2 font-lock-variable-name-face t)
         '(3 font-lock-string-face t))
   ;; Look for keywords within those expressions
   (list (format "%s \\(%s\\) " tt-start-tag tt-keywords)
         '(1 font-lock-keyword-face t))
   '("\\[% *\\(#.*?\\)%\\]"
     (1 font-lock-comment-face t)))
  "Expressions to font-lock in tt-mode.")

(defun tt-tag-occurs (tag start end)
  (let ((count 0))
    (goto-char start)
    (while (re-search-forward (regexp-quote tag) end t)
      (incf count))
    count))

(defun tt-adjust-font-lock-region nil
  ;(message "Adjusting region! %d %d" font-lock-beg font-lock-end)

  (let (ret) 
    (setq ret (catch :return 
    ;; see if we are inside a [% ... %] with both tags outside of regio
    (ignore-errors 
      (let ((near-close (re-search-backward "%\\]" nil t))
            (near-open  (re-search-backward "\\[%" nil t)))
        (when (> near-open near-close) ; we see a %] ... [% we are inside [%
          (setq font-lock-beg near-open)
          (throw :return t)))
      
      (let ((near-close (re-search-forward "%\\]" nil t))
            (near-open  (re-search-forward "\\[%" nil t)))
        (when (< near-close near-open) ; we see a %] ... [% we are inside %]
          (setq font-lock-end near-close)
          (throw :return t))))
    
    ;; see if [% ... %] is unbalanced in the current region
    (let ((opening (tt-tag-occurs "[%" font-lock-beg font-lock-end))
          (closing (tt-tag-occurs "%]" font-lock-beg font-lock-end))
          result)
      (if (/= opening closing) (setq result t))
      (cond
       ((> opening closing) ; more opening tags, expand forwards
        (setq font-lock-end 
              (or (re-search-forward "%\\]" nil t) (point-max))))
       ((< opening closing) ; more closing tags, expand backwards
        (setq font-lock-beg
              (or (re-search-backward "\\[%" nil t) (point-min)))))
      (throw :return result))))
    
    (if ret
        (message "font lock region expanded to -> %s"
                 (let ((s (buffer-substring-no-properties font-lock-beg font-lock-end)))
                   (while (string-match "\n" s) 
                     (setq s (replace-match " [n] " nil nil s))) s))
      (message "region not expanded furtuer"))
    
    ret))


  
(defun tt-mode ()
  "Major mode for editing Template Toolkit files"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'tt-mode)
  (setq mode-name "TT")
  (if (string-match "Xemacs" emacs-version)
      (progn
	(make-local-variable 'font-lock-keywords)
	(setq font-lock-keywords tt-font-lock-keywords))
    ;; Emacs
    (setq font-lock-defaults '(tt-font-lock-keywords nil t))
    (setq font-lock-extend-region-functions '(tt-adjust-font-lock-region)))
  (font-lock-mode)
  (run-mode-hooks 'tt-mode-hook))

(provide 'tt-mode)

;; tt-mode.el ends here
