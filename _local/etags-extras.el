;;;  Järneström Jonas <Jonas.Jarnestrom@ki.ericsson.se> 
;;;; A smarter
;;;  find-tag that automagically reruns etags when it cant find a               
;;;  requested item and then makes a new try to locate it.                      
;;;  Fri Mar 15 09:52:14 2002                                                   

(defadvice find-tag (around refresh-etags activate)
  "Rerun etags and reload tags if tag not found and redo find-tag.              
If buffer is modified, ask about save before running etags."
  (let* ((extension (file-name-extension (buffer-file-name))))
    (condition-case err
        ad-do-it
      (error (and (buffer-modified-p)
                  (not (ding))
                  (y-or-n-p "Buffer is modified, save it? ")
                  (save-buffer))
             (er-refresh-etags extension)
             ad-do-it))))

(defun er-refresh-etags (extension)
  "Run etags on all peer files in current dir and reload them silently."
  (interactive)
  (shell-command (format "etags *.%s" (or extension ".el")))
  (let ((tags-revert-without-query t))  ; don't query, revert silently
    (visit-tags-table default-directory nil)))

;; i wrote this:
(defun cperl-project-find-tag (tagname &optional next-p regexp-p)
  (interactive (find-tag-interactive "Find tag: "))
  "Visit the tags table associated with this project, then do the
tag search.  If it fails, then rebuild the tags table and try
again.  If that fails, well, you're fucked."
  (let* ((root (expand-file-name 
                (format "%s/.." (look-for-Makefile.PL (buffer-file-name)))))
         (tags (format "%s/TAGS" root)))
    (shell-command (format "touch %s" tags))
    (let ((tags-revert-without-query t))
      (visit-tags-table tags))
    
    (ad-deactivate 'find-tag)
    (condition-case nil
        (find-tag tagname next-p regexp-p)
      (error (message "Rebuilding tags table for %s" root)
             (shell-command 
              (format "find %s/lib | grep pm$ | etags - -o %s" root tags))
             (let ((tags-revert-without-query t))
               (visit-tags-table tags nil))
             (find-tag tagname next-p regexp-p)))
    (ad-activate 'find-tag)))

(add-hook 'cperl-mode-hook 
          (lambda nil (local-set-key (kbd "M-.") 'cperl-project-find-tag)))

(provide 'etags-extras)
