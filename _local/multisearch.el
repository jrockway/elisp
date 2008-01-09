(defun build-multisearch-terms ()
  "Interactively prompts for search/replacement pairs and returns a
list of (search . replacement) cons cells"
  (let (result done)
    (while (not done)
      (let* ((search  (read-from-minibuffer "Term to replace: " ))
             (replace (read-from-minibuffer 
                       (format "Replace %s with: " search))))
        (if (and (equal search "") (equal replace ""))
            (setq done t)
          (setq result (append result (list (cons search replace)))))))
    result))

(defun do-multisearch (terms)
  "given a list of cons cells, search for each car and replace with each cdr"
  (let ((main-regex (regexp-opt (mapcar 'car terms))))
    (while (re-search-forward main-regex nil t)
      (goto-char (match-beginning 0))
      (let (found (terms terms))
        (while (and (not found) terms)
          (let* ((term (car terms))
                 (search (car term))
                 (replace (cdr term)))
            (if (and (looking-at search) 
                     (y-or-n-p (format "Replace match for '%s' with '%s'? "
                                       search replace)))
                     (progn (setq found t)
                            (replace-match replace))))
          (setq terms (cdr terms)))))))

(defun multisearch ()
  (interactive)
  (do-multisearch (build-multisearch-terms)))

(provide 'multisearch)
