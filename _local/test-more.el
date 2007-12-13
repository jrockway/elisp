(provide 'test-more)

(defmacro with-temp-string (string &rest body)
  `(with-temp-buffer (insert-string ,string) ,@body))

(defmacro cons-eq (a b)
  (let ((atmp (make-symbol "a"))
        (btmp (make-symbol "b")))
    `(let ((,atmp ,a)
           (,btmp ,b))
       (and (eq (car ,atmp) (car ,btmp))
            (eq (cdr ,atmp) (cdr ,btmp))))))

(defmacro push-test (var)
  `(setq ,var (append ,var (list (cons counter message)))))

(defun runtests (&rest tests)
  (let ((buf (or (get-buffer "*runtests*") 
                 (get-buffer-create "*runtests*")))
        (counter 0)
        pass fail)
    (switch-to-buffer-other-window buf)
    (goto-char (point-max))
    (while tests
      (let ((test (car tests)))
        (setq counter (+ 1 counter))
        (setq tests (cdr tests))
        (let ((passed (car test)) 
              (message (cdr test)))
          (insert-string 
           (format 
            (if message "%s %d - %s\n" "%s %d\n") 
            (if passed (progn (push-test pass) "ok")
              (progn (push-test fail) "not ok")) 
            counter message)))))
    (insert-string (format "Result: %s\n\n" (if (not fail) "PASS" "FAIL")))
    (list pass fail)))

(defun is-deeply (got expected &optional message)
  (let ((result (cons-eq got expected)))
    (if (not result)
        (setq message 
              (concat message 
                      (format "\n# got:      %s\n# expected: %s" got expected))))
    (cons result message)))
