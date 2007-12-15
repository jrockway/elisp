(require 'thingatpt)
(require 'perl-things)

(defun prepare-var-for-dump (var)
  (let* ((type (substring var 0 1)))
    (if (equal type "$") var (concat "\\" var))))

(defun perl-insert-debug-statement (comment)
  "Insert 'use YAML; die Dump(`var')' where `var' is the variable near the
   point.  If invoked with an argument, comments out the line where `var' 
   is found."
  (interactive "P")
  (let ((var (or (thing-at-point 'perl-variable)
                 (read-from-minibuffer "Expression to dump: "))))
    (if comment (progn 
                  (beginning-of-line)
                  (cperl-indent-command)
                  (insert-string "# ")))
    (end-of-line)
    (insert-string (format "\nuse YAML; die Dump(%s);" 
                           (prepare-var-for-dump var)))
    (cperl-indent-command)))

(defun increment-number-at-point (&optional amount)
  "Increment the number under point by `amount'"
  (interactive "p")
  (let ((num (number-at-point)))
    (when (numberp num)
      (let ((newnum (+ num amount)) 
            (p (point)))
        (save-excursion
          (skip-chars-backward "-.0123456789")
          (delete-region (point) (+ (point) (length (number-to-string num))))
          (insert (number-to-string newnum)))
        (goto-char p) 
        newnum))))

(defun increment-test-counter (&optional amount)
  "Increment the Test::More test counter by `amount'"
  (interactive "p")
  (save-excursion
    (goto-char (point-min))
    (condition-case nil
        (re-search-forward "tests\s+=>\s+")
      (error (error "No Test::More counter found!")))
    (message "Counter is now %d" (increment-number-at-point amount))))

(defun read-with-default (string &optional default error)
  (let ((read (read-string
               (if default 
                   (format "%s (default %s): " string default)
                 (format "%s: " string)))))
    (if (equal read "") (setq read nil))
    (if (and (not read) (not default)) (error error))
    (if (not read) (setq read default))
    read))

(defun add-semicolon (string)
  (if (string-match ";" string)
      string
    (concat string ";")))

(defun add-use ()
  "Add a new perl use statement after the existing use statements."
  (interactive)
  (let ((module (read-with-default "Module" (thing-at-point 'perl-module)
                                   "You must specify a module to use!")))
    (save-excursion
      (goto-char (point-max))
      (condition-case nil
          (re-search-backward "^\\(use .+;\\)")
        (error (goto-char 0)))
      (end-of-line)
      (insert (concat "\nuse " (add-semicolon module))))))

(provide 'cperl-extras)
