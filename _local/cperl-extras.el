(provide 'cperl-extras)
(require 'thingatpt)

(defun perl-insert-debug-statement ()
  (interactive)
  (insert-string "use YAML; die Dump(")
  (set-mark (point))
  (insert-string ");")
  (exchange-point-and-mark))

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
        (re-search-forward "tests\s+=>\s+" nil nil nil)
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
