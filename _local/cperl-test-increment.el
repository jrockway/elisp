(require 'my-macros)

(defun increment-number-at-point (&optional amount)
  "Increment the number under point by AMOUNT."
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
  "Increment the Test::More test counter by AMOUNT."
  (interactive "p")
  (save-excursion-rewind
    (condition-case nil
        (re-search-forward "tests\s+=>\s+")
      (error (error "No Test::More counter found!")))
    (message "Counter is now %d" (increment-number-at-point amount))))

(provide 'cperl-test-increment)

