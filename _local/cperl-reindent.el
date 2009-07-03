(defun look-for-opening-without-close nil
  "Look backwards from the point for an unbalanced opening brace.
 For example, in `{ foo => sub {}, bar => baz X', we'll match the
 leading `{'.  Returns the position of the match, or nil if there
 is no match."
  ;; TODO, if point is on or after a )}] character, move back one and
  ;; do this stuff; it only seems right.
  (let ((state 0) pos)
    (save-excursion
      (while (>= state 0)
        (if (not (re-search-backward
                  (format "\\(%s\\)\\|\\(%s\\)"
                          (regexp-opt (list "[" "(" "{")) ; opening
                          (regexp-opt (list "]" ")" "}"))) ; closing
                  nil t))

            ; no match, break out of loop
            (setq state -1)

          (if (match-string-no-properties 2) ; opening or closing?
              (progn (setq state (+ state 1)))
            (setq state (- state 1))
            (match-beginning 0)
            (setq pos (point))))) pos)))

(defun narrow-to-sexp-area nil
  "Narrow from the point to the nearest unmatched opening thingie"
  (interactive)
  (let ((start (+ 1 (look-for-opening-without-close))) (end (point)))
    (save-excursion
      (goto-char start)
      (when (looking-at "\n")
        (next-line)
        (beginning-of-line)
        (setq start (point))))
    (narrow-to-region start end)))

(defun cperl-reindent-eol nil
  (save-excursion (end-of-line) (point)))

(defun cperl-reindent-bol nil
  (save-excursion (beginning-of-line) (point)))

(defvar cperl-reindent-hash-key-regex
  "[^[:space:]][[:space:]]*=>")

(defun cperl-reindent-hash-target nil
  "What column should be = in `=>' be at?"
  (let ((ret 0))
    (ignore-errors
      (while t
        (beginning-of-line)
        (let ((offset (point)))
          (re-search-forward cperl-reindent-hash-key-regex)
          (setq ret (max ret (+ 1 (- (match-beginning 0) offset)))))
        (next-line)))
    ret))

(defun cperl-reindent-hash nil
  "Inside an `sexp' that contain pairs, reformat the pairs correctly."
  (interactive)
  (save-excursion
    (save-restriction
      (when (save-excursion
              (forward-line 0)
              (looking-at "[[:space:]]*[})];?"))
        (forward-line 0))
      (narrow-to-sexp-area)
      (goto-char (point-min))
      (let ((target (+ 1 (cperl-reindent-hash-target))))
        (goto-char (point-min))
        (ignore-errors
          (while t
            (re-search-forward cperl-reindent-hash-key-regex)
            (let* ((bol (cperl-reindent-bol))
                   (cur (+ 1 (match-beginning 0)))
                   (col (- cur bol))
                   (spaces (- target col)))
              (goto-char cur)
              (if (looking-at "[[:space:]]+")
                  (replace-match "" nil t))
              (dotimes (i spaces) (insert " "))
              (re-search-forward "=>")
              (if (looking-at "[[:space:]]*")
                  (replace-match " " nil t))
              (end-of-line))))))))

(provide 'cperl-reindent)