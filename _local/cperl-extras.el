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
                                   "You must specify a module to use!"))
        after statement)
    (save-excursion
      (goto-char (point-max))
      (condition-case nil
          (progn (re-search-backward "^\\(use [[:alnum:]:]+\;\\)")
                 (setq after (match-string 1)))
        (error (goto-char 0)))
      (end-of-line)
      (setq statement (concat "use " (add-semicolon module)))
      (insert (concat "\n" statement))
      (message (format "Added '%s' after '%s'" statement after)))))

(defun insert-self-shift (noshift)
  (interactive)
  (if noshift
      (insert-string "my ($self) = @_;\n")
    (insert-string "my $self = shift;\n"))
  (cperl-indent-command))

(defun cpan-install ()
  (interactive)
  (let ((module (read-with-default "Module" (thing-at-point 'perl-module)
                                   "You must specify a module to install!")))

    (start-process "cpan-process" "*cpan-install*"
                   "/home/jon/perl/install/bin/cpanp" "install" module)
    (show-buffer (split-window) "*cpan-install*")))

(global-set-key "\C-c\C-i" 'cpan-install)

(defun find-tests (&optional filename)
  (interactive)
  (if (not filename) (setq filename (buffer-file-name)))
  (if (string-match "/lib/.+$" filename)
      (find-file (replace-match "/t" nil nil filename))
    (error "No idea where the tests are!")))

(provide 'cperl-extras)
