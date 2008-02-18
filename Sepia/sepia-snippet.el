(eval-when-compile
  (require 'snippet))

(defun sepia-snippet-abbrev ()
  (require 'snippet nil t)
  (when (featurep 'snippet)
    (snippet-with-abbrev-table
     'sepia-mode-abbrev-table
     ("for" . "for my $${VAR} ($${LIST}) {\n$>$.\n}$>")
     ("foreach" . "foreach my $${VAR} ($${LIST}) {\n$>$.\n}$>")
     ("if" . "if ($${TEST}) {\n$>$.\n}$>")
     ("elsif" . "elsif ($${TEST}) {\n$>$.\n}$>")
     ("else" . "else {\n$>$.\n}$>")
     ("unless" . "unless ($${TEST}) {\n$>$.\n}$>")
     ("while" . "while ($${TEST}) {\n$>$.\n}$>")
     ("until" . "until ($${TEST}) {\n$>$.\n}$>")
     ("for" . "for my $${VAR} ($${LIST}) {\n$>$.\n}$>")
     ("sub" . "sub $${NAME}\n{\n$>$.\n}$>"))))

(add-hook 'sepia-mode-hook 'sepia-snippet-abbrev)
