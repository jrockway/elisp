
(defun swap-strict-and-moose ()
  (interactive)
  "Changes `use strict; use warnings' to `use Moose' or the reverse"
  (save-excursion-rewind
   (when (re-search-forward "use strict;\nuse warnings;" nil t)
     (replace-match "use Moose;"))
   (when (re-search-forward "use Moose;" nil t)
     (replace-match "use strict;\nuse warnings;"))))

(defun convert-moose-to-declare ()
  "Convert package Foo; use Moose; to the MX::D equivalent."
  (interactive)
  (save-excursion-rewind
   (when (re-search-forward "package \\([^[:space:]]+\\);")
     (replace-match
      (format "use MooseX::Declare;\n\nclass %s {\n\n};" (match-string 1)))
     (next-line)
     (delete-region (progn (beginning-of-line) (point))
                    (progn (end-of-line) (point))))))

(provide 'cperl-moose)
