
(defun swap-strict-and-moose ()
  (interactive)
  "Changes `use strict; use warnings' to `use Moose' or the reverse"
  (save-excursion-rewind 
   (when (re-search-forward "use strict;\nuse warnings;" nil t)
     (replace-match "use Moose;"))
   (when (re-search-forward "use Moose;" nil t)
     (replace-match "use strict;\nuse warnings;"))))

(provide 'cperl-moose)