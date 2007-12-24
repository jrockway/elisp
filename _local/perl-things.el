; this file is "things" for thing-at-point related to Perl
(require 'thingatpt)

(defun backwards-chars-then-look-for (backwards &optional forward)
  "Starting from the point, look backwards for start of something enjoyable,  
   then look forwards for the end of it.  Return the start and end as a cons
   cell, or nil if we don't see anything."
  (if (not forward) (setq forward (format "[%s]+" backwards)))
  (save-excursion
    (skip-chars-backward backwards)  ; backwards to start of interest
    (if (looking-at forward)         ; forward to the end of it
        (cons (point) (match-end 0))
      nil)))
  
(defun bounds-of-module-at-point ()
  "Determine where a module name starts for (thing-at-point 'perl-module)"
  (backwards-chars-then-look-for "_[:alpha:]:\\->" "[_[:alpha:]:]+"))

(defun bounds-of-var-at-point ()
  "Determine where a perl variable name starts for (thing-at-point 'perl-variable)"
  (backwards-chars-then-look-for "[:alpha:]_$@#%*&=" "[[:alpha:]_$@#%*&]+"))

; tell thing-at-point about this stuff
(put 'perl-module   'bounds-of-thing-at-point 'bounds-of-module-at-point)
(put 'perl-variable 'bounds-of-thing-at-point 'bounds-of-var-at-point)

(provide 'perl-things)
