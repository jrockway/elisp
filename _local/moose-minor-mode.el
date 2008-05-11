(defcustom moose-minor-mode-class-keywords
  (list "has" "before" "after" "around" "inner" "augment" "meta" "extends")
  "A list of functions recognized as Moose class keywords.")

(defcustom moose-minor-mode-attribute-keywords 
  (list "is" "isa" "traits" "default" "builder" "accessor" 
        "reader" "writer")
  "A list of hash keys recognized as keywords in
`has attribute => (...)' declarations."
  :tag "Keywords in attribute declarations")

(defvar moose-minor-mode-font-lock-keywords nil)
(setq moose-minor-mode-font-lock-keywords
      `(("\\(has\\)[[:space:]]+\\(['\"]?\\)\\([[:alnum:]]+\\)\\2[[:space:]]+=>[[:space:]]+([[:space:]+\n]\\(?:[[:space:]]+\\(reader\\|writer\\|is\\)[[:space:]]+=>.+[\n]?\\)+[[:space:]]*)"
         (1 font-lock-keyword-face t)
         (3 font-lock-string-face t) ; moose-attribute-face?
         (4 font-lock-builtin-face prepend))
        ("\\(before\\|after\\)"
         (1 font-lock-keyword-face nil))))

;(define-minor-mode moose-minor-mode
;  "A minor mode for font-locking Moose keywords." nil " Moose"
;  (font-lock-add-keywords 'cperl-mode moose-minor-mode-font-lock-keywords))

(defun moose-extend nil
  (let (start end (point (point)))
    (when (and
           (setq start (re-search-backward "^has" nil t))
           (setq end   (re-search-forward "^has.+=>[ ]*([ ]*\n\\(?:[^)]+\n\\)+)"))
           (> end point)
           (< start point))
      (setq font-lock-beg start)
      (setq font-lock-end end)
      t)))
  
(define-derived-mode moose-mode fundamental-mode "Moose"
  (setq font-lock-defaults '(moose-minor-mode-font-lock-keywords nil nil))
  (setq font-lock-extend-region-functions '(moose-extend)))
