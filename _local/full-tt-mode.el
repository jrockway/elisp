
(defvar full-tt-mode-keywords
  (concat "\\b\\(?:"
          (regexp-opt (list "GET" "CALL" "SET" "DEFAULT" "INSERT" "INCLUDE"
                            "BLOCK" "END" "PROCESS" "WRAPPER" "IF" "UNLESS"
                            "ELSIF" "ELSE" "SWITCH" "CASE" "FOR" "FOREACH"
                            "WHILE" "FILTER" "USE" "MACRO" "PERL" "RAWPERL"
                            "TRY" "THROW" "CATCH" "FINAL" "LAST" "RETURN"
                            "STOP" "CLEAR" "META" "TAGS" "IN"))
          "\\)\\b"))

(defvar full-tt-mode-vmethods
  (regexp-opt (list "defined" "length" "repeat" "replace" "remove" 
                    "match" "search" "split" "chunk" "substr" "list" 
                    "hash " "size" "keys" "values" "items" "each" 
                    "pairs " "list" "sort" "nsort" "import" "defined" 
                    "exists" "delete " "size" "item" "first" "last" 
                    "size" "max" "defined" "reverse" "join" "grep"
                    "sort" "nsort" "unshift" "shift" "pop" "unique" 
                    "import" "merge" "slice" "splice" "hash")))

(defvar full-tt-mode-filters
  (regexp-opt (list "format" "upper" "lower" "ucfirst" "lcfirst" 
                    "trim" "collapse" "html" "html_entity" "html_para" 
                    "html_break" "html_para_break" "html_line_break" 
                    "uri" "url" "indent" "truncate" "repeat" "remove" 
                    "replace" "redirect" "eval" "evaltt" "perl" "evalperl" 
                    "stdout" "stderr" "null" "latex")))

(defvar full-tt-mode-font-lock-keywords nil)
(setq full-tt-mode-font-lock-keywords
      `((,(format "\\(%s\\)" full-tt-mode-keywords)
         (1 font-lock-keyword-face nil))
        (,(format "[.]\\(%s\\)" full-tt-mode-vmethods)
         (1 font-lock-constant-face nil))
        ("\\b\\([[:alnum:]_]+\\)[.]"
         (1 font-lock-variable-name-face nil))
        (,(format "|[[:space:]]*\\(%s\\)" full-tt-mode-filters)
         (1 font-lock-type-face nil))
        ("|[[:space:]]*\\([[:alnum:]_+]+\\)"
         (1 font-lock-function-name-face nil))
        ("\\(\\$[[:alnum:]_]+\\)"
         (1 font-lock-variable-name-face nil))))

(defvar full-tt-mode-syntax-table)
(defvar full-tt-mode-mode-map)  
(defvar full-tt-mode-hook)

(defun full-tt-mode-init-syntax nil
  (modify-syntax-entry ?_ ".")
  (modify-syntax-entry ?# "<")
  (modify-syntax-entry ?\n ">")
  (modify-syntax-entry ?\' "\""))

(define-derived-mode full-tt-mode fundamental-mode "Full TT"
  "Major mode for editing Template Toolkit files.
\\{full-tt-mode-map}"
  (interactive)
  (full-tt-mode-init-syntax)
  (setq font-lock-defaults '(full-tt-mode-font-lock-keywords nil nil)))

(ignore-errors (require 'mmm-mode))
(when (fboundp 'mmm-add-classes)
  (mmm-add-classes
  '((tt-mmm-mode
     :submode full-tt-mode
     :face mmm-output-submode-face
     :front "\\[%[+-]?"
     :back  "[+-]?%\\]")))
  (mmm-add-mode-ext-class 'html-mode "\\.tt2?" 'tt-mmm-mode))

(provide 'full-tt-mode)
