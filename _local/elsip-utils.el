(provide 'elisp-utils)
(defun expand-macro (expression)
  (interactive "xExpand macro: ")
  (message (macroexpand expression)))
