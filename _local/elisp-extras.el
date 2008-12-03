(defun macroexpand-last-sexp ()
  (interactive)
  (prin1 (macroexpand
          (read
           (save-excursion
             (re-search-backward ")")
             (thing-at-point 'sexp))))))

(add-hook 'emacs-lisp-mode-hook
          (function
           (lambda ()
             (local-set-key "\C-x\C-w" 'macroexpand-last-sexp))))

(defun doe nil "Toggle [`debug-on-error']"
  (interactive)
  (setq debug-on-error (if debug-on-error nil t)))

(require 'slime-fontifying-fu)

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (font-lock-add-keywords 'emacs-lisp-mode slime-additional-font-lock-keywords)))

(provide 'elisp-extras)
