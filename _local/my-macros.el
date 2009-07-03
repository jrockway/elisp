
(defmacro save-excursion-rewind (&rest body)
  "Like save-excursion, but rewinds to `point-min' before executing BODY"
  `(save-excursion
     (goto-char (point-min))
     ,@body))

(defmacro protect-unwind (unwindform &rest bodyforms)
  "Like unwind-protect, but with arguments reversed"
  `(unwind-protect (progn ,@bodyforms) ,unwindform))

; XXX not really a macro
(defun read-with-default (string &optional default error)
  (let ((read (read-string
               (if default
                   (format "%s (default %s): " string default)
                 (format "%s: " string)))))
    (if (equal read "") (setq read nil))
    (if (and (not read) (not default)) (error error))
    (if (not read) (setq read default))
    read))

(defmacro define-invoke-for-each (name orig &optional docstring varname)
  "Make a function of one argument into a function of multiple arguments.
Argument NAME is the name of the generated function.
Argument ORIG is the original function.
Optional argument DOCSTRING is the docstring of the generated function.
Optional argument VARNAME is the name of the argument to the generated function."
  `(defun ,name (,(or varname 'things))
     ,(or docstring
          (format "Invoke %s on each element of THINGS." orig))
     (loop for thing in ,(or varname 'things) do (,orig thing))))

(defmacro* let+ (to-bind &body forms)
  "Bind TO-BIND to _ and execute FORM."
  `(let ((_ ,to-bind))
     ,@forms))


(provide 'my-macros)
