(require 'cperl-project)

(defun looking-at-requires ()
  "Returns t if we are looking-at requires or build_requires.
Sets match; capture 1 is the requires keyword (or
build_requires), capture 2 is the module name"
  (looking-at "\\(\\(?:build_\\)?requires\\)[[:space:]]+[^A-Za-z9-0_]+\\([A-Za-z0-9_:]+\\)"))

(defun parse-requires-line ()
  "Looks for a requires 'Foo' or build_requires 'Foo' statement
on the current line of the current buffer and updates the requires
and build-requires lists accordingly"
  (when (looking-at-requires)
    (let* ((type (match-string-no-properties 1))
           (module (match-string-no-properties 2))
           (version (save-excursion ; parse "Foo::Bar" => 'foo' into 'foo'
                      (let ((eol (save-excursion (end-of-line) (point))))
                        (goto-char (match-end 0))
                        (forward-char)
                        (skip-chars-forward "[:space:]=>")
                        (if (re-search-forward
                             "\\(['\"]\\)\\(.+\\)\\1" eol t)
                            (match-string-no-properties 0)
                          nil))))
           (result (if version (cons module version) module))
           slot)
      (cond
       ((equal type "requires") (setq slot 'requires))
       ((equal type "build_requires") (setq slot 'build-requires)))
      (add-to-list slot result))))

(defun is-module-install ()
  "Determines if the current buffer is a Module::Install
Makefile.PL.  Throws an error if not, otherwise returns t."
  (save-excursion-rewind ; check for MI
    (if (not (search-forward "use inc::Module::Install" nil t))
        (error "Not a Module::Install Makefile.PL!")
      t)))

(defun parse-Makefile.PL ()
  "Parse a Makefile.PL in the current buffer and return a list of
prereqs in a (requires . build-requires) cons cell"
  (save-excursion
    (goto-char (point-min))
    (is-module-install)
    (let (requires build-requires)
      (while (zerop (forward-line 1)) (parse-requires-line))
      (cons requires build-requires))))

(defmacro bounds-of (&rest move-around)
  "Executes MOVE-AROUND and returns `(point)' there"
  `(save-excursion ,@move-around (point)))

(defun write-requires-line (type def)
  (if (not (listp def))
      (insert (format "%s '%s';\n" type def))
    (insert (format "%s '%s' => %s;\n" type (car def) (cdr def)))))

(defun sort-modules (a b)
  (string< (if (listp a) (car a) a)
           (if (listp b) (car b) b)))

(defun rewrite-Makefile.PL-requires (requires)
  "Given a (requires . build-require) cons cell REQUIRES, kill
the existing requires and build_requires statements and
regenerate them from the REQUIRES list"
  (let ((r (car requires))
        (b (cdr requires)))
    (setq r (sort r 'sort-modules))
    (setq b (sort b 'sort-modules))
    (setq requires (cons r b)))
  (let (where)
    (save-excursion-rewind ; first, blow away requires
      (while (zerop (forward-line 1))
        (if (looking-at-requires)
            (progn
              (if (not where) (setq where (point))) ; save start position
              (delete-region (bounds-of (beginning-of-line))
                             (bounds-of (end-of-line) (forward-char)))
              (backward-char)))))
      (save-excursion
        ; goto where we want to insert
        (if where (goto-char where) ; where the old stuff was
          (bounds-of                ; or before WriteAll()
           (or (re-search-forward "WriteAll" nil t) (goto-char (point-max)))))
        (if (not (save-excursion (forward-line -1) (looking-at "^$")))
            (insert "\n"))
        (mapc (lambda (arg) (write-requires-line (car arg) (cdr arg)))
              (append
               (mapcar (lambda (arg) (cons "requires" arg))
                       (car requires))
               (mapcar (lambda (arg) (cons "build_requires" arg))
                       (cdr requires)))))))

(defun add-requires-to-Makefile.PL (makefile requires &optional build-requires)
  "Visits MAKEFILE and adds elements of REQUIRES to the requires
section of it; if BUILD-REQUIRES is non-nil, add the elements of
the list to the build_requires section."
  (save-excursion
    (let ((kill-when-done (not (find-buffer-visiting makefile)))
          (cperl-no-flymake t)) ; suppress flymake for a file we won't even see
      (protect-unwind (if kill-when-done (kill-buffer nil))
        (find-file makefile)
        (let* ((all (parse-Makefile.PL))
               (r (car all))
               (b (cdr all)))
          (mapc (lambda (arg) (add-to-list 'r arg)) requires)
          (mapc (lambda (arg) (add-to-list 'b arg)) build-requires)
          (rewrite-Makefile.PL-requires (cons r b))
          (save-buffer))))))

(defun add-requires-to-Makefile.PL-by-file-type (&rest modules)
  (let* ((f (buffer-file-name))
         (makefile (look-for-Makefile.PL f)))
    (cond
     ((perl-module-lib-file-p f)
      (add-requires-to-Makefile.PL makefile modules))
     ((perl-module-test-file-p f)
      (add-requires-to-Makefile.PL makefile nil modules))
     (t (error "Not a library or test file!")))))

(defun add-Makefile.PL-requires ()
  (interactive)
  (let ((module (read-with-default "Module" (thing-at-point 'perl-module)
                                   "You must specify a module!")))
    (add-requires-to-Makefile.PL-by-file-type module)))

(defun visit-Makefile.PL ()
  (interactive)
  (find-file (look-for-Makefile.PL)))

(provide 'cperl-makefile)
