;;; Sepia -- Simple Emacs-Perl InterAction: ugly, yet effective.
;;; (a.k.a. Septik -- Sean's Emacs-Perl Total Integration Kludge.)

;; Copyright (C) 2004-2007 Sean O'Rourke.  All rights reserved, some
;; wrongs reversed.  This code is distributed under the same terms as
;; Perl itself.

;;; Commentary:

;; Sepia is a set of tools for Perl development in Emacs.  Its goal is
;; to extend CPerl mode with two contributions: fast code navigation
;; and interactive development.  It is inspired by Emacs' current
;; support for a number of other languages, including Lisp, Python,
;; Ruby, and Emacs Lisp.
;;
;; See sepia.texi, which comes with the distribution.

;;; Code:

(require 'cperl-mode)
(require 'gud)
(require 'cl)
;; try optional modules, but don't bitch if we fail:
(ignore-errors (require 'sepia-w3m))
(ignore-errors (require 'sepia-tree))
(ignore-errors (require 'sepia-ido))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Comint communication

(defvar sepia-perl5lib nil
"* List of extra PERL5LIB directories for `sepia-repl'.")

(defvar sepia-program-name "perl"
"* Perl program name.")

(defvar sepia-view-pod-function
  (if (featurep 'w3m) 'sepia-w3m-view-pod 'sepia-perldoc-buffer)
"* Function to view current buffer's documentation.

Useful values include `sepia-w3m-view-pod' and `sepia-perldoc-buffer'.")

(defvar sepia-module-list-function
  (if (featurep 'w3m) 'w3m-find-file 'browse-url-of-buffer)
"* Function to view a list of installed modules.

Useful values include `w3m-find-file' and `browse-url-of-buffer'.")

(defvar sepia-complete-methods t
"* Non-nil if Sepia should try to complete methods for \"$x->\".

NOTE: this feature can be problematic, since it evaluates the
object in order to find its type.  Currently completion is only
attempted for objects that are simple scalars.")

(defvar sepia-indent-expand-abbrev t
"* If non-NIL, `sepia-indent-or-complete' tries `expand-abbrev'.")

(defvar sepia-use-completion t
  "* Use completion based on Xref database.

Turning this off may speed up some operations, if you don't mind
losing completion.")

(defvar sepia-eval-defun-include-decls t
  "* Generate and use a declaration list for `sepia-eval-defun'.
Without this, code often will not parse; with it, evaluation may
be a bit less responsive.  Note that since this only includes
subs from the evaluation package, it may not always work.")

(defvar sepia-prefix-key "\M-."
  "* Prefix for functions in `sepia-keymap'.")

;;; User options end here.

(defvar sepia-process nil
"The perl process with which we're interacting.")
(defvar sepia-output nil
"Current perl output for a response to `sepia-eval-raw', appended
to by `perl-collect-output'.")
(defvar sepia-passive-output ""
"Current perl output for miscellaneous user interaction, used to
look for \";;;###\" lisp evaluation markers.")

(defvar sepia-perl-builtins nil
"List of Perl builtins for completion.")

(defun sepia-collect-output (string)
"Collect perl output for `sepia-eval-raw' into sepia-output."
  (setq sepia-output (concat sepia-output string))
  "")

(defun sepia-eval-raw (str)
  "Evaluate perl code STR, returning a pair (RESULT-STRING . OUTPUT)."
  (sepia-ensure-process)
  (let (ocpof)
    (unwind-protect
         (let ((sepia-output "")
               (start 0))
           (with-current-buffer (process-buffer sepia-process)
             (setq ocpof comint-preoutput-filter-functions
                   comint-preoutput-filter-functions
                   '(sepia-collect-output)))
           (setq str (concat "local $Sepia::STOPDIE=0;"
                             "local $Sepia::STOPWARN=0;"
                             "{ package " (sepia-buffer-package) ";"
                             str " }\n"))
           (comint-send-string sepia-process
                               (concat (format "<<%d\n" (length str)) str))
           (while (not (and sepia-output
                            (string-match "> $" sepia-output)))
             (accept-process-output sepia-process))
           (if (string-match "^;;;[0-9]+\n" sepia-output)
               (cons
                (let* ((x (read-from-string sepia-output
                                            (+ (match-beginning 0) 3)))
                       (len (car x))
                       (pos (cdr x)))
                  (prog1 (substring sepia-output (1+ pos) (+ len pos 1))
                    (setq start (+ pos len 1))))
                (and (string-match ";;;[0-9]+\n" sepia-output start)
                     (let* ((x (read-from-string
                                sepia-output
                                (+ (match-beginning 0) 3)))
                            (len (car x))
                            (pos (cdr x)))
                       (substring sepia-output (1+ pos) (+ len pos 1)))))
               (cons sepia-output nil)))
      (with-current-buffer (process-buffer sepia-process)
        (setq comint-preoutput-filter-functions ocpof)))))

(defun sepia-eval (str &optional context detailed)
"Evaluate STR in CONTEXT (void by default), and return its result
as a Lisp object.  If DETAILED is specified, return a
pair (RESULT . OUTPUT)."
  (let* ((tmp (sepia-eval-raw
               (case context
                 (list-context 
                  (concat "Sepia::tolisp([" str "])"))
                 (scalar-context
                  (concat "Sepia::tolisp(scalar(" str "))"))
                 (t (concat str ";1")))))
         (res (car tmp))
         (errs (cdr tmp)))
    (setq res (if context 
                  (if (string= res "") "" (car (read-from-string res)))
                  1))
    (if detailed
        (cons res errs)
        res)))

(defun sepia-call (fn context &rest args)
"Call perl function FN in CONTEXT with arguments ARGS, returning
its result as a Lisp value."
  (sepia-eval (concat fn "(" (mapconcat #'sepia-lisp-to-perl args ", ") ")")
              context))

(defun sepia-watch-for-eval (string)
"Monitor inferior Perl output looking for Lisp evaluation
requests.  The format for these requests is
\"\\n;;;###LENGTH\\nDATA\".  Only one such request can come from
each inferior Perl prompt."
  (setq sepia-passive-output (concat sepia-passive-output string))
  (cond
    ((string-match "^;;;###[0-9]+" sepia-passive-output)
     (if (string-match "^;;;###\\([0-9]+\\)\n\\(?:.\\|\n\\)*\n\\(.*> \\)"
                         sepia-passive-output)
       (let* ((len (car (read-from-string
                         (match-string 1 sepia-passive-output))))
              (pos (1+ (match-end 1)))
              (res (ignore-errors (eval (car (read-from-string
                                              sepia-passive-output pos
                                              (+ pos len)))))))
         (message "%s => %s"
                  (substring sepia-passive-output pos (+ pos len)) res)
         (goto-char (point-max))
         (insert (substring sepia-passive-output (+ 1 pos len)))
         (set-marker (process-mark (get-buffer-process (current-buffer)))
                     (point))
         (setq sepia-passive-output ""))
       ""))
    (t (setq sepia-passive-output "") string)))


(defvar sepia-metapoint-map
  (let ((map (make-sparse-keymap)))
    (when (featurep 'ido)
      (define-key map "j" 'sepia-jump-to-symbol))
    (dolist (kv '(("c" . sepia-callers)
                  ("C" . sepia-callees)
                  ("a" . sepia-apropos)
                  ("A" . sepia-var-apropos)
                  ("v" . sepia-var-uses)
                  ("V" . sepia-var-defs)
                  ;;		  ("V" . sepia-var-assigns)
                  ("\M-." . sepia-dwim)
                  ;; ("\M-." . sepia-location)
                  ("l" . sepia-location)
                  ("f" . sepia-defs)
                  ("r" . sepia-rebuild)
                  ("m" . sepia-module-find)
                  ("n" . sepia-next)
                  ("t" . find-tag)
                  ("d" . sepia-perldoc-this)))
      (define-key map (car kv) (cdr kv)))
    map)
  "Keymap for Sepia functions.  This is just an example of how you
might want to bind your keys, which works best when bound to
`\\M-.'.")

(defvar sepia-shared-map
  (let ((map (make-sparse-keymap)))
    (define-key map sepia-prefix-key sepia-metapoint-map)
    (define-key map "\M-," 'sepia-next)
    (define-key map "\C-\M-x" 'sepia-eval-defun)
    (define-key map "\C-c\C-l" 'sepia-load-file)
    (define-key map "\C-c\C-p" 'sepia-view-pod) ;was cperl-pod-spell
    (define-key map "\C-c\C-d" 'cperl-perldoc)
    (define-key map "\C-c\C-r" 'sepia-repl)
    (define-key map "\C-c\C-s" 'sepia-scratch)
    (define-key map "\C-c\C-e" 'sepia-eval-expression)
    (define-key map "\C-c!" 'sepia-set-cwd)
    (define-key map (kbd "TAB") 'sepia-indent-or-complete)
    map)
  "Sepia bindings common to all modes.")

;;;###autoload
(defun sepia-perldoc-this (name)
  "View perldoc for module at point."
  (interactive (list (sepia-interactive-arg 'module)))
  (let ((wc (current-window-configuration))
        (old-pd (symbol-function 'w3m-about-perldoc))
        (old-pdb (symbol-function 'w3m-about-perldoc-buffer)))
    (condition-case stuff
        (flet ((w3m-about-perldoc (&rest args)
                 (let ((res (apply old-pd args)))
                   (or res (error "lose: %s" args))))
               (w3m-about-perldoc-buffer (&rest args)
                 (let ((res (apply old-pdb args)))
                   (or res (error "lose: %s" args)))))
          (funcall (if (featurep 'w3m) 'w3m-perldoc 'cperl-perldoc) name))
      (error (set-window-configuration wc)))))

(defun sepia-view-pod ()
  "View POD for the current buffer."
  (interactive)
  (funcall sepia-view-pod-function))

(defun sepia-module-list ()
  "List installed modules with links to their documentation.

This lists not just top-level packages appearing in packlist
files, but all documented modules on the system, organized by
package."
  (interactive)
  (let ((file "/tmp/modlist.html"))
    ;; (unless (file-exists-p file)
    (sepia-eval-raw (format "Sepia::html_module_list(\"%s\")" file))
    (funcall sepia-module-list-function file)))

(defun sepia-package-list ()
  "List installed packages with links to their documentation.

This lists only top-level packages appearing in packlist files.
For modules within packages, see `sepia-module-list'."
  (interactive)
  (let ((file "/tmp/packlist.html"))
    ;; (unless (file-exists-p file)
    (sepia-eval-raw (format "Sepia::html_package_list(\"%s\")" file))
    (funcall sepia-module-list-function file)))

(defun sepia-perldoc-buffer ()
  "View current buffer's POD using pod2html and `browse-url'.

Interactive users should call `sepia-view-pod'."
  (let ((buffer (get-buffer-create "*sepia-pod*"))
        (errs (get-buffer-create "*sepia-pod-errors*"))
        (inhibit-read-only t))
    (with-current-buffer buffer (erase-buffer))
    (save-window-excursion
      (shell-command-on-region (point-min) (point-max) "pod2html"
                               buffer nil errs))
    (with-current-buffer buffer (browse-url-of-buffer))))

(defun sepia-perl-name (sym &optional mod)
"Convert a Perl name to a Lisp name."
  (setq sym (substitute ?_ ?- (if (symbolp sym) (symbol-name sym) sym)))
  (if mod
      (concat mod "::" sym)
      sym))

(defun sepia-live-p ()
  (and (processp sepia-process)
       (eq (process-status sepia-process) 'run)))

(defun sepia-ensure-process (&optional remote-host)
  (unless (sepia-live-p)
    (with-current-buffer (get-buffer-create "*sepia-repl*")
      (sepia-repl-mode)
      (set (make-local-variable 'sepia-passive-output) ""))
    (if remote-host
        (comint-exec "*sepia-repl*" "attachtty" "attachtty" nil
                     (list remote-host))
        (let ((stuff (split-string sepia-program-name nil t)))
          (comint-exec (get-buffer-create "*sepia-repl*")
                       "perl" (car stuff) nil
                       (append
                        (cdr stuff)
                        (mapcar (lambda (x) (concat "-I" x)) sepia-perl5lib)
                        '("-MSepia" "-MSepia::Xref"
                          "-e" "Sepia::repl")))))
    (setq sepia-process (get-buffer-process "*sepia-repl*"))
    (accept-process-output sepia-process 1)
    ;; Steal a bit from gud-common-init:
    (setq gud-running t)
    (setq gud-last-last-frame nil)
    (set-process-filter sepia-process 'gud-filter)
    (set-process-sentinel sepia-process 'gud-sentinel)))

;;;###autoload
(defun sepia-repl (&optional remote-host)
  "Start the Sepia REPL."
  (interactive (list (and current-prefix-arg
                          (read-string "Host: "))))
  (sepia-init) ;; set up keymaps, etc.
  (sepia-ensure-process remote-host)
  (pop-to-buffer (get-buffer "*sepia-repl*")))

(defvar sepia-repl-mode-map
  (let ((map (copy-keymap sepia-shared-map)))
    (set-keymap-parent map gud-mode-map)
    (define-key map (kbd "<tab>") 'comint-dynamic-complete)
    (define-key map "\C-a" 'comint-bol)
    map)

"Keymap for Sepia interactive mode.")
    
(define-derived-mode sepia-repl-mode gud-mode "Sepia REPL"
  "Major mode for the Sepia REPL.

\\{sepia-repl-mode-map}"
    (set (make-local-variable 'comint-dynamic-complete-functions)
         '(sepia-complete-symbol comint-dynamic-complete-filename))
    (set (make-local-variable 'comint-preoutput-filter-functions)
         '(sepia-watch-for-eval))
    ;; (set (make-local-variable 'comint-use-prompt-regexp) t)
    (modify-syntax-entry ?: "_")
    (modify-syntax-entry ?> ".")
    (set (make-local-variable 'comint-prompt-regexp) "^[^>\n]*> *")
    (set (make-local-variable 'gud-target-name) "sepia")
    (set (make-local-variable 'gud-marker-filter) 'sepia-gud-marker-filter)
    (set (make-local-variable 'gud-minor-mode) 'sepia)

    (setq gud-comint-buffer (current-buffer))
    (setq gud-last-last-frame nil)
    (setq gud-sepia-acc nil)

    (gud-def gud-break ",break %f:%l" "\C-b" "Set breakpoint at current line.")
    (gud-def gud-step ",step %p" "\C-s" "Step one line.")
    (gud-def gud-next ",next %p" "\C-n" "Step one line, skipping calls.")
    (gud-def gud-cont ",continue" "\C-r" "Continue.")
    (gud-def gud-print "%e" "\C-p" "Evaluate something.")
    (gud-def gud-remove ",delete %l %f" "\C-d" "Delete current breakpoint.")
    (run-hooks 'sepia-repl-mode-hook))

(defvar gud-sepia-acc nil
  "Accumulator for `sepia-gud-marker-filter'.")

(defun sepia-gud-marker-filter (str)
  (setq gud-sepia-acc
        (if gud-sepia-acc
            (concat gud-sepia-acc str)
            str))
  (while (string-match "_<\\([^:>]+\\):\\([0-9]+\\)>\\(.*\\)" gud-sepia-acc)
    (setq gud-last-last-frame gud-last-frame
          gud-last-frame (cons
                          (match-string 1 gud-sepia-acc)
                          (string-to-number (match-string 2 gud-sepia-acc)))
          gud-sepia-acc (match-string 3 gud-sepia-acc)))
  (setq gud-sepia-acc
        (if (string-match "\\(_<.*\\)" gud-sepia-acc)
            (match-string 1 gud-sepia-acc)
            nil))
  str)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Xref

(defun define-xref-function (package name doc)
  "Define a lisp mirror for a low-level Sepia function."
  (let ((lisp-name (intern (format "xref-%s" name)))
	(pl-name (sepia-perl-name name package)))
    (fmakunbound lisp-name)
    (eval `(defun ,lisp-name (&rest args)
             ,doc
             (apply #'sepia-call ,pl-name 'list-context args)))))

(defun define-modinfo-function (name &optional doc context)
"Define a lisp mirror for a function from Module::Info."
  (let ((name (intern (format "sepia-module-%s" name)))
        (pl-func (sepia-perl-name name))
	(full-doc (concat (or doc "") "

This function uses Module::Info, so it does not require that the
module in question be loaded.")))
    (when (fboundp name) (fmakunbound name))
    (eval `(defun ,name (mod)
             ,full-doc
             (interactive (list (sepia-interactive-arg 'module)))
             (sepia-maybe-echo
              (sepia-call "Sepia::module_info" ',(or context 'scalar-context)
                         mod ,pl-func)
              (interactive-p))))))

(defun sepia-thing-at-point (what)
  "Like `thing-at-point', but hacked to avoid REPL prompt."
  (let ((th (thing-at-point what)))
    (and th (not (string-match "[ >]$" th)) th)))

(defvar sepia-sub-re "^ *sub\\s +\\(.+\\_>\\)")

(defvar sepia-history nil)

(defun sepia-interactive-arg (&optional sepia-arg-type)
"Default argument for most Sepia functions.  TYPE is a symbol --
either 'file to look for a file, or anything else to use the
symbol at point."
  (let* ((default (case sepia-arg-type
		    (file (or (thing-at-point 'file) (buffer-file-name)))
                    (t (sepia-thing-at-point 'symbol))))
         (text (capitalize (symbol-name sepia-arg-type)))
         (choices
          (lambda (str &rest blah)
            (let ((completions (xref-completions
                                str
                                (case sepia-arg-type
                                  (module nil)
                                  (variable "VARIABLE")
                                  (function "CODE")
                                  (t nil)))))
              (when (eq sepia-arg-type 'module)
                (setq completions
                      (remove-if (lambda (x) (string-match "::$" x)) completions)))
              completions)))
         (prompt (if default
                     (format "%s [%s]: " text default)
                     (format "%s: " text)))
	 (ret (if sepia-use-completion
                  (completing-read prompt 'blah-choices nil nil nil 'sepia-history
                                   default)
		  (read-string prompt nil 'sepia-history default))))
    (push ret sepia-history)
    ret))

(defun sepia-interactive-module ()
"Guess which module we should look things up in.  Prompting for a
module all the time is a PITA, but I don't think this (choosing
the current file's module) is a good alternative, either.  Best
would be to choose the module based on what we know about the
symbol at point."
  (let ((xs (xref-file-modules (buffer-file-name))))
    (if (= (length xs) 1)
        (car xs)
        nil)))

(defun sepia-maybe-echo (result &optional print-message)
  (when print-message
    (message "%s" result))
  result)

(defun sepia-find-module-file (mod)
  (or (sepia-module-file mod)
      (car (xref-guess-module-file mod))))

(defun sepia-module-find (mod)
"Find the file defining module MOD."
  (interactive (list (sepia-interactive-arg 'module)))
  (let ((fn (sepia-find-module-file mod)))
    (if fn
        (progn
          (message "Module %s in %s." mod fn)
          (pop-to-buffer (find-file-noselect (expand-file-name fn))))
        (message "Can't find module %s." mod))))

(defmacro ifa (test then &rest else)
  `(let ((it ,test))
     (if it ,then ,@else)))

(defvar sepia-found-refiner)

(defun sepia-show-locations (locs)
  (when locs
    (pop-to-buffer (get-buffer-create "*sepia-places*"))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (dolist (loc (sort (remove nil locs) ; XXX where's nil from?
                         (lambda (a b)
                           (or (string< (car a) (car b))
                               (and (string= (car a) (car b))
                                    (< (second a) (second b)))))))
        (destructuring-bind (file line name &rest blah) loc
          (let ((str (ifa (find-buffer-visiting file)
                          (with-current-buffer it
                            (ifa sepia-found-refiner
                                 (funcall it line name)
                                 (goto-line line))
                            (message "line for %s was %d, now %d" name line
                                     (line-number-at-pos))
                            (setq line (line-number-at-pos))
                            (let ((tmpstr
                                   (buffer-substring (sepia-bol-from (point))
                                                     (sepia-eol-from (point)))))
                              (if (> (length tmpstr) 60)
                                  (concat "\n    " tmpstr)
                                  tmpstr)))
                          "...")))
            (insert (format "%s:%d:%s\n" (abbreviate-file-name file) line str)))))
      (grep-mode)
      (goto-char (point-min)))))

(defmacro define-sepia-query (name doc &optional gen test prompt)
  "Define a sepia querying function."
  `(defun ,name (ident &optional module file line display-p)
     ,(concat doc "

With prefix arg, list occurences in a `grep-mode' buffer.
Without, place the occurrences on `sepia-found', so that
calling `sepia-next' will cycle through them.

Depending on the query, MODULE, FILE, and LINE may be used to
narrow the results, as long as doing so leaves some matches.
When called interactively, they are taken from the current
buffer.
")
     (interactive (list (sepia-interactive-arg ,(or prompt ''function))
			(sepia-interactive-module)
			(buffer-file-name)
			(line-number-at-pos (point))
			current-prefix-arg
			))
     (let ((ret
	    ,(if test
		 `(let ((tmp (,gen ident module file line)))
		    (or (mapcan #',test tmp) tmp))
                 `(,gen ident module file line))))
       ;; Always clear out the last found ring, because it's confusing
       ;; otherwise.
       (sepia-set-found nil ,(or prompt ''function))
       (if display-p
           (sepia-show-locations ret)
           (sepia-set-found ret ,(or prompt ''function))
           (sepia-next)))))

(define-sepia-query sepia-defs
    "Find all definitions of sub."
  xref-apropos
  xref-location)

(define-sepia-query sepia-callers
    "Find callers of FUNC."
  xref-callers
  xref-location)

(define-sepia-query sepia-callees
    "Find a sub's callees."
  xref-callees
  xref-location)

(define-sepia-query sepia-var-defs
    "Find a var's definitions."
  xref-var-defs
  (lambda (x) (setf (third x) ident) (list x))
  'variable)

(define-sepia-query sepia-var-uses
    "Find a var's uses."
  xref-var-uses
  (lambda (x) (setf (third x) ident) (list x))
  'variable)

(define-sepia-query sepia-var-assigns
    "Find/list assignments to a variable."
  xref-var-assigns
  (lambda (x) (setf (third x) ident) (list x))
  'variable)

(defalias 'sepia-package-defs 'sepia-module-describe)

(define-sepia-query sepia-apropos
    "Find/list subroutines matching regexp."
  (lambda (name &rest blah) (xref-apropos name 1))
  xref-location
  'function)

(define-sepia-query sepia-var-apropos
    "Find/list variables matching regexp."
  xref-var-apropos
  xref-var-defs
  'variable)

(defun sepia-location (name &optional jump-to)
  "Find the definition of NAME.

When called interactively (or with JUMP-TO true), go directly
to this location."
  (interactive (list (sepia-interactive-arg 'function) t))
  (let* ((fl (or (car (xref-location name))
                 (car (remove-if #'null
                                 (apply #'xref-location (xref-apropos name)))))))
    (when (and (car fl) (string-match "^(eval " (car fl)))
      (message "Can't find definition of %s in %s." name (car fl))
      (setq fl nil))
    (if jump-to
        (if fl (progn
                 (sepia-set-found (list fl) 'function)
                 (sepia-next))
            (message "No definition for %s." name))
        fl)))

;;;###autoload
(defun sepia-dwim (&optional display-p)
    "Try to do the right thing with identifier at point.
* Find all definitions, if thing-at-point is a function
* Find all uses, if thing-at-point is a variable
* Find documentation, if thing-at-point is a module
* Prompt otherwise
"
    (interactive "P")
    (multiple-value-bind (type obj) (sepia-ident-at-point)
      (sepia-set-found nil type)
      (let* ((module-doc-p nil)
             (ret
              (cond
                ((member type '(?% ?$ ?@)) (xref-var-defs obj))
                ((or (equal type ?&)
                     (let (case-fold-search)
                       (string-match "^[^A-Z]" obj)))
                 (list (sepia-location obj)))
                ((sepia-looks-like-module obj)
                 (setq module-doc-p t)
                 `((,(sepia-perldoc-this obj) 1 nil nil)))
                (t (setq module-doc-p t)
                   (call-interactively 'sepia-defs)))))
        (unless module-doc-p
          (if display-p
              (sepia-show-locations ret)
              (sepia-set-found ret type)
              (sepia-next))))))

(defun sepia-rebuild ()
  "Rebuild the Xref database."
  (interactive)
  (xref-rebuild))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Perl motion commands.

;;; XXX -- these are a hack to prevent infinite recursion calling
;;; e.g. beginning-of-defun from beginning-of-defun-function.
;;; `beginning-of-defun' should handle this.
(defmacro sepia-safe-bodf (&optional n)
  `(let ((beginning-of-defun-function
          (if (and (boundp 'beginning-of-defun-function)
                   (eq beginning-of-defun-function 'sepia-beginning-of-defun))
              nil
              beginning-of-defun-function)))
     (beginning-of-defun ,n)))

(defmacro sepia-safe-eodf (&optional n)
  `(let ((end-of-defun-function
          (if (and (boundp 'end-of-defun-function)
                   (eq end-of-defun-function 'sepia-end-of-defun))
              nil
              end-of-defun-function)))
     (end-of-defun ,n)))

(defun sepia-beginning-of-defun (&optional n)
"Move to beginning of current function.

The prefix argument is the same as for `beginning-of-defun'."
  (interactive "p")
  (setq n (or n 1))
  (ignore-errors
    (when (< n 0)
      (sepia-end-of-defun (- n))
      (setq n 1))
    (re-search-backward sepia-sub-re nil nil n)))

(defun sepia-inside-defun ()
  "True if point is inside a sub."
  (condition-case nil
      (save-excursion
        (let ((cur (point)))
          (re-search-backward sepia-sub-re)
          (when (< (point) cur)
            (search-forward "{")
            (backward-char 1)
            (forward-sexp)
            (> (point) cur))))
    (error nil)))

(defun sepia-end-of-defun (&optional n)
  "Move to end of current function.

The prefix argument is the same as for `end-of-defun'."
  (interactive "p")
  (setq n (or n 1))
  (when (< n 0)
    (sepia-beginning-of-defun (- n))
    (setq n 1))
  ;; If we're outside a defun, skip to the next
  (ignore-errors
    (unless (sepia-inside-defun)
      (re-search-forward sepia-sub-re)
      (forward-char 1))
    (dotimes (i n)
      (re-search-backward sepia-sub-re)
      (search-forward "{")
      (backward-char 1)
      (forward-sexp))
    (point)))

(defun sepia-defun-around-point (&optional where)
  "Return the text of function around point."
  (unless where
    (setq where (point)))
  (save-excursion
    (goto-char where)
    (and (sepia-beginning-of-defun)
         (match-string-no-properties 1))))

(defun sepia-lexicals-at-point (&optional where)
  "Find lexicals in scope at point."
  (interactive "d")
  (unless where
    (setq where (point)))
  (let ((subname (sepia-defun-around-point where))
        (mod (sepia-buffer-package)))
    (xref-lexicals (sepia-perl-name subname mod))))

;;;###autoload
(defun sepia-load-file (file &optional rebuild-p collect-warnings)
  "Reload a file (interactively, the current buffer's file).

With REBUILD-P (or a prefix argument when called interactively),
also rebuild the xref database."
  (interactive (list (expand-file-name (buffer-file-name))
                     prefix-arg
                     (format "*%s errors*" (buffer-file-name))))
  (save-buffer)
  (when collect-warnings
    (let (kill-buffer-query-functions)
      (ignore-errors
        (kill-buffer collect-warnings))))
  (let* ((tmp (sepia-eval (format "do '%s' || ($@ && do { local $Sepia::Debug::STOPDIE; die $@ })" file)
                          'scalar-context t))
         (res (car tmp))
         (errs (cdr tmp)))
    (message "sepia: %s returned %s" (abbreviate-file-name file)
             (if (equal res "") "undef" res))
    (when (and collect-warnings
               (> (length errs) 1))
      (with-current-buffer (get-buffer-create collect-warnings)
        (let ((inhibit-read-only t))
          (delete-region (point-min) (point-max))
          (insert errs)
          (sepia-display-errors (point-min) (point-max))
          (pop-to-buffer (current-buffer))))))
  (when rebuild-p
    (xref-rebuild)))

(defvar sepia-found)

(defun sepia-set-found (list &optional type)
  (setq list
	(remove-if (lambda (x)
                     (or (not x)
                         (and (not (car x)) (string= (fourth x) "main"))))
                   list))
  (setq sepia-found (cons -1 list))
  (setq sepia-found-refiner (sepia-refiner type)))

(defun sepia-refiner (type)
  (case type
    (function
     (lambda (line ident)
      (let ((sub-re (concat "^\\s *sub\\s +.*" ident "\\_>")))
        ;; Test this because sometimes we get lucky and get the line
        ;; just right, in which case beginning-of-defun goes to the
        ;; previous defun.
          (or (and line
                   (progn
                     (goto-line line)
		     (beginning-of-defun)
                     (looking-at sub-re)))
              (progn (goto-char (point-min))
                     (re-search-forward sub-re nil t)))
          (beginning-of-line))))
    ;; Old version -- this may actually work better if
    ;; beginning-of-defun goes flaky on us.
;;         (or (re-search-backward sub-re
;; 				   (sepia-bol-from (point) -20) t)
;; 	       (re-search-forward sub-re
;; 				  (sepia-bol-from (point) 10) t))
;; 	   (beginning-of-line)
    (variable
     (lambda (line ident)
       (let ((var-re (concat "\\_<" ident "\\_>")))
	 (cond
	   (line (goto-line line)
		 (or (re-search-backward var-re (sepia-bol-from (point) -5) t)
		     (re-search-forward var-re (sepia-bol-from (point) 5) t)))
	   (t (goto-char (point-min))
              (re-search-forward var-re nil t))))))
    (t (lambda (line ident) (and line (goto-line line))))))

(defun sepia-next (&optional arg)
  "Go to the next thing (e.g. def, use) found by sepia."
  (interactive "p")
  (or arg (setq arg 1))
  (if (cdr sepia-found)
      (let ((i (car sepia-found))
            (list (cdr sepia-found))
            (len (length (cdr sepia-found)))
            (next (+ (car sepia-found) arg))
            (prompt ""))
        (if (and (= len 1) (>= i 0))
            (message "No more definitions.")
          ;; if stepwise found next or previous item, it can cycle
          ;; around the `sepia-found'. When at first or last item, get
          ;; a warning
          (if (= (abs arg) 1)
              (progn
                (setq i next)
                (if (< i 0)
                    (setq i (1- len))
                  (if (>= i len)
                      (setq i 0)))
                (if (= i (1- len))
                    (setq prompt "Last one! ")
                  (if (= i 0)
                      (setq prompt "First one! "))))
            ;; if we skip several item, when arrive the first or last
            ;; item, we will stop at the one. But if we already at last
            ;; item, then keep going
            (if (< next 0)
                (if (= i 0)
                    (setq i (mod next len))
                  (setq i 0
                        prompt "First one!"))
              (if (> next len)
                  (if (= i (1- len))
                      (setq i (mod next len))
                    (setq i (1- len)
                          prompt "Last one!")))))
          (setcar sepia-found i)
          (setq next (nth i list))
          (let ((file (car next))
                (line (cadr next))
                (short (nth 2 next))
                (mod (nth 3 next)))
        (unless file
          (setq file (and mod (sepia-find-module-file mod)))
          (if file
                  (setcar next file)
                (error "No file for %s." (car next))))
            (message "%s at %s:%s. %s" short file line prompt)
        (when (file-exists-p file)
          (find-file (or file (sepia-find-module-file mod)))
          (when sepia-found-refiner
            (funcall sepia-found-refiner line short))
          (beginning-of-line)
              (recenter)))))
      (message "No more definitions.")))

(defun sepia-previous (&optional arg)
  (interactive "p")
  (or arg (setq arg 1))
  (sepia-next (- arg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completion

(defun sepia-ident-before-point ()
  "Find the Perl identifier at or preceding point."
  (save-excursion
    (let* ((end (point))
           (beg (progn
                  (skip-chars-backward "a-zA-Z0-9_:")
                  (point)))
           (sigil (if (= beg (point-min))
                      nil
                      (char-before (point)))))
      (list (when (member sigil '(?$ ?@ ?% ?* ?&)) sigil)
            (buffer-substring-no-properties beg end)))))

(defun sepia-simple-method-before-point ()
  "Find the \"simple\" method call before point.

Looks for a simple method called on a variable before point and
returns the list (OBJECT METHOD).  For example, \"$x->blah\"
returns '(\"$x\" \"blah\").  Only simple methods are recognized,
because completing anything evaluates it, so completing complex
expressions would lead to disaster."
  (when sepia-complete-methods
    (let ((end (point))
          (bound (max (- (point) 100) (point-min)))
          arrow beg)
      (save-excursion
        ;; XXX - can't do this because COMINT's syntax table is weird.
        ;; (skip-syntax-backward "_w")
        (skip-chars-backward "a-zA-Z0-9_")
        (when (looking-back "->\\s *" bound)
          (setq arrow (search-backward "->" bound))
          (skip-chars-backward "a-zA-Z0-9_:")
          (cond
            ;; $x->method
            ((char-equal (char-before (point)) ?$)
             (setq beg (1- (point))))
            ;; X::Class->method
            ((multiple-value-bind (type obj) (sepia-ident-at-point)
               (and (not type)
                    (sepia-looks-like-module obj)))
             (setq beg (point))))
          (when beg
            (list (buffer-substring-no-properties beg arrow)
                  (buffer-substring-no-properties (+ 2 arrow) end)
                  (buffer-substring-no-properties beg end))))))))

(defun sepia-ident-at-point ()
  "Find the Perl identifier at point."
  (save-excursion
    (when (looking-at "[%$@*&]")
      (forward-char 1))
    (let* ((beg (progn
                 (when (re-search-backward "[^A-Za-z_0-9:]" nil 'mu)
                   (forward-char 1))
                 (point)))
          (sigil (if (= beg (point-min))
                     nil
                     (char-before (point))))
          (end (progn
                 (when (re-search-forward "[^A-Za-z_0-9:]" nil 'mu)
                   (forward-char -1))
                 (point))))
      (list (when (member sigil '(?$ ?@ ?% ?* ?&)) sigil)
            (buffer-substring-no-properties beg end)))))

(defun sepia-function-at-point ()
  "Find the Perl function called at point."
  (condition-case nil
      (save-excursion
        (let ((pt (point))
              bof)
          (sepia-beginning-of-defun)
          (setq bof (point))
          (goto-char pt)
          (sepia-end-of-defun)
          (when (and (>= pt bof) (< pt (point)))
            (goto-char bof)
            (looking-at "\\s *sub\\s +")
            (forward-char (length (match-string 0)))
            (concat (or (sepia-buffer-package) "")
                    "::"
                    (cadr (sepia-ident-at-point))))))
    (error nil)))

(defun sepia-repl-complete ()
  "Try to complete the word at point in the REPL.
Just like `sepia-complete-symbol', except that it also completes
REPL shortcuts."
  (interactive)
  (error "TODO"))

(defvar sepia-shortcuts
  '("break" "cd" "debug" "define" "delete" "eval" "format" "help" "lsbreak"
    "methods" "package" "pwd" "quit" "reload" "shell" "size" "strict" "undef"
    "wantarray")
  "List of currently-defined REPL shortcuts.

XXX: this needs to be updated whenever you add one on the Perl side.")

(defun sepia-complete-symbol ()
  "Try to complete the word at point.
The word may be either a global variable if it has a
sigil (sorry, no lexicals), a module, or a function.  The
function currently ignores module qualifiers, which may be
annoying in larger programs.

The function is intended to be bound to \\M-TAB, like
`lisp-complete-symbol'."
  (interactive)
  (let ((win (get-buffer-window "*Completions*" 0))
        len
        completions
        type
        meth)
    (if (and (eq last-command this-command)
             win (window-live-p win) (window-buffer win)
             (buffer-name (window-buffer win)))

        ;; If this command was repeated, and
        ;; there's a fresh completion window with a live buffer,
        ;; and this command is repeated, scroll that window.
        (with-current-buffer (window-buffer win)
          (if (pos-visible-in-window-p (point-max) win)
              (set-window-start win (point-min))
            (save-selected-window
              (select-window win)
              (scroll-up))))

      ;; Otherwise actually do completion:
      ;; 0 - try a shortcut
      (when (eq major-mode 'sepia-repl-mode)
      (save-excursion
        (comint-bol)
          (when (looking-at ",\\([a-z]+\\)$")
          (let ((str (match-string 1)))
            (setq len (length str)
                    completions (all-completions str sepia-shortcuts))))))
      ;; 1 - Look for a method call:
      (unless completions
        (setq meth (sepia-simple-method-before-point))
        (when meth
          (setq len (length (caddr meth))
                completions (xref-method-completions
                             (cons 'expr (format "'%s'" (car meth)))
                             (cadr meth)
                             "Sepia::repl_eval")
                type (format "%s->" (car meth)))))
      (multiple-value-bind (typ name) (sepia-ident-before-point)
        (unless completions
          ;; 2 - look for a regular function/variable/whatever
          (setq type typ
                len (+ (if type 1 0) (length name))
                completions (xref-completions
                             name
                             (case type
                               (?$ "VARIABLE")
                               (?@ "ARRAY")
                               (?% "HASH")
                               (?& "CODE")
                               (?* "IO")
                               (t ""))
                             (and (eq major-mode 'sepia-mode)
                                  (sepia-function-at-point)))))
        ;; 3 - try a Perl built-in
        (when (and (not completions)
                   (or (not type) (eq type ?&)))
          (when (string-match ".*::([^:]+)$" name)
            (setq name (match-string 1 name)))
          (setq completions (all-completions name sepia-perl-builtins)))
        (case (length completions)
          (0 (message "No completions.") nil)
          (1 ;; XXX - skip sigil to match s-i-before-point
           (delete-region (- (point) len) (point))
           (insert (or type "") (car completions))
           ;; Hide stale completions buffer (stolen from lisp.el).
           (if win (with-selected-window win (bury-buffer))) t)
          (t (let ((old name)
                   (new (try-completion "" completions)))
               (if (<= (length new) (length old))
                   (with-output-to-temp-buffer "*Completions*"
                     (display-completion-list completions))
                 (let ((win (get-buffer-window "*Completions*" 0)))
                   (if win (with-selected-window win (bury-buffer))))
                 (delete-region (- (point) len) (point))
                 (insert (or type "") new))))))
      t)))

(defun sepia-indent-or-complete ()
"Indent the current line or complete the symbol around point.

Specifically, try completion when indentation doesn't move point.
This function is intended to be bound to TAB."
  (interactive)
  (let ((pos (point)))
    (let (beginning-of-defun-function
          end-of-defun-function)
      (cperl-indent-command))
    (when (and (= pos (point))
               (not (bolp))
               (or (eq last-command 'sepia-indent-or-complete)
                   (looking-at "\\_>")))
      (unless (and sepia-indent-expand-abbrev
                   (expand-abbrev))
        (sepia-complete-symbol)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; scratchpad code

(defvar sepia-mode-map
  (let ((map (copy-keymap sepia-shared-map)))
    (set-keymap-parent map cperl-mode-map)
    (define-key map "\C-c\C-h" nil)
    map)
 "Keymap for Sepia mode.")

(defvar sepia-mode-abbrev-table nil
"Abbrevs for Sepia mode.")

;;;###autoload
(define-derived-mode sepia-mode cperl-mode "Sepia"
  "Major mode for Perl editing, derived from cperl mode.
\\{sepia-mode-map}"
  :abbrev-table nil
  (sepia-init)
  (sepia-install-eldoc)
  (sepia-doc-update)
  (set (make-local-variable 'beginning-of-defun-function)
       'sepia-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function)
       'sepia-end-of-defun))

(defun sepia-init ()
  "Perform the initialization necessary to start Sepia."
    ;; Load perl defs:
  ;; Create glue wrappers for Module::Info funcs.
  (unless (fboundp 'xref-completions)
    (dolist (x '((name "Find module name.\n\nDoes not require loading.")
                 (version "Find module version.\n\nDoes not require loading.")
                 (inc-dir "Find directory in which this module was found.\n\nDoes not require loading.")
                 (file "Absolute path of file defining this module.\n\nDoes not require loading.")
                 (is-core "Guess whether or not a module is part of the core distribution.
Does not require loading.")
                 (modules-used "List modules used by this module.\n\nRequires loading." list-context)
                 (packages-inside "List sub-packages in this module.\n\nRequires loading." list-context)
                 (superclasses "List module's superclasses.\n\nRequires loading." list-context)))
      (apply #'define-modinfo-function x))
    ;; Create low-level wrappers for Sepia
    (dolist (x '((completions "Find completions in the symbol table.")
                 (method-completions "Complete on an object's methods.")
                 (location "Find an identifier's location.")
                 (mod-subs "Find all subs defined in a package.")
                 (mod-decls "Generate declarations for subs in a package.")
                 (mod-file "Find the file defining a package.")
                 (apropos "Find subnames matching RE.")
                 (lexicals "Find lexicals for a sub.")
                 ))
      (apply #'define-xref-function "Sepia" x))

    (dolist (x '((rebuild "Build Xref database for current Perl process.")
                 (redefined "Rebuild Xref information for a given sub.")

                 (callers "Find all callers of a function.")
                 (callees "Find all functions called by a function.")

                 (var-apropos "Find varnames matching RE.")
                 (mod-apropos "Find modules matching RE.")
                 (file-apropos "Find files matching RE.")

                 (var-defs "Find all definitions of a variable.")
                 (var-assigns "Find all assignments to a variable.")
                 (var-uses "Find all uses of a variable.")

                 (mod-redefined "Rebuild Xref information for a given package.")
                 (guess-module-file "Guess file corresponding to module.")
                 (file-modules "List the modules defined in a file.")))
      (apply #'define-xref-function "Sepia::Xref" x))
    ;; Initialize built hash
    (sepia-init-perl-builtins)))

(defvar sepia-scratchpad-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map sepia-mode-map)
    (define-key map "\C-j" 'sepia-scratch-send-line)
    map))

;;;###autoload
(define-derived-mode sepia-scratchpad-mode sepia-mode "Sepia-Scratch"
  "Major mode for the Perl scratchpad, derived from Sepia mode."
  (sepia-init))

;;;###autoload
(defun sepia-scratch ()
  "Switch to the sepia scratchpad."
  (interactive)
  (pop-to-buffer
   (or (get-buffer "*sepia-scratch*")
       (with-current-buffer (get-buffer-create "*sepia-scratch*")
         (sepia-scratchpad-mode)
         (current-buffer)))))

(defun sepia-scratch-send-line (&optional scalarp)
  "Send the current line to perl, and display the result."
  (interactive "P")
  (insert "\n"
   (format "%S" (sepia-eval-raw (concat "scalar do{"
		       (buffer-substring (sepia-bol-from (point))
					 (sepia-eol-from (point)))
		       "}")))
   "\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellany

(defun sepia-string-count-matches (reg str)
  (let ((n 0)
        (pos -1))
    (while (setq pos (string-match reg str (1+ pos)))
      (incf n))
    n))

(defun sepia-perlize-region-internal (pre post beg end replace-p)
  "Pass buffer text from BEG to END through a Perl command."
  (let* ((exp (concat pre "<<'SEPIA_END_REGION';\n"
                      (buffer-substring-no-properties beg end)
                      (if (= (char-before end) ?\n) "" "\n")
		      "SEPIA_END_REGION\n" post))
	 (new-str (car (sepia-eval-raw exp))))
    (if replace-p
	(progn (delete-region beg end)
	       (goto-char beg)
	       (insert new-str))
        (if (> (sepia-string-count-matches "\n" new-str) 2)
            (with-current-buffer (get-buffer-create "*sepia-filter*")
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert new-str)
                (goto-char (point-min))
                (pop-to-buffer (current-buffer))))
            (message "%s" new-str)))))

(defun sepia-eol-from (pt &optional n)
  (save-excursion
    (goto-char pt)
    (end-of-line n)
    (point)))

(defun sepia-bol-from (pt &optional n)
  (save-excursion
    (goto-char pt)
    (beginning-of-line n)
    (point)))

(defun sepia-perl-pe-region (expr beg end &optional replace-p)
  "Do the equivalent of perl -pe on region

\(i.e. evaluate an expression on each line of region).  With
prefix arg, replace the region with the result."
  (interactive "MExpression: \nr\nP")
  (sepia-perlize-region-internal
   "do { my $ret=''; local $_; local $/ = \"\\n\"; my $region = "
   (concat "; for (split /(?<=\\n)/, $region, -1) { " expr
	   "} continue { $ret.=$_}; $ret}")
   (sepia-bol-from beg) (sepia-eol-from end) replace-p))

(defun sepia-perl-ne-region (expr beg end &optional replace-p)
  "Do the moral equivalent of perl -ne on region

\(i.e. evaluate an expression on each line of region).  With
prefix arg, replace the region with the result."
  (interactive "MExpression:\nr\nP")
  (sepia-perlize-region-internal
   "do { my $ret='';my $region = "
   (concat "; for (split /(?<=\\n)/, $region, -1) { $ret .= do { " expr
	   ";} }; ''.$ret}")
   (sepia-bol-from beg) (sepia-eol-from end) replace-p))
  
(defun sepia-perlize-region (expr beg end &optional replace-p)
  "Evaluate a Perl expression on the region as a whole.

With prefix arg, replace the region with the result."
  (interactive "MExpression:\nr\nP")
  (sepia-perlize-region-internal
   "do { local $_ = " (concat "; do { " expr ";}; $_ }") beg end replace-p))

(defun sepia-core-version (module &optional message)
  "Report the first version of Perl shipping with MODULE."
  (interactive (list (sepia-interactive-arg 'module) t))
  (let* ((version
          (sepia-eval
           (format "eval { Sepia::core_version('%s') }" module)
           'scalar-context))
         (res (if version
                  (format "%s was first released in %s." module version)
                  (format "%s is not in core." module))))
    (when message (message "%s" res))
    res))

(defun sepia-guess-package (sub &optional file)
  "Guess which package SUB is defined in."
  (let ((defs (xref-location (xref-apropos sub))))
    (or (and (= (length defs) 1)
	     (or (not file) (equal (caar defs) file))
	     (fourth (car defs)))
	(and file
	     (fourth (find-if (lambda (x) (equal (car x) file)) defs)))
	;; (car (xref-file-modules file))
	(sepia-buffer-package))))

;;;###autoload
(defun sepia-eval-defun ()
  "Re-evaluate the current function and rebuild its Xrefs."
  (interactive)
  (let (pt end beg sub res
           sepia-eval-package
           sepia-eval-file
           sepia-eval-line)
    (save-excursion
      (setq pt (point)
            end (progn (end-of-defun) (point))
            beg (progn (beginning-of-defun) (point)))
      (goto-char beg)
      (when (looking-at "^sub\\s +\\(.+\\_>\\)")
        (setq sub (match-string 1))
        (let ((body (buffer-substring-no-properties beg end)))
          
          (setq sepia-eval-package (sepia-guess-package sub (buffer-file-name))
                sepia-eval-file (buffer-file-name)
                sepia-eval-line (line-number-at-pos beg)
                res
                (sepia-eval-raw
                 (if sepia-eval-defun-include-decls
                     (concat
                      (apply #'concat (xref-mod-decls sepia-eval-package))
                      body)
                     body))))))
    (if (cdr res)
        (progn
          (when (string-match " line \\([0-9]+\\), near \"\\([^\"]*\\)\""
                              (cdr res))
            (goto-char beg)
            (beginning-of-line (string-to-number (match-string 1 (cdr res))))
            (search-forward (match-string 2 (cdr res))
                            (sepia-eol-from (point)) t))
          (message "Error: %s" (cdr res)))
        (xref-redefined sub sepia-eval-package)
        (message "Defined %s" sub))))

;;;###autoload
(defun sepia-eval-expression (expr &optional list-p message-p)
  "Evaluate EXPR in scalar context."
  (interactive (list (read-string "Expression: ") current-prefix-arg t))
  (let ((res (sepia-eval expr (if list-p 'list-context 'scalar-context))))
    (when message-p (message "%s" res))
    res))

(defun sepia-extract-def (file line obj)
  (with-current-buffer (find-file-noselect (expand-file-name file))
    (save-excursion
      (funcall (sepia-refiner 'function) line obj)
      (beginning-of-line)
      (when (looking-at (concat "^\\s *sub\\_>.*\\_<" obj "\\_>"))
	(buffer-substring (point)
			  (progn (end-of-defun) (point)))))))

(defun sepia-eval-no-run (string)
  (let ((res (sepia-eval-raw
              (concat "eval q#{ BEGIN { use B; B::minus_c(); $^C=1; } do { "
                      string
                      " };BEGIN { die \"ok\\n\" }#, $@"))))
    (if (string-match "^ok\n" (car res))
        nil
        (car res))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REPL

(defvar sepia-eval-file nil
  "File in which `sepia-eval' evaluates perl expressions.")
(defvar sepia-eval-line nil
  "Line at which `sepia-eval' evaluates perl expressions.")

(defun sepia-set-cwd (dir)
  "Set the inferior Perl process's working directory to DIR.

When called interactively, the current buffer's
`default-directory' is used."
  (interactive (list (expand-file-name default-directory)))
  (sepia-call "Cwd::chdir" 'list-context dir))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Doc-scanning

(defvar sepia-doc-map (make-hash-table :test #'equal))
(defvar sepia-var-doc-map (make-hash-table :test #'equal))
(defvar sepia-module-doc-map (make-hash-table :test #'equal))

(defun sepia-doc-scan-buffer ()
  (save-excursion
    (goto-char (point-min))
    (loop
       while (re-search-forward
		 "^=\\(item\\|head[2-9]\\)\\s +\\([%$&@A-Za-z_].*\\)" nil t)
       if
         (ignore-errors
           (let ((short (match-string 2)) longdoc)
              (setq short
                    (let ((case-fold-search nil))
                      (replace-regexp-in-string
                       "E<lt>" "<"
                       (replace-regexp-in-string
                        "E<gt>" ">"
                        (replace-regexp-in-string
                         "[A-DF-Z]<\\([^<>]+\\)>" "\\1" short)))))
              (while (string-match "^\\s *[A-Z]<\\(.*\\)>\\s *$" short)
                (setq short (match-string 1 short)))
              (setq longdoc
                    (let ((beg (progn (forward-line 2) (point)))
                          (end (1- (re-search-forward "^=" nil t))))
                      (forward-line -1)
                      (goto-char beg)
                      (if (re-search-forward "^\\(.+\\)$" end t)
                          (concat short ": "
                                  (substring-no-properties
                                   (match-string 1)
                                   0 (position ?. (match-string 1))))
                          short)))
              (cond
                ;; e.g. "$x -- this is x"
                ((string-match "^[%$@]\\([A-Za-z0-9_:]+\\)\\s *--\\s *\\(.*\\)"
                               short)
                 (list 'variable (match-string-no-properties 1 short)
                       (or (and (equal short (match-string 1 short)) longdoc)
                           short)))
                ;; e.g. "C<foo(BLAH)>" or "$x = $y->foo()"
                ((string-match "\\([A-Za-z0-9_:]+\\)\\s *\\(\\$\\|(\\)" short)
                 (list 'function (match-string-no-properties 1 short)
                       (or (and (equal short (match-string 1 short)) longdoc)
                           short)))
                ;; e.g. "C<$result = foo $args...>"
                ((string-match "=\\s *\\([A-Za-z0-9_:]+\\)" short)
                 (list 'function (match-string-no-properties 1 short)
                       (or (and (equal short (match-string 1 short)) longdoc)
                           short)))
                ;; e.g. "$x this is x" (note: this has to come last)
                ((string-match "^[%$@]\\([^( ]+\\)" short)
                 (list 'variable (match-string-no-properties 1 short) longdoc)))))
       collect it)))

(defun sepia-buffer-package ()
  (save-excursion
    (or (and (re-search-backward "^\\s *package\\s +\\([^ ;]+\\)\\s *;" nil t)
	     (match-string-no-properties 1))
	"main")))

(defun sepia-doc-update ()
  "Update documentation for a file.

This documentation, taken from \"=item\" entries in the POD, is
used for eldoc feedback."
  (interactive)
  (let ((pack (ifa (sepia-buffer-package) (concat it "::") "")))
    (dolist (x (sepia-doc-scan-buffer))
      (let ((map (ecase (car x)
		   (function sepia-doc-map)
		   (variable sepia-var-doc-map))))
	(puthash (second x) (third x) map)
	(puthash (concat pack (second x)) (third x) map)))))

(defun sepia-looks-like-module (obj)
  (let (case-fold-search)
    (or (string-match "^\\([A-Z][A-Za-z0-9]+::\\)*[A-Z]+[A-Za-z0-9]+\\sw*$" obj)
        (string-match
         (eval-when-compile (regexp-opt '("strict" "vars" "warnings" "lib")))
         obj))))

(defun sepia-symbol-info (&optional obj type)
  "Eldoc function for Sepia-mode.

Looks in `sepia-doc-map' and `sepia-var-doc-map', then tries
calling `cperl-describe-perl-symbol'."
  (unless obj
    (multiple-value-bind (ty ob) (sepia-ident-at-point)
      (setq obj (if (consp ob) (car ob) ob)
            type ty)))
  (if obj
      (or (gethash obj (ecase (or type ?&)
                         (?& sepia-doc-map)
                         ((?$ ?@ ?%) sepia-var-doc-map)
                         (nil sepia-module-doc-map)
                         (?* sepia-module-doc-map)
                         (t (error "sepia-symbol-info: %s" type))))
          ;; Loathe cperl a bit.
          (flet ((message (&rest blah) (apply #'format blah)))
            (let* (case-fold-search
                   (cperl-message-on-help-error nil)
                   (hlp (car (save-excursion (cperl-describe-perl-symbol obj)))))
              (if hlp
                  (progn
                    ;; cperl's docstrings are too long.
                    (setq hlp (replace-regexp-in-string "\\s \\{2,\\}" "  " hlp))
                    (if (> (length hlp) 75)
                        (concat (substring hlp 0 72) "...")
                        hlp))
                  ;; Try to see if it's a module
                  (if (and
                       (let ((bol (save-excursion (beginning-of-line)
                                                  (point))))
                         (looking-back " *\\(?:use\\|require\\|package\\) +[^ ]+" bol))
                       (sepia-looks-like-module obj))
                      (sepia-core-version obj)
                      ""))))
      "")))

(defun sepia-install-eldoc ()
  "Install Sepia hooks for eldoc support."
  (interactive)
  (require 'eldoc)
  (set-variable 'eldoc-documentation-function 'sepia-symbol-info t)
  (if cperl-lazy-installed (cperl-lazy-unstall))
  (eldoc-mode 1)
  (set-variable 'eldoc-idle-delay 1.0 t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Error jump:

(defun sepia-extract-next-warning (pos &optional end)
  (catch 'foo
    (while (re-search-forward "^\\(.+\\) at \\(.+?\\) line \\([0-9]+\\)"
                              end t)
      (unless (string= "(eval " (substring (match-string 2) 0 6))
        (throw 'foo (list (match-string 2)
                          (string-to-number (match-string 3))
                          (match-string 1)))))))

(defun sepia-goto-error-at (pos)
  "Visit the source of the error on line at point."
  (interactive "d")
  (ifa (sepia-extract-next-warning (sepia-bol-from pos) (sepia-eol-from pos))
       (destructuring-bind (file line msg) it
	 (find-file file)
	 (goto-line line)
	 (message "%s" msg))
       (error "No error to find.")))

(defun sepia-display-errors (beg end)
  "Display source causing errors in current buffer from BEG to END."
  (interactive "r")
  (goto-char beg)
  (let ((msgs nil))
    (loop for w = (sepia-extract-next-warning (sepia-bol-from (point)) end)
       while w
       do (destructuring-bind (file line msg) w
            (push (format "%s:%d:%s\n" (abbreviate-file-name file) line msg)
                  msgs)))
    (erase-buffer)
    (goto-char (point-min))
    (mapcar #'insert (nreverse msgs))
    (goto-char (point-min))
    (grep-mode)))

(defun sepia-lisp-to-perl (thing)
  "Convert elisp data structure to Perl."
  (cond
    ((null thing) "undef")
    ((symbolp thing)
     (let ((pname (substitute ?_ ?- (symbol-name thing)))
           (type (string-to-char (symbol-name thing))))
       (if (member type '(?% ?$ ?@ ?*))
           pname
           (concat "\\*" pname))))
    ((stringp thing) (format "%S" (substring-no-properties thing 0)))
    ((integerp thing) (format "%d" thing))
    ((numberp thing) (format "%g" thing))
    ;; Perl expression
    ((and (consp thing) (eq (car thing) 'expr))
     (cdr thing))        ; XXX -- need quoting??
    ((and (consp thing) (not (consp (cdr thing))))
     (concat (sepia-lisp-to-perl (car thing)) " => "
             (sepia-lisp-to-perl (cdr thing))))
    ;; list
    ((or (not (consp (car thing)))
         (listp (cdar thing)))
     (concat "[" (mapconcat #'sepia-lisp-to-perl thing ", ") "]"))
    ;; hash table
    (t
     (concat "{" (mapconcat #'sepia-lisp-to-perl thing ", ") "}"))))

(defun sepia-init-perl-builtins ()
  (setq sepia-perl-builtins (make-hash-table))
  (dolist (s '("abs"
"accept"
"alarm"
"atan2"
"bind"
"binmode"
"bless"
"caller"
"chdir"
"chmod"
"chomp"
"chop"
"chown"
"chr"
"chroot"
"close"
"closedir"
"connect"
"continue"
"cos"
"crypt"
"dbmclose"
"dbmopen"
"defined"
"delete"
"die"
"dump"
"each"
"endgrent"
"endhostent"
"endnetent"
"endprotoent"
"endpwent"
"endservent"
"eof"
"eval"
"exec"
"exists"
"exit"
"exp"
"fcntl"
"fileno"
"flock"
"fork"
"format"
"formline"
"getc"
"getgrent"
"getgrgid"
"getgrnam"
"gethostbyaddr"
"gethostbyname"
"gethostent"
"getlogin"
"getnetbyaddr"
"getnetbyname"
"getnetent"
"getpeername"
"getpgrp"
"getppid"
"getpriority"
"getprotobyname"
"getprotobynumber"
"getprotoent"
"getpwent"
"getpwnam"
"getpwuid"
"getservbyname"
"getservbyport"
"getservent"
"getsockname"
"getsockopt"
"glob"
"gmtime"
"goto"
"grep"
"hex"
"import"
"index"
"int"
"ioctl"
"join"
"keys"
"kill"
"last"
"lc"
"lcfirst"
"length"
"link"
"listen"
"local"
"localtime"
"log"
"lstat"
"map"
"mkdir"
"msgctl"
"msgget"
"msgrcv"
"msgsnd"
"next"
"oct"
"open"
"opendir"
"ord"
"pack"
"package"
"pipe"
"pop"
"pos"
"print"
"printf"
"prototype"
"push"
"quotemeta"
"rand"
"read"
"readdir"
"readline"
"readlink"
"readpipe"
"recv"
"redo"
"ref"
"rename"
"require"
"reset"
"return"
"reverse"
"rewinddir"
"rindex"
"rmdir"
"scalar"
"seek"
"seekdir"
"select"
"semctl"
"semget"
"semop"
"send"
"setgrent"
"sethostent"
"setnetent"
"setpgrp"
"setpriority"
"setprotoent"
"setpwent"
"setservent"
"setsockopt"
"shift"
"shmctl"
"shmget"
"shmread"
"shmwrite"
"shutdown"
"sin"
"sleep"
"socket"
"socketpair"
"sort"
"splice"
"split"
"sprintf"
"sqrt"
"srand"
"stat"
"study"
"sub"
"sub*"
"substr"
"symlink"
"syscall"
"sysopen"
"sysread"
"sysseek"
"system"
"syswrite"
"tell"
"telldir"
"tie"
"tied"
"time"
"times"
"truncate"
"uc"
"ucfirst"
"umask"
"undef"
"unlink"
"unpack"
"unshift"
"untie"
"utime"
"values"
"vec"
"wait"
"waitpid"
"wantarray"
"warn"
"write"
))
        (puthash s t sepia-perl-builtins)))

(provide 'sepia)
;;; sepia.el ends here
