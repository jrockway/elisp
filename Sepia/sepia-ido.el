(eval-when-compile
  (require 'ido)
  (require 'cl))

(defun* sepia-icompleting-recursive-read (prompt dir &key
                                                 list-fn
                                                 parent-fn
                                                 chdir-fn
                                                 rootp-fn
                                                 slashp-fn)
"Like `ido-read-file-name', but without all the file-specific
bells-and-whistles.  Arguments are:
    list-fn           list current dir
    parent-fn         get parent dir
    chdir-fn          change to dir
    rootp-fn          is dir root?
    slashp-fn         does dir end in slash?
"
  (flet ((ido-make-file-list (prefix)
           (setq ido-temp-list (funcall list-fn (or prefix ""))))
         (ido-exhibit () (sepia-ido-exhibit))
         (ido-is-root-directory (&optional dir)
           (funcall rootp-fn (or dir ido-current-directory)))
         (ido-set-current-directory (dir &optional subdir foo)
           (funcall chdir-fn dir subdir foo))
         (ido-final-slash (str &rest blah)
           (funcall slashp-fn str))
         (ido-file-name-directory (x)
           (funcall parent-fn x))
         ;; And stub out these two suckers...
         (ido-is-tramp-root (&rest blah) nil)
         (ido-nonreadable-directory-p (dir) nil))
    (setq ido-current-directory dir)
    (let ((ido-saved-vc-hb nil)
          (ido-directory-nonreadable nil)
          (ido-context-switch-command 'ignore)
          (ido-directory-too-big nil))
      (sepia-ido-read-internal 'file prompt nil nil t))))

(defun sepia-rootp-fn (dir)
  (member dir '("" "::")))

(defun sepia-chdir-fn (dir sub blah)
  (setq dir
        (cond
          (sub (concat dir (car ido-matches)))
          ((member dir (list ido-current-directory "::")) dir)
          ((string-match (concat "^" dir) ido-current-directory)
           dir)
          (t (concat ido-current-directory (car ido-matches)))))
  ;; XXX what's that doing?!?
  ;; (unless ido-matches
  ;;   (error "help! dir = %s" dir))
  ;; (setq dir (concat ido-current-directory (car ido-matches)))
  (if (string-equal ido-current-directory dir)
      nil
      ;; XXX: concat?
      (setq ido-current-directory (ido-final-slash dir))
      (when (get-buffer ido-completion-buffer)
        (kill-buffer ido-completion-buffer))
      t))

(defun sepia-list-fn (str)
  (let ((listing-dir ido-current-directory))
    (when (or (not ido-current-directory)
              (string-match "^\\(?:::\\)?$" ido-current-directory))
      (setq ido-current-directory ""
            listing-dir "::"))
    (mapcar (lambda (x)
              (substring x (length listing-dir)))
            (xref-apropos (concat listing-dir str ".*") t "CODE" "STASH"))))

(defun sepia-dir-fn (str)
  (if (string-match "^\\(.*::\\)[^:]+:*$" str)
      (match-string 1 str)
      ""))

(defun sepia-slashp-fn (str)
  (cond
    ((string-match "::$" str) str)
    ((string-match ":$" str) (concat str ":"))
    (t nil)))

(defun sepia-jump-to-symbol ()
"Jump to a symbol's definition using ido-like completion."
  (interactive)
  (let ((pack (concat (sepia-buffer-package) "::"))
        ido-case-fold)
    (sepia-location
     (sepia-icompleting-recursive-read "Jump to: " pack
                                       :list-fn 'sepia-list-fn
                                       :parent-fn 'sepia-dir-fn
                                       :chdir-fn 'sepia-chdir-fn
                                       :rootp-fn 'sepia-rootp-fn
                                       :slashp-fn 'sepia-slashp-fn)
     t)))

(defun sepia-ido-exhibit ()
  "Post command hook for `sepia-icompleting-recursive-read'.
Like `ido-exhibit', but without weird file-specific bells and
whistles.  Since ido is controlled through a bunch of dynamic
variables, it's hard to figure out what can be safely cut."

  (when (= ido-use-mycompletion-depth (minibuffer-depth))
    (let ((contents (buffer-substring-no-properties (minibuffer-prompt-end)
                                                    (point-max)))
	  (buffer-undo-list t)
	  try-single-dir-match)

      (save-excursion
	(goto-char (point-max))
	;; Register the end of input, so we know where the extra stuff
	;; (match-status info) begins:
	(unless (boundp 'ido-eoinput)
	  ;; In case it got wiped out by major mode business:
	  (make-local-variable 'ido-eoinput))
	(setq ido-eoinput (point))

	;; Handle explicit directory changes
	(when (ido-final-slash contents)
          (ido-set-current-directory contents)
	  (setq ido-exit 'refresh)
	  (exit-minibuffer)
          (setq ido-text-init ""))

	;; Update the list of matches
	(setq ido-text contents)
	(ido-set-matches)

        ;; Enter something ending in a "slash"
	(when (and ido-matches
		   (null (cdr ido-matches))
		   (ido-final-slash (car ido-matches))
		   try-single-dir-match)
	  (ido-set-current-directory
	   (concat ido-current-directory (car ido-matches)))
	  (setq ido-exit 'refresh)
	  (exit-minibuffer))

	(setq ido-rescan t)

	(ido-set-common-completion)
	(let ((inf (ido-completions
		    contents
		    minibuffer-completion-table
		    minibuffer-completion-predicate
		    (not minibuffer-completion-confirm))))
	  (insert inf))))))


(defun sepia-ido-complete ()
  "Try to complete the current pattern amongst the file names."
  (interactive)
  (let (res)
    (cond

      ((not ido-matches)
       (when ido-completion-buffer
         (call-interactively (setq this-command ido-cannot-complete-command))))

      ((= 1 (length ido-matches))
       ;; only one choice, so select it.
       (if (not ido-confirm-unique-completion)
           (exit-minibuffer)
           (setq ido-rescan (not ido-enable-prefix))
           (delete-region (minibuffer-prompt-end) (point))
           (insert (car ido-matches))))

      (t ;; else there could be some completions
       (setq res ido-common-match-string)
       (if (and (not (memq res '(t nil)))
                (not (equal res ido-text)))
           ;; found something to complete, so put it in the minibuffer.
           (progn
             ;; move exact match to front if not in prefix mode
             (setq ido-rescan (not ido-enable-prefix))
             (delete-region (minibuffer-prompt-end) (point))
             (insert res))
           ;; else nothing to complete
           (call-interactively
            (setq this-command ido-cannot-complete-command)))))))

(defun sepia-ido-read-internal (item prompt history &optional
                                default require-match initial)
  "Perform the ido-read-buffer and ido-read-file-name functions.
Return the name of a buffer or file selected.
PROMPT is the prompt to give to the user.
DEFAULT if given is the default directory to start with.
If REQUIRE-MATCH is non-nil, an existing file must be selected.
If INITIAL is non-nil, it specifies the initial input string."
  (let
      ((ido-cur-item item)
       (ido-entry-buffer (current-buffer))
       (ido-process-ignore-lists t)
       (ido-process-ignore-lists-inhibit nil)
       (ido-set-default-item t)
       ido-default-item
       ido-selected
       ido-final-text
       (done nil)
       (icomplete-mode nil) ;; prevent icomplete starting up
       ;; Exported dynamic variables:
       ido-cur-list
       ido-ignored-list
       (ido-rotate-temp nil)
       (ido-keep-item-list nil)
       (ido-use-merged-list nil)
       (ido-try-merged-list t)
       (ido-pre-merge-state nil)
       (ido-case-fold ido-case-fold)
       (ido-enable-prefix ido-enable-prefix)
       (ido-enable-regexp ido-enable-regexp)
       )

    ;; (ido-define-mode-map)
    (ido-setup-completion-map)
    (setq ido-text-init initial)
    (while (not done)
      (ido-trace "\n_LOOP_" ido-text-init)
      (setq ido-exit nil)
      (setq ido-rescan t)
      (setq ido-rotate nil)
      (setq ido-text "")
      ;; XXX: set ido-default-item?

      (if ido-keep-item-list
	(setq ido-keep-item-list nil
	      ido-rescan nil)
	(setq ido-ignored-list nil
	      ido-cur-list (ido-make-file-list ido-default-item)))

      (setq ido-rotate-temp nil)

      (ido-set-matches)
      (if (and ido-matches (eq ido-try-merged-list 'auto))
	  (setq ido-try-merged-list t))
      (let
	  ((minibuffer-local-completion-map ido-completion-map)
	   (max-mini-window-height (or ido-max-window-height
				       (and (boundp 'max-mini-window-height)
                                            max-mini-window-height)))
	   (ido-completing-read t)
	   (ido-require-match require-match)
	   (ido-use-mycompletion-depth (1+ (minibuffer-depth)))
	   (show-paren-mode nil))
	;; prompt the user for the file name
	(setq ido-exit nil)
	(setq ido-final-text
	      (catch 'ido
		(completing-read
		 (ido-make-prompt item prompt)
		 '(("dummy" . 1)) nil nil ; table predicate require-match
		 (prog1 ido-text-init (setq ido-text-init nil))	;initial-contents
		 history))))

      (if (get-buffer ido-completion-buffer)
	  (kill-buffer ido-completion-buffer))

      (cond
       ((eq ido-exit 'refresh)
	(if (and (eq ido-use-merged-list 'auto)
		 (or (input-pending-p)))
	    (setq ido-use-merged-list nil
		  ido-keep-item-list t))
	nil)

       ((eq ido-exit 'done)
	(setq done t
	      ido-selected ido-text
	      ido-exit nil)

        (setq ido-text-init (read-string (concat prompt "[EDIT] ")
                                         ido-final-text)))

       ((eq ido-exit 'keep)
	(setq ido-keep-item-list t))

       ((memq ido-exit '(dired fallback findfile findbuffer))
	(setq done t))

       ((eq ido-exit 'updir)
	;; cannot go up if already at the root-dir (Unix) or at the
	;; root-dir of a certain drive (Windows or MS-DOS).
        (unless (ido-is-root-directory)
          (ido-set-current-directory (ido-file-name-directory
                                      (substring ido-current-directory 0 -2)))
          (setq ido-set-default-item t)))

       ;; Handling the require-match must be done in a better way.
       ((and require-match (not (ido-existing-item-p)))
	(error "must specify valid item"))

       (t
	(setq ido-selected
	      (if (or (eq ido-exit 'takeprompt)
		      (null ido-matches))
		  ido-final-text
		;; else take head of list
		(ido-name (car ido-matches))))

	(cond

	 ((ido-final-slash ido-selected)
	  (ido-set-current-directory ido-current-directory ido-selected)
	  (setq ido-set-default-item t))

	 (t
	  (setq done t))))))
    ido-selected))

(provide 'sepia-ido)
