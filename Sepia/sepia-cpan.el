(require 'button)

(define-button-type 'sepia-cpan
  'follow-link nil
  'action 'sepia-cpan-button
  'help-echo "[r]eadme, [d]ocumentation, [i]nstall"
  'keymap sepia-cpan-mode-map)

(defvar sepia-cpan-actions
  '(("r" . sepia-cpan-readme)
    ("d" . sepia-cpan-doc)
    ("i" . sepia-cpan-install)
    ("b" . sepia-cpan-browse)
    ("?" . sepia-cpan-readme)))

(defun sepia-cpan-doc (mod)
  "Browse the online Perldoc for MOD."
  (interactive "sModule: ")
  (browse-url (concat "http://search.cpan.org/perldoc?" mod)))

(defun sepia-cpan-readme (mod)
  "Display the README file for MOD."
  (interactive "sModule: ")
  (with-current-buffer (get-buffer-create "*sepia-cpan-readme*")
    (insert (sepia-call "Sepia::CPAN::readme" 'list-context mod))
    (pop-to-buffer (current-buffer))))

(defun sepia-cpan-install (mod)
  "Install MOD and its prerequisites."
  (interactive "sModule: ")
  (when (y-or-n-p (format "Install %s? " mod))
    (sepia-call "Sepia::CPAN::install" 'void-context mod)))

(defun sepia-cpan-list (pattern)
  "Return a list modules matching PATTERN."
  ;; (interactive "sPattern (regexp): ")
  (sepia-eval (format "map { Sepia::CPAN::interesting_parts $_ } Sepia::CPAN::list('/%s/')" pattern)
              'list-context))

(defun sepia-cpan-button (button)
  (funcall (cdr (assoc sepia-cpan-button sepia-cpan-actions))
           (button-label button)))

(defvar sepia-cpan-button)

(defun sepia-cpan-button-press ()
  (interactive)
  (let ((sepia-cpan-button (this-command-keys)))
    (push-button)))

(defvar sepia-cpan-mode-map
  (let ((km (make-sparse-keymap)))
    (set-keymap-parent km button-map)
    ;; (define-key km "q" 'bury-buffer)
    (define-key km "/" 'sepia-cpan-search)
    (define-key km "s" 'sepia-cpan-search)
    (dolist (k (mapcar #'car sepia-cpan-actions))
      (define-key km k 'sepia-cpan-button-press))
    km))

(define-derived-mode sepia-cpan-mode view-mode "CPAN"
  "Major mode for CPAN browsing.")

(defun sepia-cpan-search (pat)
  (interactive  "sPattern (regexp): ")
  (switch-to-buffer "*sepia-cpan*")
  (sepia-cpan-mode)
  (setq buffer-read-only nil)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (insert (format "\
CPAN modules matching /%s/
    [r]eadme, [d]ocumentation, [i]nstall, [s]earch

" pat))
  (let ((mods (sepia-cpan-list pat))
        (fields 
         '("id" "fullname" "inst_version" "cpan_version" "cpan_file"))
        lengths fmt)
    (when mods
      (dolist (mod mods)
        (setcdr (assoc "cpan_file" mod)
                (replace-regexp-in-string "^.*/" ""
                                          (cdr (assoc "cpan_file" mod)))))
      (setq lengths
            (mapcar
             (lambda (f)
               (+ 2 (apply #'max (mapcar
                                  (lambda (x)
                                    (length (format "%s" (cdr (assoc f x)))))
                                  mods))))
             fields))
      (setq fmt
            (concat (mapconcat (lambda (x) (format "%%-%ds" x)) lengths "")
                    "\n"))
      (insert (format fmt "Module" "Author" "Inst." "CPAN" "Distribution"))
      (insert (format fmt "------" "------" "-----" "----" "------------"))
      (dolist (mod mods)
        (let ((beg (point)))
          (insert
           (apply #'format fmt
                  (mapcar (lambda (x) (cdr (assoc x mod))) fields)))
          (make-button beg (+ beg (length (cdr (assoc "id" mod))))
                       :type 'sepia-cpan)))))
  (setq buffer-read-only t
        truncate-lines t))

(provide 'sepia-cpan)
