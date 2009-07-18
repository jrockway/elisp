;;; eshell-extras.el --- extra eshell commands
;;; Code:

(require 'my-macros)
(require 'eproject)
(require 'eproject-extras)

(defun eshell/clear ()
  "Clear the screen."
  (dotimes (i 50) (eshell-print "\n"))
  ;;(recenter 1)
  nil)

(define-invoke-for-each eshell/byte-compile-file byte-compile-file
  "Byte-compile the given FILES"
  files)

(defun eshell/rcd ()
  "CD to the project root."
  (let* ((pwd (eshell/pwd))
         (no-kill-p (get-file-buffer pwd)))
    (eshell/cd
     (with-current-buffer (find-file pwd)
       (prog1 (eproject-root)
         (when (not no-kill-p)
           (kill-buffer)))))))

(defvar eproject-project-locations
  '("/home/jon/projects/"
    "/home/jon/projects/cpan_modules/"
    "/home/jon/work")
  "Places for pcd to look in for projects.")

(defun eshell/pcd (project)
  "CD to the root of PROJECT."

  (eshell/cd
   (let+ (eproject-project-root project)
         (if _
             _
           (loop for base in
                   (mapcar #'file-name-as-directory
                           eproject-project-locations)
                 when (file-exists-p (concat base project))
                 return (concat base project))))))

(defun eshell/magit ()
  "Run magit status here."
  (call-interactively #'magit-status)
  nil)

(defun eshell/loop (&rest args)
  (eval
   (cons 'loop
         (mapcar (lambda (x)
                   (cond ((stringp x) (read x))
                         ((listp x) `(quote ,x))
                         (t x)))
                 args))))

(provide 'eshell-extras)
;;; eshell-extras.el ends here
