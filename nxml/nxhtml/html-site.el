;;; html-site.el --- Keeping (X)HTML files together

;; Copyright (C) 2006, 2007 by Lennart Borgman

;; Author: Lennart Borgman <lennartDOTborgmanDOT073ATstudentDOTluDOTse>
;; Created: Wed Mar 01 17:25:52 2006
(defconst html-site:version "0.2");; Version:
;; Last-Updated: Sat Sep 22 13:16:46 2007 (7200 +0200)
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;; TODO: maybe use browse-url-filename-alist

(eval-when-compile (require 'cl))
(eval-when-compile (load-library "cl-macs"))
(defvar html-site-list) ;; Silence compiler
(defvar html-site-current) ;; Silence compiler

(defun html-site-dir-contains (dir file)
  (when (= ?~ (string-to-char file))
    (setq file (expand-file-name file)))
  (let ((d (file-name-as-directory dir)))
    (if (< (length d) (length file))
        (string= d (substring file 0 (length d)))
      (when (file-directory-p file)
        (string= d (file-name-as-directory file))))))

(defun html-site-chk-wtocdir (out-dir site-dir)
  (or
   (unless (file-name-absolute-p out-dir)
     (lwarn '(html-site) :error "Output directory is not absolute: %s" out-dir))
   (if (file-exists-p out-dir)
       (unless (file-directory-p out-dir)
         (lwarn '(html-site) :error "File %s for output exists but is not a directory" out-dir))
     (unless (string= out-dir (file-name-as-directory out-dir))
       (lwarn '(html-site) :error "File name could not be a directory: %s" out-dir)))
   (when (html-site-dir-contains out-dir site-dir)
     (lwarn '(html-site) :error "Ouput directory for pages with TOC must not contain site dir."))
   (when (html-site-dir-contains site-dir out-dir)
     (lwarn '(html-site) :error "Site dir must not contain ouput directory for pages with TOC."))))



(defun html-site-set-site (name)
  (interactive
   (let ((site-names)
         must-contain
         (use-dialog-box nil))
     (unless (< 0 (length html-site-list))
       (error "No sites defined yet"))
     (when (and buffer-file-name
                ;;(string-match "ml" (symbol-name major-mode))
                )
       (when (y-or-n-p "Should site contain current file? ")
         (setq must-contain buffer-file-name)))
     (dolist (m html-site-list)
       (let* ((name (elt m 0))
              (dir  (html-site-site-dir name)))
         (when (or (not must-contain)
                   (html-site-dir-contains dir buffer-file-name))
           (setq site-names (cons name site-names)))))
     (unless site-names
       (when must-contain
         (error "No sites contains %s" must-contain)))
     (list (when site-names
             (let ((prompt (if (< 0 (length html-site-current))
                               (concat "Current site is \""
                                       html-site-current
                                       "\". New site's name: ")
                             "Site name: ")))
               (completing-read prompt site-names nil t nil 'site-names))))))
  (unless (or (string= name "")
              (string= name html-site-current))
    (setq html-site-current name)
    (customize-save-variable 'html-site-current html-site-current)))


(defun html-site-ensure-site-defined (site-name)
  (unless html-site-list
    (error "No sites defined. Please customize `html-site-list'."))
  (unless (file-directory-p (html-site-site-dir site-name))
    (error "Local file web site directory does not exists: %s"
           (html-site-site-dir site-name))))
(defun html-site-current-ensure-site-defined ()
  (unless (and (< 0 (length html-site-current))
               (assoc html-site-current html-site-list))
    (error "No current site set"))
  (html-site-ensure-site-defined html-site-current))

(defun html-site-remote-contains (site-name url with-toc)
  (html-site-dir-contains (html-site-remote-root site-name with-toc) url))
(defun html-site-current-remote-contains (url with-toc)
  (html-site-remote-contains html-site-current url with-toc))

(defun html-site-ensure-file-in-site (site-name file-name)
  (html-site-ensure-site-defined site-name)
  (unless (html-site-contains site-name file-name)
    (error "This file is not in site %s" site-name)))
(defun html-site-current-ensure-file-in-site (file-name)
  (html-site-ensure-file-in-site html-site-current file-name))

(defun html-site-ensure-buffer-in-site (site-name)
  (unless buffer-file-name
    (error "This buffer is not visiting a file"))
  (html-site-ensure-file-in-site site-name buffer-file-name))
(defun html-site-current-ensure-buffer-in-site ()
  (html-site-ensure-buffer-in-site html-site-current))


(defun html-site-site-dir (site-name)
  (file-name-as-directory
   (nth 1 (assoc site-name html-site-list))))
(defun html-site-current-site-dir () (html-site-site-dir html-site-current))

(defun html-site-contains (site-name file)
  (html-site-dir-contains (html-site-site-dir site-name) file))
(defun html-site-current-contains (file)
  (html-site-contains html-site-current file))

(defun html-site-page-list (site-name)
  (nth 2 (assoc site-name html-site-list)))
(defun html-site-current-page-list () (html-site-page-list html-site-current))

(defun html-site-frames-file (site-name)
  (nth 3 (assoc site-name html-site-list)))
(defun html-site-current-frames-file () (html-site-frames-file html-site-current))

(defun html-site-toc-file (site-name)
  (nth 4 (assoc site-name html-site-list)))
(defun html-site-current-toc-file () (html-site-toc-file html-site-current))

(defun html-site-merge-dir (site-name)
  (nth 5 (assoc site-name html-site-list)))
(defun html-site-current-merge-dir () (html-site-merge-dir html-site-current))

(defun html-site-merge-template (site-name)
  (nth 6 (assoc site-name html-site-list)))
(defun html-site-current-merge-template () (html-site-merge-template html-site-current))

(defun html-site-extra-fun (site-name)
  (nth 7 (assoc site-name html-site-list)))
(defun html-site-current-extra-fun () (html-site-extra-fun html-site-current))

(defun html-site-ftp-host (site-name)
  (nth 8 (assoc site-name html-site-list)))
(defun html-site-current-ftp-host () (html-site-ftp-host html-site-current))

(defun html-site-ftp-user (site-name)
  (nth 9 (assoc site-name html-site-list)))
(defun html-site-current-ftp-user () (html-site-ftp-user html-site-current))

(defun html-site-ftp-password (site-name)
  (nth 10 (assoc site-name html-site-list)))
(defun html-site-current-ftp-password () (html-site-ftp-password html-site-current))

(defun html-site-ftp-dir (site-name)
  (nth 11 (assoc site-name html-site-list)))
(defun html-site-current-ftp-dir () (html-site-ftp-dir html-site-current))

(defun html-site-ftp-wtoc-dir (site-name)
  (nth 12 (assoc site-name html-site-list)))
(defun html-site-current-ftp-wtoc-dir () (html-site-ftp-wtoc-dir html-site-current))

(defun html-site-web-host (site-name)
  (nth 13 (assoc site-name html-site-list)))
(defun html-site-current-web-host () (html-site-web-host html-site-current))

(defun html-site-web-dir (site-name)
  (nth 14 (assoc site-name html-site-list)))
(defun html-site-current-web-dir () (html-site-web-dir html-site-current))

(defun html-site-web-wtoc-dir (site-name)
  (nth 15 (assoc site-name html-site-list)))
(defun html-site-current-web-wtoc-dir () (html-site-web-wtoc-dir html-site-current))

(defun html-site-web-full (site-name with-toc)
  (let ((host (html-site-web-host site-name)))
    (unless (and host
                 (< 0 (length host)))
      (error "Web site host not known for %s" site-name))
    (save-match-data
      (unless (string-match "^https?://" host)
        (setq host (concat "http://" host))))
    (concat host
            (if with-toc
                (html-site-web-wtoc-dir site-name)
              (html-site-web-dir site-name)))))
(defun html-site-current-web-full (with-toc)
  (html-site-web-full html-site-current with-toc))

(defvar html-site-ftp-temporary-passwords nil)
(defun html-site-get-ftp-pw ()
  (let ((pw (html-site-current-ftp-password)))
    (unless (< 0 (length pw))
      (let* ((user-site (concat (html-site-current-ftp-user)
                                "@"
                                (html-site-current-ftp-host)))
             (site-pw (assoc user-site html-site-ftp-temporary-passwords)))
        (if site-pw
            (setq pw (cdr site-pw))
          (setq pw (read-string
                    (concat "Ftp password for "
                            (html-site-current-ftp-user)
                            " at "
                            (html-site-current-ftp-host)
                            " : ")))
          (setq html-site-ftp-temporary-passwords
                (cons
                 (cons user-site pw)
                 html-site-ftp-temporary-passwords)))))
    pw))





(defun html-site-path-in-mirror (site-root path-in-site mirror-root)
  (assert (html-site-dir-contains site-root path-in-site))
  (let ((rel-path (file-relative-name path-in-site site-root)))
    (if (string= rel-path ".")
        (directory-file-name mirror-root)
      (concat (file-name-as-directory mirror-root) rel-path))))

;; Some checks to see if html-site-path-in-mirror works:
(when t
  (assert (string=
           "http://some.site/tempmirror/in/hej.html"
           (html-site-path-in-mirror "c:/temp"
                                     "c:/temp/in/hej.html"
                                     "http://some.site/tempmirror")))
  (assert (string=
           "c:/temp/in/hej.html"
           (html-site-path-in-mirror "http://some.site/tempmirror"
                                     "http://some.site/tempmirror/in/hej.html"
                                     "c:/temp")))
  (assert (string=
           "in/hej.html"
           (file-relative-name "http:/temp/in/hej.html" "http:/temp")))
  )


(defun html-site-local-to-web (site-name local-file with-toc)
  (html-site-ensure-file-in-site site-name local-file)
  (html-site-path-in-mirror (html-site-site-dir site-name)
                            local-file
                            (html-site-web-full site-name with-toc)))
(defun html-site-current-local-to-web (local-file with-toc)
  (html-site-local-to-web html-site-current local-file with-toc))

(defun html-site-remote-root (site-name with-toc)
  (concat "/ftp:"
          (html-site-ftp-user site-name)
          "@" (html-site-ftp-host site-name)
          ":"
          (if with-toc
              (html-site-ftp-wtoc-dir site-name)
            (html-site-ftp-dir site-name))))
(defun html-site-current-remote-root (with-toc)
  (html-site-remote-root html-site-current with-toc))

(defun html-site-local-to-remote (site-name local-file with-toc)
  (html-site-ensure-file-in-site site-name local-file)
  (html-site-path-in-mirror (html-site-site-dir site-name)
                            local-file
                            (html-site-remote-root site-name with-toc)))
(defun html-site-current-local-to-remote (local-file with-toc)
  (html-site-local-to-remote html-site-current local-file with-toc))

(defun html-site-remote-to-local (site-name remote-file with-toc)
  ;;(html-site-ensure-file-in-site remote-file)
  ;; Fix-me above
  (html-site-path-in-mirror (html-site-remote-root site-name with-toc)
                            remote-file
                            (html-site-site-dir site-name)))
(defun html-site-current-remote-to-local (remote-file with-toc)
  (html-site-remote-to-local html-site-current remote-file with-toc))


(defvar html-site-files-re "\.x?html?$")

(defun html-site-edit-pages-file ()
  "Edit the list of pages to be used for table of contents."
  (interactive)
  (html-site-current-ensure-site-defined)
  (find-file (html-site-current-page-list))
  )

(defun html-site-get-sub-files (dir file-patt)
  (let ((sub-files)
        (sub-dirs)
        (dir-files (directory-files dir t "^[^.]")))
    (dolist (f dir-files)
      (if (file-directory-p f)
          (add-to-list 'sub-dirs f)
        (when (string-match file-patt f)
          (add-to-list 'sub-files f))))
    (dolist (sub-dir sub-dirs)
      (setq sub-files (append sub-files (html-site-get-sub-files sub-dir file-patt)))
      )
    sub-files))

(defun html-site-file-is-local (filename)
  "Return t if FILENAME is a local file name.
No check is done that the file exists."
  ;;(find-file-name-handler "/ftp:c:/eclean/" 'file-exists-p)
  (null (find-file-name-handler filename 'file-exists-p)))

(defgroup html-site nil
  "Customization group for html-site."
  :group 'nxhtml)

(defcustom html-site-list nil
  "Known site directories and corresponding attributes.
Each element in the list is a list containing:

* Name for the site.
* Site root directory.
* Page list file - Pages for table of contents (TOC). Usually
  initially built from the site directory by
  `html-toc-create-pages-file'.
* Frames file.
* TOC file for the frames file.
* Output directory - where to put the merged TOC and site
  pages.
* Output template file - html template for merging. See `html-wtoc-dir'
  for examples.
* Function for additional tasks - for example copying images, style
  sheets, scripts etc.
--
"
  :type '(repeat
          (list
           (string :tag "*** Site name ***")
           (directory :tag "Site root directory")
           (file :tag "Page list file")
           (file :tag "Frames file")
           (file :tag "Contents file for frames")
           (directory :tag "Output directory for pages with TOC" :help-echo "Where to put the merged files")
           (file :tag "Template file for pages with TOC" :help-echo "HTML template for merging")
           (choice :tag "Extra function for pages with TOC"
                   (const nil :tag "Default function")
                   (function)
                   )
           (string :tag "Ftp host address")
           (string :tag "Ftp user")
           (string :tag "Ftp password")
           (string :tag "Ftp directory root")
           (string :tag "Ftp directory root for pages with TOC")
           (string :tag "Web host address")
           (string :tag "Web directory root")
           (string :tag "Web directory root for pages with TOC")
           ))
  :set (lambda (symbol value)
         (let ((ok t))
           (dolist (e value)
             (let (
                   (name     (elt e 0))
                   (site-dir (elt e 1))
                   (pag-file (elt e 2))
                   (frm-file (elt e 3))
                   (toc-file (elt e 4))
                   (out-dir  (elt e 5))
                   (tpl-file (elt e 6))
                   (fun      (elt e 7))
                   (ftp-host (elt e 8))
                   (ftp-user (elt e 9))
                   (ftp-pw   (elt e 10))
                   (ftp-dir  (elt e 11))
                   (ftp-wtoc-dir (elt e 12))
                   (web-host (elt e 13))
                   (web-dir  (elt e 14))
                   (web-wtoc-dir (elt e 15))
                   )
               (unless (not (string= "" name))
                 (lwarn '(html-site-list) :error "Empty site name"))
               (if (not (file-directory-p site-dir))
                   (progn
                     (lwarn '(html-site-list) :error "Site directory for %s not found: %s" name site-dir)
                     (setq ok nil))
                 (unless (file-exists-p pag-file)
                   (lwarn '(html-site-list) :warning "Pages list file for %s does not exist: %s" name pag-file))
                 (unless (file-exists-p tpl-file)
                   (lwarn '(html-site-list) :warning "Template file for %s does not exist: %s" name tpl-file)))
               (when (< 0 (length out-dir))
                 (html-site-chk-wtocdir out-dir site-dir))
               (when fun
                 (unless (functionp fun)
                   (lwarn '(html-site-list) :error "Site %s - Unknown function: %s" name fun)
                   (setq ok nil)
                   ))
               ))
           (set-default symbol value)))
  :group 'html-site)

(defcustom html-site-current ""
  "Current site name.
Use the entry with this name in `html-site-list'."
  :set (lambda (symbol value)
         (or (when (= 0 (length value))
               (message "html-site-current (information): No current site set"))
             (let ((site-names))
               (dolist (m html-site-list)
                 (setq site-names (cons (elt m 0) site-names)))
               (or
                (unless (member value site-names)
                  (lwarn '(html-site-current) :error "Can't find site: %s" value))
                (let ((site-dir (html-site-site-dir value)))
                  (unless (file-directory-p site-dir)
                    (lwarn '(html-site-current) :error "Can't find site directory: %s" value))))))
         (set-default symbol value))
  :type 'string
  :set-after '(html-site-list)
  :group 'html-site)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Put subprocess here at the moment ...

(defconst noshell-procbuf-name "*Noshell process buffer*")

(defvar noshell-proc-name nil)
(defun noshell-procbuf-setup (procbuf-name)
  (unless procbuf-name
    (setq procbuf-name noshell-procbuf-name))
  (with-current-buffer (get-buffer-create procbuf-name)
    (unless (get-buffer-window (current-buffer))
      (when (one-window-p) (split-window))
      (let ((cb (current-buffer)))
        (set-window-buffer (other-window 1) cb)))
    ;;(setq buffer-read-only t)
    (noshell-process-mode)
    (compilation-minor-mode 1)
;;     (let ((inhibit-read-only t)
;;           (output-buffer (current-buffer)))
;;       (goto-char (point-max))
;;       (setq noshell-proc-name name)
;;       (let ((s (concat
;;                 "\n\n\n>>>>>>>>>>>>>>>>>> Starting "
;;                 noshell-proc-name "\n")))
;;         (put-text-property 0 (length s)
;;                            'face (list 'bold '(:foreground "green"))
;;                            s)
;;         (insert s)))
    (sit-for 0.01) ;; Display update
    (current-buffer)))

(defun noshell-procbuf-teardown (proc)
    (with-current-buffer (process-buffer proc)
      (goto-char (point-max))
      (let ((inhibit-read-only t)
            (s (concat
                "<<<<<<<<<<<<<<<<<<< Finished OK: "
                noshell-proc-name "\n")))
        (put-text-property 0 (length s)
                           'face (list 'bold '(:foreground "green"))
                           s)
        (insert s))))

(defun noshell-procbuf-run (buffer prog &rest args)
  (with-current-buffer buffer
    (let ((inhibit-read-only t)
          (proc nil)
          )
      (unwind-protect
          (progn
            (setq proc (apply 'start-process "myproc" (current-buffer) prog args))
            )
        )
      (save-excursion
        (unless proc
          (let ((s "\n\n<<<<<<<<<<<<< There was a process starting error!"))
            (put-text-property 0 (length s)
                               'face (list 'bold '(:foreground "red"))
                               s)
            (insert s))
          (error "Subprocess terminated with error status")))
      (set-process-sentinel proc 'noshell-sentinel)
      proc)
    )
  )
(defun noshell-sentinel (process event)
  (with-current-buffer (process-buffer process)
    (let ((inhibit-read-only t))
      ;;(insert (format "Process: %s recieved %s\n" process event))
      (cond ((string-match "abnormally" event)
             (let ((s (concat "\n<<<<<< Error: "
                              (substring event 0 -1)
                              " <<<<<<<<<")))
               (put-text-property 0 (length s)
                                  'face (list 'bold '(:foreground "red"))
                                  s)
               (insert s)))
            ((string-match "finished" event)
             (noshell-procbuf-teardown process))
            (t
             (insert event))))))

(defun noshell-procbuf-syncrun (prog &rest args)
  (with-current-buffer (get-buffer noshell-procbuf-name)
    (let ((inhibit-read-only t)
          (sts nil))
      (unwind-protect
          (progn
            ;;(setq sts (apply 'call-process prog nil (current-buffer) t args))
            (setq sts (apply 'call-process prog nil (list (current-buffer) t) t args))
            )
        )
      (save-excursion
        (unless (= 0 sts)
          (let ((s (format "\n\n<<<<<<<<<<<<< There was a process error: %s" sts)))
            (put-text-property 0 (length s)
                               'face (list 'bold '(:foreground "red"))
                               s)
            (insert s))
          (error "Subprocess terminated with error status")))
      )
    )
  )

(define-derived-mode noshell-process-mode fundamental-mode "Subprocess"
  nil
  (setq buffer-read-only t)
  (buffer-disable-undo (current-buffer))
  )
(define-key noshell-process-mode-map [(control ?c)(control ?k)] 'noshell-kill-subprocess)
(define-key noshell-process-mode-map [(control ?g)] 'noshell-quit)
(defun noshell-quit ()
  (interactive)
  (noshell-kill-subprocess)
  (keyboard-quit))
(defun noshell-kill-subprocess ()
  (interactive)
  (when (eq major-mode 'noshell-process-mode)
    (if (get-buffer-process (current-buffer))
        (interrupt-process (get-buffer-process (current-buffer)))
      (error "The subprocess is not running"))))



;; Provide here to be able to load the files in any order
(provide 'html-site)
(require 'html-upl nil t)
(defvar html-site-mode-menu-map
  (let ((map (make-sparse-keymap "html-site-mode-menu-map")))

    (when (featurep 'html-upl)
      (let ((upl-map (make-sparse-keymap)))
        (define-key map [html-site-upl-map]
          (list 'menu-item "File Transfer" upl-map))
        ;;(define-key upl-map [html-site-upl-edit-remote-wtoc]
        ;;  (list 'menu-item "Edit Remote File With TOC" 'html-upl-edit-remote-file-with-toc))
        (define-key upl-map [html-site-upl-edit-remote]
          (list 'menu-item "Edit Remote File" 'html-upl-edit-remote-file))
        (define-key upl-map [html-site-upl-ediff-buffer]
          (list 'menu-item "Ediff Remote/Local Files" 'html-upl-ediff-buffer))
        (define-key upl-map [html-site-upl-sep] (list 'menu-item "--"))
        (define-key upl-map [html-site-upl-upload-site-with-toc]
          (list 'menu-item "Upload Site with TOC" 'html-upl-upload-site-with-toc))
        (define-key upl-map [html-site-upl-upload-site]
          (list 'menu-item "Upload Site" 'html-upl-upload-site))
        (define-key upl-map [html-site-upl-upload-file]
          (list 'menu-item "Upload Single File" 'html-upl-upload-file))
      ))

    (let ((site-map (make-sparse-keymap)))
      (define-key map [html-site-site-map]
        (list 'menu-item "Site" site-map))
      (define-key site-map [html-site-customize-site-list]
        (list 'menu-item "Edit Sites" (lambda () (interactive)
                                        (customize-option 'html-site-list))))
      (define-key site-map [html-site-set-site]
        (list 'menu-item "Set Current Site" 'html-site-set-site))
      )

    map))


(defvar html-site-mode-map
  (let ((map (make-sparse-keymap )))
    (define-key map [menu-bar html-site-mode]
      (list 'menu-item "Web Site" html-site-mode-menu-map))
    map))

(define-minor-mode html-site-mode
  "Adds a menu for easy access of setting site, uploading etc."
  :init-value nil
  :lighter nil
  :keymap html-site-mode-map
  :group 'html-site)

(defvar html-site-mode-off-list
  '(nxhtml-mode))

(define-global-minor-mode html-site-global-mode html-site-mode
  (lambda ()
    (html-site-mode 1)
    (when t ;buffer-file-name
      (unless (memq major-mode html-site-mode-off-list)
        (html-site-mode 1))))
  :group 'html-site)
;; The problem with global minor modes:
(when (and html-site-global-mode
           (not (boundp 'define-global-minor-mode-bug)))
  (html-site-global-mode 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; html-site.el ends here
