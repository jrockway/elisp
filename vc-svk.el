;; Based upon vc-svn.el
;; Hack--most things are very slow/may work improperly.

;; Copyright (C) 1995, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006
;;           Free Software Foundation, Inc.

;; Author:      FSF (see vc.el for full credits)
;; Maintainer:

;; This file is not yet part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; I created this *only* to allow me to open files and do `vc-diff'.
;; If anything else even works, it's not my fault. :) This needs a lot
;; of love before it is a good VC backend, but it will allow you to do
;; some minimal tasks.

;; I primarily use FSF Emacs 22. It may not work with older Emacsen.
;; Or it might, thanks to your bug reports.

;; vc-svn commentary:
;; This is preliminary support for Subversion (http://subversion.tigris.org/).
;; It started as `sed s/cvs/svk/ vc.cvs.el' (from version 1.56)
;; and hasn't been completely fixed since.

;; Sync'd with Subversion's vc-svn.el as of revision 5801.

;;; Bugs:

;; - VC-dired is (really) slow.
;; - (svk) Most other commands are, too.

;;; Changelog:
;; * 20050707: Yikes! My original was inadvertently posted with two instances of misplaced parens/stale code and didn't work at all!
;;
;; * 20050711: Improve handling of files not in the repository.
;;
;; * Old bugs on Emacs 21.4 (are these fixed yet?):
;; 00 (defalias) only takes 2 arguments
;; 00 While switching to vc-dired-mode from dired mode: "No subdir-alist in lib".  vc-directory barfs with "Symbol's function definition is void: vc-stay-local-p".
;; 00 Loading any file yields "Symbol's definition is void: assoc-string".
;; 00 hate hate hate.
;;
;; * 20050816:
;; 00 Fixed more problems with added and unknown files.
;; 00 Fixed the `time-less-p' and `assoc-string' problems on Emacs <22.
;; 00 Fixed some unsual cases in vc-svk-do-status.
;; 00 Fixed vc-register (also fixes the defalias problem on Emacs <22).
;; 00 *May* fix the vc-dired problem. I can't test this, and don't use it.
;;
;; * 20050817:
;; 00 Handle directories correctly in `vc-svk-registered'.
;; 00 A few more compatibility fixes.
;;
;; * 20051006:
;; 00 Less crashing for subdirectories that aren't registered.
;; 00 Misc. cleanups.
;;
;; * 20051017:
;; 00 Tweak last fix.
;;
;; * 20051025:
;; 00 Require SVK 1.03, use it to get better status.
;; 00 Support vc-annotate (C-x v g) (more to be done).
;;
;; * 20051026: (The first new features in vc-svk that aren't in vc-svn!)
;; 00 Accurate coloring in vc-annotate. If you don't like the lag while the cache builds, set `vc-svk-annotate-absolutely' to nil.
;; 00 vc-annotate works in Emacs 21.
;;
;; * 20051102:
;; 00 One line fix for symlink to one WD inside another.
;;
;; * 20060219:
;; 00 Fix some rare crashes for un-added subtrees.
;; 00 New feature: `vc-annotate-revision-previous-to-line'

;;; Code:

(eval-when-compile
  (require 'vc))
(require 'cl)
(require 'time-date)

;; Compatibility with Emacs <22

(if (fboundp 'time-less-p)
    (defalias 'vc-svk-time-less-p 'time-less-p)
  (defun vc-svk-time-less-p (t1 t2)
    "Say whether time value T1 is less than time value T2."
    (with-decoded-time-value ((high1 low1 micro1 t1)
                              (high2 low2 micro2 t2))
      (or (< high1 high2)
          (and (= high1 high2)
               (or (< low1 low2)
                   (and (= low1 low2)
                        (< micro1 micro2))))))))

(if (fboundp 'assoc-string)
    (defalias 'vc-svk-assoc-string 'assoc-string)
  (defun vc-svk-assoc-string (key alist)
    (assoc-default key alist
                   (lambda (a b)
                     (and (stringp a) (stringp b) (string-equal a b))))))

;; SVK repositories are (almost always? all?) local anyway.
(defmacro vc-svk-stay-local-p (file) nil)

(if (fboundp 'vc-switches)
    (defalias 'vc-svk-switches 'vc-switches)
  (defun vc-svk-switches (backend op)
    (let ((switches
           (or (if backend
                   (let ((sym (vc-make-backend-sym
                               backend (intern (concat (symbol-name op)
                                                       "-switches")))))
                     (if (boundp sym) (symbol-value sym))))
               (let ((sym (intern (format "vc-%s-switches" (symbol-name op)))))
                 (if (boundp sym) (symbol-value sym)))
               (cond
                ((eq op 'diff) diff-switches)))))
      (if (stringp switches) (list switches)
        ;; If not a list, return nil.
        ;; This is so we can set vc-diff-switches to t to override
        ;; any switches in diff-switches.
        (if (listp switches) switches)))))

(unless (boundp 'vc-disable-async-diff)
  ;; pessimistic assumption
  (setq vc-disable-async-diff t))

(if (boundp 'vc-annotate-parent-file)
    (defun vc-svk-annotate-parent-file ()
      vc-annotate-parent-file)
  (defun vc-svk-annotate-parent-file ()
    (buffer-file-name vc-parent-buffer)))

(if (< emacs-major-version 22)
    (defun vc-svk-date-to-day (date)
      ;; SVN gives e.g. "2005-10-26T05:34:02.209866Z\n" which are
      ;; rejected by Emacs <22.
      (let ((i (string-match "T" date)))
        (date-to-day (if i
                         (concat (substring date 0 i)
                                 " "
                                 (substring date (1+ i)))
                       date))))
  (defalias 'vc-svk-date-to-day 'date-to-day))

;;;
;;; Customization options
;;;

(defcustom vc-svk-global-switches nil
  "*Global switches to pass to any SVK command."
  :type '(choice (const :tag "None" nil)
                 (string :tag "Argument String")
                 (repeat :tag "Argument List"
                         :value ("")
                         string))
  :version "21.4"
  :group 'vc)

(defcustom vc-svk-register-switches nil
  "*Extra switches for registering a file into SVK.
A string or list of strings passed to the checkin program by
\\[vc-register]."
  :type '(choice (const :tag "None" nil)
                 (string :tag "Argument String")
                 (repeat :tag "Argument List"
                         :value ("")
                         string))
  :version "21.4"
  :group 'vc)

(defcustom vc-svk-diff-switches
  t                           ;`svk' doesn't support common args like -c or -b.
  "String or list of strings specifying extra switches for svk diff under VC.
If nil, use the value of `vc-diff-switches'.
If you want to force an empty list of arguments, use t."
  :type '(choice (const :tag "Unspecified" nil)
                 (const :tag "None" t)
                 (string :tag "Argument String")
                 (repeat :tag "Argument List"
                         :value ("")
                         string))
  :version "21.4"
  :group 'vc)

(defcustom vc-svk-header (or (cdr (assoc 'SVK vc-header-alist)) '("\$Id\$"))
  "*Header keywords to be inserted by `vc-insert-headers'."
  :version "21.4"
  :type '(repeat string)
  :group 'vc)

(defconst vc-svk-use-edit nil
  ;; Subversion does not provide this feature (yet).
  "*Non-nil means to use `svk edit' to \"check out\" a file.
This is only meaningful if you don't use the implicit checkout model
\(i.e. if you have $SVKREAD set)."
  ;; :type 'boolean
  ;; :version "21.4"
  ;; :group 'vc
  )

(defconst vc-svk-status-file-re
  "^[ ADMCI?!~][ MC][ L][ +][ S]..\\([ *]\\) +\\([-0-9]+\\) +\\([0-9?]+\\) +\\([^ ]+\\) +")

;;;
;;; State-querying functions
;;;

;;; FIXME
;;;###autoload (add-to-list 'vc-handled-backends 'SVK)
;;;###autoload (defun vc-svk-registered (file)
;;;###autoload   (when (string-match
;;;###autoload          "^Checkout Path:"
;;;###autoload          (shell-command-to-string (concat "svk info "
;;;###autoload                                           (expand-file-name file))))
;;;###autoload     (setq file nil)
;;;###autoload     (load "vc-svk")
;;;###autoload     (vc-svk-registered file)))

(add-to-list 'vc-handled-backends 'SVK)
(defun vc-svk-registered (file)
  "Check if FILE is SVK registered."

  (let ((lfile (file-truename file))   ; SVK stores truenames
        (file-buffer (current-buffer)))
    (when (vc-svk-co-path-p lfile)
      (save-window-excursion            ; being left in some random buffer
                                        ; confuses `vc-find-file-hook'
        (with-temp-buffer
          (cd (file-name-directory lfile))
          (condition-case nil
              (progn
                (vc-svk-do-status lfile)
                (vc-svk-parse-status t (unless (string-equal file lfile)
                                         file))
                (eq 'SVK (vc-file-getprop file 'vc-backend)))
            ;; We can't find an `svk' executable.  We could also deregister SVK.
            (file-error nil)))))))

(defun vc-svk-state (file &optional localp)
  "SVK-specific version of `vc-state'."
  (setq localp (or localp (vc-svk-stay-local-p file)))
  (with-temp-buffer
    (cd (file-name-directory file))
    (vc-svk-do-status file)
    (vc-svk-parse-status localp)
    (vc-file-getprop file 'vc-state)))

(defun vc-svk-state-heuristic (file)
  "SVK-specific state heuristic."
  (vc-svk-state file 'local))

(defun vc-svk-dir-state (dir &optional localp)
  "Find the SVK state of all files in DIR."
  (setq localp (or localp (vc-svk-stay-local-p dir)))
  (let ((default-directory dir))
    ;; Don't specify DIR in this command, the default-directory is
    ;; enough.  Otherwise it might fail with remote repositories.
    (with-temp-buffer
      (vc-svk-do-status dir)
      (vc-svk-parse-status localp))))

(defun vc-svk-workfile-version (file)
  "SVK-specific version of `vc-workfile-version'."
  ;; There is no need to consult RCS headers under SVK, because we
  ;; get the workfile version for free when we recognize that a file
  ;; is registered in SVK.
  (vc-svk-registered file)
  (vc-file-getprop file 'vc-workfile-version))

(defun vc-log-version-at-point ()
  "Extract the revision number at point as a string."
  (buffer-substring-no-properties (1+ (point))
                                  (save-excursion
                                    (search-forward ":" nil t)
                                    (1- (point)))))

(defun vc-svk-previous-version (file rev)
  "The greatest revision number string before REV in which FILE was modified."
  ;; Parse log -q to find it. Non-optimal.
  (with-temp-buffer
    (vc-svk-command t 0 file "log" "-q")
    (goto-char (point-min))
    ;; If the file was modified in rev we can jump to it exactly.
    (search-forward-regexp (concat "^r" rev) nil t)
    (goto-char (match-beginning 0))
    (let ((revnum (string-to-number rev)))
      (unless (= revnum (string-to-number (vc-log-version-at-point)))
        ;; Otherwise, go line-by-line looking for it.
        (goto-char (point-min))
        (forward-line 1)
        (while (and (bolp) (< revnum
                              (string-to-number (vc-log-version-at-point))))
          (forward-line 2))
        (forward-line -2))
      ;; The line with the desired revnum:
      (forward-line 2)
      (when (bolp)
        (vc-log-version-at-point)))))

(defun vc-svk-checkout-model (file)
  "SVK-specific version of `vc-checkout-model'."
  ;; It looks like Subversion has no equivalent of CVSREAD.
  'implicit)

;; vc-svk-mode-line-string doesn't exist because the default implementation
;; works just fine.

(defun vc-svk-dired-state-info (file)
  "SVK-specific version of `vc-dired-state-info'."
  (let ((svk-state (vc-state file)))
    (cond ((eq svk-state 'edited)
           (if (equal (vc-workfile-version file) "0")
               "(added)" "(modified)"))
          ((eq svk-state 'needs-patch) "(patch)")
          ((eq svk-state 'needs-merge) "(merge)"))))

;;;
;;; State-changing functions
;;;

(defun vc-svk-register (file &optional rev comment)
  "Register FILE into the SVK version-control system.
COMMENT can be used to provide an initial description of FILE.

`vc-register-switches' and `vc-svk-register-switches' are passed to
the SVK command (in that order)."
  (apply 'vc-svk-command nil 0 file "add" (vc-svk-switches 'SVK 'register)))

(defun vc-svk-could-register (file)
  "Return non-nil if FILE could be registered in SVK.
This is only possible if SVK is responsible for FILE's directory."
  (and (vc-svk-co-path-of file)
       (vc-svk-registered (file-name-directory
                           (vc-svk-file-name-no-trailsep file)))))

(defun vc-svk-init-version () "1")

(defun vc-svk-checkin (file rev comment)
  "SVK-specific version of `vc-backend-checkin'."
  (let ((status (apply
                 'vc-svk-command nil 1 file "ci"
                 (nconc (list "-m" comment) (vc-svk-switches 'SVK 'checkin)))))
    (set-buffer "*vc*")
    (goto-char (point-min))
    (unless (equal status 0)
      ;; Check checkin problem.
      (cond
       ((search-forward "Transaction is out of date" nil t)
        (vc-file-setprop file 'vc-state 'needs-merge)
        (error (substitute-command-keys
                (concat "Up-to-date check failed: "
                        "type \\[vc-next-action] to merge in changes"))))
       (t
        (pop-to-buffer (current-buffer))
        (goto-char (point-min))
        (shrink-window-if-larger-than-buffer)
        (error "Check-in failed"))))
    ;; Update file properties
    ;; (vc-file-setprop
    ;;  file 'vc-workfile-version
    ;;  (vc-parse-buffer "^\\(new\\|initial\\) revision: \\([0-9.]+\\)" 2))
    ))

(defun vc-svk-find-version (file rev buffer)
  (apply 'vc-svk-command
         buffer 0 file
         "cat"
         (and rev (not (string= rev ""))
              (concat "-r" rev))
         (vc-svk-switches 'SVK 'checkout)))

(defun vc-svk-checkout (file &optional editable rev)
  (message "Checking out %s..." file)
  (with-current-buffer (or (get-file-buffer file) (current-buffer))
    (vc-call update file editable rev (vc-svk-switches 'SVK 'checkout)))
  (vc-mode-line file)
  (message "Checking out %s...done" file))

(defun vc-svk-update (file editable rev switches)
  (if (and (file-exists-p file) (not rev))
      ;; If no revision was specified, just make the file writable
      ;; if necessary (using `svk-edit' if requested).
      (and editable (not (eq (vc-svk-checkout-model file) 'implicit))
           (if vc-svk-use-edit
               (vc-svk-command nil 0 file "edit")
             (set-file-modes file (logior (file-modes file) 128))
             (if (equal file buffer-file-name) (toggle-read-only -1))))
    ;; Check out a particular version (or recreate the file).
    (vc-file-setprop file 'vc-workfile-version nil)
    (apply 'vc-svk-command nil 0 file
           "update"
           ;; default for verbose checkout: clear the sticky tag so
           ;; that the actual update will get the head of the trunk
           (cond
            ((null rev) "-rBASE")
            ((or (eq rev t) (equal rev "")) nil)
            (t (concat "-r" rev)))
           switches)))

(defun vc-svk-delete-file (file)
  (vc-svk-command nil 0 file "remove"))

(defun vc-svk-rename-file (old new)
  (vc-svk-command nil 0 new "move" (file-relative-name old)))

(defun vc-svk-revert (file &optional contents-done)
  "Revert FILE to the version it was based on."
  (unless contents-done
    (vc-svk-command nil 0 file "revert"))
  (unless (eq (vc-checkout-model file) 'implicit)
    (if vc-svk-use-edit
        (vc-svk-command nil 0 file "unedit")
      ;; Make the file read-only by switching off all w-bits
      (set-file-modes file (logand (file-modes file) 3950)))))

(defun vc-svk-merge (file first-version &optional second-version)
  "Merge changes into current working copy of FILE.
The changes are between FIRST-VERSION and SECOND-VERSION."
  (vc-svk-command nil 0 file
                 "merge"
                 "-r" (if second-version
                        (concat first-version ":" second-version)
                      first-version))
  (vc-file-setprop file 'vc-state 'edited)
  (with-current-buffer (get-buffer "*vc*")
    (goto-char (point-min))
    (if (looking-at "C  ")
        1                                ; signal conflict
      0)))                                ; signal success

(defun vc-svk-merge-news (file)
  "Merge in any new changes made to FILE."
  (message "Merging changes into %s..." file)
  ;; (vc-file-setprop file 'vc-workfile-version nil)
  (vc-file-setprop file 'vc-checkout-time 0)
  (vc-svk-command nil 0 file "update")
  ;; Analyze the merge result reported by SVK, and set
  ;; file properties accordingly.
  (with-current-buffer (get-buffer "*vc*")
    (goto-char (point-min))
    ;; get new workfile version
    (if (re-search-forward
         "^\\(Updated to\\|At\\) revision \\([0-9]+\\)" nil t)
        (vc-file-setprop file 'vc-workfile-version (match-string 2))
      (vc-file-setprop file 'vc-workfile-version nil))
    ;; get file status
    (goto-char (point-min))
    (prog1
        (if (looking-at "At revision")
            0 ;; there were no news; indicate success
          (if (re-search-forward
               (concat "^\\([CGDU]  \\)?"
                       (regexp-quote (file-name-nondirectory file)))
               nil t)
              (cond
               ;; Merge successful, we are in sync with repository now
               ((string= (match-string 1) "U  ")
                (vc-file-setprop file 'vc-state 'up-to-date)
                (vc-file-setprop file 'vc-checkout-time
                                 (nth 5 (file-attributes file)))
                0);; indicate success to the caller
               ;; Merge successful, but our own changes are still in the file
               ((string= (match-string 1) "G  ")
                (vc-file-setprop file 'vc-state 'edited)
                0);; indicate success to the caller
               ;; Conflicts detected!
               (t
                (vc-file-setprop file 'vc-state 'edited)
                1);; signal the error to the caller
               )
            (pop-to-buffer "*vc*")
            (error "Couldn't analyze svk update result")))
      (message "Merging changes into %s...done" file))))

;;;
;;; History functions
;;;

(defun vc-svk-print-log (file &optional buffer)
  "Get change log associated with FILE."
  (save-current-buffer
    (vc-setup-buffer buffer)
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      ;; Add a line to tell log-view-mode what file this is.
      (insert "Working file: " (file-relative-name file) "\n"))
    (vc-svk-command
     buffer
     (if (and (vc-svk-stay-local-p file) (fboundp 'start-process)) 'async 0)
     file "log")))

(defun vc-svk-diff (file &optional oldvers newvers buffer)
  "Get a difference report using SVK between two versions of FILE."
  (unless buffer (setq buffer "*vc-diff*"))
  (if (and oldvers (equal oldvers (vc-workfile-version file)))
      ;; Use nil rather than the current revision because svk handles it
      ;; better (i.e. locally).
      (setq oldvers nil))
  (if (string= (vc-workfile-version file) "0")
      ;; This file is added but not yet committed; there is no master file.
      (if (or oldvers newvers)
          (error "No revisions of %s exist" file)
        ;; We regard this as "changed".
        ;; Diff it against /dev/null.
        ;; Note: this is NOT a "svk diff".
        (apply 'vc-do-command buffer
               1 "diff" file
               (append (vc-svk-switches nil 'diff) '("/dev/null")))
        ;; Even if it's empty, it's locally modified.
        1)
    (let* ((switches
            (if vc-svk-diff-switches
                (vc-svk-switches 'SVK 'diff)
              (list "-x" (mapconcat 'identity (vc-svk-switches nil 'diff) " "))))
           (async (and (not vc-disable-async-diff)
                       (vc-svk-stay-local-p file)
                       (or oldvers newvers) ; Svk diffs those locally.
                       (fboundp 'start-process))))
      (apply 'vc-svk-command buffer
             (if async 'async 0)
             file "diff"
             (append
              switches
              (when oldvers
                (list "-r" (if newvers (concat oldvers ":" newvers)
                             oldvers)))))
      (if async 1                      ; async diff => pessimistic assumption
        ;; For some reason `svk diff' does not return a useful
        ;; status w.r.t whether the diff was empty or not.
        (buffer-size (get-buffer buffer))))))

(defun vc-svk-diff-tree (dir &optional rev1 rev2)
  "Diff all files at and below DIR."
  (vc-svk-diff (file-name-as-directory dir) rev1 rev2))

;;;
;;; Snapshot system
;;;

(defun vc-svk-create-snapshot (dir name branchp)
  "Assign to DIR's current version a given NAME.
If BRANCHP is non-nil, the name is created as a branch (and the current
workspace is immediately moved to that new branch).
NAME is assumed to be a URL."
  (vc-svk-command nil 0 dir "copy" name)
  (when branchp (vc-svk-retrieve-snapshot dir name nil)))

(defun vc-svk-retrieve-snapshot (dir name update)
  "Retrieve a snapshot at and below DIR.
NAME is the name of the snapshot; if it is empty, do a `svk update'.
If UPDATE is non-nil, then update (resynch) any affected buffers.
NAME is assumed to be a URL."
  (vc-svk-command nil 0 dir "switch" name)
  ;; FIXME: parse the output and obey `update'.
  )

;;;
;;; Annotate
;;;

(defun vc-svk-annotate-command (file buf &optional rev)
  (vc-svk-command buf 0 file "annotate" (if rev (concat "-r" rev)))
  (with-current-buffer buf
    (goto-char (point-min))
    (delete-region (point) (line-end-position 3))))

(defvar vc-svk-annotate-absolutely t
  "Non-nil to ask SVK about each revision's date in `vc-svk-annotate-time'.
Otherwise date annotations by revision number. There is a delay to get
the revision dates at first and a little memory to cache them.")
;; Keys: "<rev num>/depot/"
(defvar vc-svk-annotate-rev-days (make-hash-table :test 'equal))

(defvar vc-svk-annotate-buffer-depot nil)
(make-variable-buffer-local 'vc-svk-buffer-depot)

(defun vc-svk-annotate-time-of-rev (rev)
  (let* ((file (vc-svk-annotate-parent-file))
         (rev (or rev
                  (vc-workfile-version file)))
         (key (concat rev
                      (or vc-svk-annotate-buffer-depot
                          (setq vc-svk-annotate-buffer-depot
                                (vc-svk-repository-hostname file))))))
    (if vc-svk-annotate-absolutely
        (or (gethash key vc-svk-annotate-rev-days)
            (setf (gethash key vc-svk-annotate-rev-days)
                  (vc-svk-date-to-day
                   (shell-command-to-string
                    (apply 'concat
                           "svk propget --revprop svn:date -r" rev
                           vc-svk-global-switches)))))
      ;; Like SVN, arbitrarily assume 10 commmits per day.
      (/ (string-to-number rev) 10.0))))

(defun vc-svk-annotate-current-time ()
  (vc-svk-annotate-time-of-rev vc-annotate-parent-rev))

(defun vc-svk-annotate-time ()
  (vc-svk-annotate-time-of-rev (vc-svk-annotate-extract-revision-at-line)))
(defun vc-svk-annotate-difference (point)
  ;; Emacs 21 compatibility.
  (unless (= point (point-max))
    (goto-char point)
    (- (time-to-days (current-time))
       (vc-svk-annotate-time))))

(defun vc-svk-annotate-extract-revision-at-line ()
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward "^[ 	]+\\([0-9]+\\)[ 	]+("
                           (line-end-position) t)
        (match-string-no-properties 1)
      nil)))

;;;
;;; Miscellaneous
;;;

;; Subversion makes backups for us, so don't bother.
;; (defalias 'vc-svk-make-version-backups-p 'vc-svk-stay-local-p
;;   "Return non-nil if version backups should be made for FILE.")

(defun vc-svk-check-headers ()
  "Check if the current file has any headers in it."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "\\$[A-Za-z\300-\326\330-\366\370-\377]+\
\\(: [\t -#%-\176\240-\377]*\\)?\\$" nil t)))

;;;
;;; Internal functions
;;;

(defun vc-svk-command (buffer okstatus file &rest flags)
  "A wrapper around `vc-do-command' for use in vc-svk.el.
The difference to vc-do-command is that this function always invokes `svk',
and that it passes `vc-svk-global-switches' to it before FLAGS."
  (if (file-exists-p (expand-file-name "~/.svk/lock"))
      (error "Another svk might be running; remove ~/.svk/lock if not.")
    (apply 'vc-do-command buffer okstatus "svk" file
           (if (stringp vc-svk-global-switches)
               (cons vc-svk-global-switches flags)
             (append vc-svk-global-switches
                     flags)))))

(defun vc-svk-repository-hostname (file)
  "Ask SVK for the depot of FILE."
  ;; Used by vc-stay-local-p to make that decision per-hostname/path.
  ;; Used in vc-svk to get depots.
  (let ((info (vc-svk-do-info-string file)))
    (when (string-match "Depot Path: \\(/.*?/\\)" info)
      (match-string 1 info))))

(defun vc-svk-parse-status (localp &optional linked-file)
  "Parse output of `vc-svk-do-status' in the current buffer.
Set file properties accordingly."
  (let (file status)
    (goto-char (point-min))
    (while (re-search-forward vc-svk-status-file-re nil t)
      (setq file (or linked-file
                     (expand-file-name
                      (buffer-substring (point) (line-end-position)))))
      (setq status (char-after (line-beginning-position)))
      (unless (eq status ??)
        (vc-file-setprop file 'vc-backend 'SVK)
        ;; Use the last-modified revision, so that searching in vc-print-log
        ;; output works.
        (vc-file-setprop file 'vc-workfile-version (match-string 3))
        (vc-file-setprop
         file 'vc-state
         (cond
          ((eq status ?\ )
           (if (eq (char-after (match-beginning 1)) ?*)
               'needs-patch
             (vc-file-setprop file 'vc-checkout-time
                              (nth 5 (file-attributes file)))
             'up-to-date))
          ((eq status ?A)
           ;; If the file was actually copied, (match-string 2) is "-".
           (vc-file-setprop file 'vc-workfile-version "0")
           (vc-file-setprop file 'vc-checkout-time 0)
           'edited)
          ((memq status '(?M ?C))
           (if (eq (char-after (match-beginning 1)) ?*)
               'needs-merge
             'edited))
          (t 'edited)))))))

(defun vc-svk-dir-state-heuristic (dir)
  "Find the SVK state of all files in DIR, using only local information."
  (vc-svk-dir-state dir 'local))

(defun vc-svk-valid-symbolic-tag-name-p (tag)
  "Return non-nil if TAG is a valid symbolic tag name."
  ;; According to the SVK manual, a valid symbolic tag must start with
  ;; an uppercase or lowercase letter and can contain uppercase and
  ;; lowercase letters, digits, `-', and `_'.
  (and (string-match "^[a-zA-Z]" tag)
       (not (string-match "[^a-z0-9A-Z-_]" tag))))

(defun vc-svk-valid-version-number-p (tag)
  "Return non-nil if TAG is a valid version number."
  (and (string-match "^[0-9]" tag)
       (not (string-match "[^0-9]" tag))))

(defun vc-svk-do-status (file)
  ;; Don't crash if SVK didn't really have the file (e.g. un-added
  ;; subdir of co path). Each such error message must be parsed
  ;; equivilient to ? in `vc-svk-parse-status'.
  (ignore-errors
    (vc-svk-command t 0 file "status" "-Nv"))
  ;; SVN always puts file at the top of status output.
  ;; SVK puts it at the bottom if file is a dir, and additionally may
  ;; output it as a relative path.
  (when (file-directory-p file)
    (save-excursion
      (previous-line 1)
      (delete-region (point)
                     (point-min))
      (delete-region (re-search-forward vc-svk-status-file-re nil t)
                     (line-end-position))
      (insert file))))

(defsubst vc-svk-do-info-string (file)
  (shell-command-to-string (concat "svk info "
                                   (expand-file-name file))))

(defun vc-svk-file-name-no-trailsep (file)
  "Return filename minus trailing separators.

Caution! Cheats and onlya removes them when Emacs is known to put
them and they matter to vc-svk."
  (let ((end (1- (length file))))
    (if (and (file-directory-p file)
             (string-equal (substring file end) "/"))
        (substring file 0 end)
      file)))

(defvar vc-svk-co-paths nil)
(defun vc-svk-co-paths ()
  (interactive)
  (let ((config "~/.svk/config")
        mtime)
    (when (file-readable-p config)
      (setq mtime (nth 5 (file-attributes "~/.svk/config")))
      (unless (and vc-svk-co-paths           ; has not it been loaded?
                   (vc-svk-time-less-p mtime ; is it unmodified since?
                                       (car vc-svk-co-paths))))
        ;; (re)load
        (setq vc-svk-co-paths (list mtime))
        (with-temp-buffer
          (insert-file-contents config)
          (when (search-forward "hash:\n" nil t) ; to start of co paths
            (while (re-search-forward               ; to next co path
                    "^ +\\(/.*\\):\n.*depotpath: \\(/.+\\)$" nil t)
              (add-to-list 'vc-svk-co-paths
                           (list (match-string-no-properties 1)
                                 (match-string-no-properties 2))))))
        (setq vc-svk-co-paths (nreverse vc-svk-co-paths))))
  vc-svk-co-paths)

;; These will often avoid slow calls to `vc-svk-command'.
(defun vc-svk-co-path-p (file)
  "Whether SVK manages a parent directory of FILE.
Note that this does not try to guarantee SVK manages this particular
subdirectory. That's for the full `vc-svk-registered' to decide."
  (vc-svk-co-paths)
  (block nil
    (unless (file-exists-p file)
      (return nil))
    ;; Check file and each parent dir for svk-ness
    ;; Yeah, this is not the greatest. And it's UNIX-centric.
    (while (and file (not (string-equal file "/")))
      ;; For both SVK and file-name-directory, dirnames must not
      ;; include trailing /
      (setq file (substring file 0 (string-match "/\\'" file)))
      (if (vc-svk-assoc-string file vc-svk-co-paths)
          (return t)
        (setq file (file-name-directory file))))))

(defun vc-svk-co-path-of (file)
  "Return the CO path holding FILE, or nil."
  (car (find-if #'(lambda (codir)
                    (and (stringp codir)
                         (string-match (concat "^" codir) file)))
                vc-svk-co-paths
                :key 'first)))

(provide 'vc-svk)

;;; Local Variables:
;;; indent-tabs-mode: nil
;;; End:
;;; vc-svk.el ends here

