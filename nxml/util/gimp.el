;;; gimp.el --- Edit files with GIMP
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: Wed May 23 14:59:50 2007
(defconst gimp:version "0.2") ;;Version:
;; Last-Updated:
;; URL:
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
;; Simple interface to start editing with GIMP.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'w32-regdat nil t)

(defcustom gimp-command-list
  (when (featurep 'w32-regdat)
    (save-match-data
      (let ((cmd (w32-regdat-gimp-win-remote-cmd))
            cmd-list)
        (while (< 0 (length cmd))
          (cond
           ((or (string-match (rx string-start
                                   ?\"
                                  (submatch
                                   (0+ (not (any ?\"))))
                                   ?\"
                                  (0+ space))
                              cmd)
                (string-match (rx string-start
                                  (submatch
                                   (0+ (not (any space))))
                                  (0+ space))
                              cmd))
            (setq cmd-list (cons (match-string-no-properties 1 cmd) cmd-list))
            (setq cmd (substring cmd (match-end 0)))
            )
          ))
        (reverse (cdr cmd-list)))))
  "List of values to use when calling GIMP.
The car of this list should be the full path to the program used
when opening files with GIMP. The rest should be any arguments to
use before the file argument.

Example:

  I currently use this value on MS Windows.

  \(\"C:\\Program Files\\GIMP-2.0\\bin\\gimp-win-remote.exe\" \"gimp-2.2.exe\")
"
  :group 'gimp)

(defun gimp-edit-file (image-file)
  "Edit IMAGE-FILE with GIMP."
  (interactive "fImage to edit in GIMP: ")
  (apply 'call-process (car gimp-command-list)
         nil
         0
         nil
         (reverse (cons image-file (reverse (cdr gimp-command-list)))))
  (let ((msg " Asked GIMP to open %s "))
    (put-text-property 0 (length msg) 'face 'highlight msg)
    (message msg image-file)))

(defun gimp-edit-buffer ()
  "Edit image file in current buffer with GIMP."
  (interactive)
  (unless (buffer-file-name)
    (error "Can't edit in GIMP because this buffer does not have a file name."))
  (gimp-edit-file (buffer-file-name)))

(provide 'gimp)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; gimp.el ends here
