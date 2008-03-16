;;; nxml-where.el --- Show XML path
;;
;; Author: Lennart Borgman
;; Maintainer:
;; Created: Tue Dec 19 14:59:01 2006
(defconst nxml-where:version "0.52");; Version:
;; Lxast-Updated: Thu Mar 01 23:16:35 2007 (3600 +0100)
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

;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

(eval-when-compile
  (require 'cl)
  (unless (featurep 'nxml-nxhtml-autostart)
    (let ((efn (expand-file-name "../autostart.el")))
      (load efn))
    (require 'nxml-mode)))

(defvar nxml-where-once-update-timer nil)
(make-variable-buffer-local 'nxml-where-once-update-timer)
(put 'nxml-where-once-update-timer 'permanent-local t)

(defvar nxml-where-ovls nil)
(make-variable-buffer-local 'nxml-where-ovls)
(put 'nxml-where-ovls 'permanent-local t)

(defun nxml-where-cancel-once ()
  (when (timerp nxml-where-once-update-timer)
    (cancel-timer nxml-where-once-update-timer)
    (setq nxml-where-once-update-timer nil)))

(defun nxml-where-update-once ()
  (condition-case err
      (progn
        (nxml-where-cancel-once)
        (setq nxml-where-once-update-timer
              (run-with-idle-timer idle-update-delay nil
                                   'nxml-where-update
                                   (current-buffer))))
    (error
     (message "%s" (error-message-string err)))))
(put 'nxml-where-update-once 'permanent-local-hook t)

(defun nxml-where-stop-updating ()
  (remove-hook 'post-command-hook 'nxml-where-update-once t)
  (nxml-where-update-once))

(defun nxml-where-restart-updating ()
  (nxml-where-update-once)
  (add-hook 'post-command-hook 'nxml-where-update-once nil t))

(defgroup nxml-where nil
  "Customization group for nxml-where."
  :group 'nxhtml
  :group 'nxml)

(define-toggle nxml-where-tag+id t
  "Show tags + id in path if non-nil.
If nil show only tag names."
  :group 'nxml-where)

(define-toggle nxml-where-header t
  "Show header with XML-path if non-nil."
  :set (lambda (sym val)
         (set-default sym val)
         (nxml-where-update-once))
  :group 'nxml-where)

(define-toggle nxml-where-marks t
  "Show marks in buffer for XML-path if non-nil."
  :set (lambda (sym val)
         (set-default sym val)
         (nxml-where-update-once))
  :group 'nxml-where)

(defface nxml-where-marking
  '((t (:inherit secondary-selection)))
  "The default face used for marking tags in path."
  :group 'nxml-where)

(defcustom nxml-where-marking 'nxml-where-marking
  "Variable pointing to the face used for marking tags in path."
  :type 'face
  :group 'nxml-where)

(defcustom nxml-where-header-attributes '("id" "name")
  "List of attributes `nxml-where-header' should display."
  :type '(repeat string)
  :group 'nxml-where)

(defcustom nxml-where-widen t
  "If non-nil and narrowed widen before getting XML path."
  :type 'boolean
  :group 'nxml-where)


(defvar nxml-where-saved-header-line-format nil)
(make-variable-buffer-local 'nxml-where-saved-header-line-format)
(put 'nxml-where-saved-header-line-format 'permanent-local t)

(defun nxml-where-save-header-line-format ()
  (unless nxml-where-saved-header-line-format
    (setq nxml-where-saved-header-line-format header-line-format)))

(defun nxml-where-restore-header-line-format ()
  (setq header-line-format nxml-where-saved-header-line-format))

(defvar nxml-where-modes '(nxml-mode nxhtml-mode))

(defun nxml-where-is-nxml ()
  (or (derived-mode-p 'nxml-mode)
      (and (featurep 'mumamo)
           mumamo-multi-major-mode
           (let ((major-mode (mumamo-main-major-mode)))
             (derived-mode-p 'nxml-mode)))))

(defun nxml-where-mode-start ()
  (unless (nxml-where-is-nxml)
    (error "Can't display XML path since major mode is not nxml-mode child."))
  (add-hook 'after-change-major-mode-hook 'nxml-where-turn-off-unless-nxml nil t)
  (nxml-where-save-header-line-format)
  (nxml-where-restart-updating))

(defun nxml-where-mode-stop ()
  (nxml-where-stop-updating)
  (nxml-where-restore-header-line-format)
  (dolist (o nxml-where-ovls)
    (delete-overlay o)))

(defun nxml-where-turn-off-unless-nxml ()
  (unless (nxml-where-is-nxml)
    (nxml-where-mode-stop)))
(put 'nxml-where-turn-off-unless-nxml 'permanent-local-hook t)

(define-minor-mode nxml-where-mode
  "Shows path in mode line."
  :global nil
  :group 'nxml-where
  (if nxml-where-mode
      ;;Turn it on
      (nxml-where-mode-start)
    ;; Turn it off
    (nxml-where-mode-stop)
    ))
(put 'nxml-where-mode 'permanent-local t)

(defun nxml-where-turn-on-in-nxml-child ()
  "Turn on `nxml-where-mode' if possible.
This is possible if `major-mode' in the buffer is derived from
`nxml-mode'."
  (when (derived-mode-p 'nxml-mode)
    (nxml-where-mode 1)))

(define-globalized-minor-mode nxml-where-global-mode nxml-where-mode
  nxml-where-turn-on-in-nxml-child
  :group 'nxml-where)
;; The problem with global minor modes:
(when (and nxml-where-global-mode
           (not (boundp 'define-global-minor-mode-bug)))
  (nxml-where-global-mode 1))

(defun nxml-where-update (buffer)
  (if nxml-where-widen
      (save-restriction
        (widen)
        (nxml-where-update-1 buffer))
    (nxml-where-update-1 buffer)))

(defun nxml-where-update-1 (buffer)
  (with-current-buffer buffer
    (unless nxml-where-marks
      (when nxml-where-ovls
        (dolist (o nxml-where-ovls)
          (delete-overlay o))
        (setq nxml-where-ovls nil)))
    (unless nxml-where-header
      (setq header-line-format nil))
    (and nxml-where-mode
         (or nxml-where-header nxml-where-marks)
         (condition-case info
             (let ((current (nxml-where)))
               (when nxml-where-header
                 (setq header-line-format current)))
           (error
            ;;(nxml-where-mode 0)
            (message "Error in nxml-where-update: %s" info)
            (sit-for 2))))))

(defvar nxml-where-get-id-pattern
         (rx space
             (eval (cons 'or nxml-where-header-attributes))
             (0+ space)
             ?=
             (0+ space)
             ?\"
             (0+ (not (any ?\")))
             ?\"
             ))

(defvar nxml-where-tag+id-pattern
         ;;(insert ;; -------------------
          (rx ?<
              (submatch
               (1+ (char "-a-z0-9:"))
               )
              (0+ (1+ space)
                  (1+ (any "a-z"))
                  (0+ space)
                  ?=
                  (0+ space)
                  ?\"
                  (0+ (not (any ?\")))
                  ?\"
                  )
              (0+ space)
              (opt ?/)
              ?>)
          ;;) ;; -------------------
         )

(defun nxml-where ()
  (let (path
        start
        end
        (ovls nxml-where-ovls)
        ovl
        )
    (save-excursion
      (catch 'err
        (while (> (point) (point-min))
          (condition-case err
              (progn
                (nxml-backward-up-element)
                (save-match-data
                  (unless (looking-at nxml-where-tag+id-pattern)
                    (throw 'err "at top"))
                  (setq start (point))
                  (setq end (match-end 0))
                  (let ((tag (match-string-no-properties 1))
                        (all (match-string-no-properties 0)))
                    (when nxml-where-tag+id
                      (when (string-match nxml-where-get-id-pattern all)
                        (setq tag (concat tag (match-string 0 all))))
                      (setq tag (concat "<" tag ">")))
                    (setq path (cons tag path)))
                  (if nxml-where-marks
                    (progn
                      (setq ovl (car ovls))
                      (setq ovls (cdr ovls))
                      (unless ovl
                        (setq ovl (make-overlay start end))
                        (overlay-put ovl 'face nxml-where-marking)
                        (setq nxml-where-ovls (cons ovl nxml-where-ovls)))
                      (unless (and (eq (overlay-buffer ovl) (current-buffer))
                                   (= start (overlay-start ovl))
                                   (= end   (overlay-end   ovl)))
                        (move-overlay ovl start end)))
                    )))
            (error
             ;;(message "nxml-where error: %S" err)
             (throw 'err "uh?"))))))
    (dolist (o ovls)
      (delete-overlay o))
    (unless path
      (setq path (list (if (looking-at "[[:space:]]*\\'")
                           "(After last tag)"
                         "(Before first tag)"))))
    ;; Throw away <html>
    (if (null path)
        (setq path " *Error* ")
      (let* ((first (car path))
             (html "<html")
             (hlen (length html)))
        (when (and (> (length first) hlen)
                   (string= html (substring first 0 hlen)))
          (setq path (cdr path))))
      (unless path
        (setq path (list "(At html start)"))))
    (let* ((sp (substring (format "%s" path) 1 -1))
           (label " Path: ")
           (totlen (+ (length sp) (length label))))
      (when (> totlen (window-width))
        (setq sp (concat "... " (substring sp (+ (- totlen (window-width)) 4)))))
      (concat label sp))))



(provide 'nxml-where)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nxml-where.el ends here
