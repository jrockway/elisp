;;; ourcomments-util.el --- Utility routines
;;
;; Author: Lennart Borgman <lennart dot borgman at gmail dot com>
;; Created: Wed Feb 21 23:19:07 2007
(defconst ourcomments-util:version "0.21") ;;Version:
;; Last-Updated: Wed Aug 29 17:34:39 2007 (7200 +0200)
;; Keywords:
;; Compatibility: Emacs 22
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; The functionality given by these small routines should in my
;; opinion be part of Emacs (but they are not that currently).
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Popups etc.

(defun point-to-coord (point)
  "Return coordinates of point in selected window.
The coordinates are in the form \(\(XOFFSET YOFFSET) WINDOW)."
  (let* ((pn (posn-at-point point))
         (x-y (posn-x-y pn))
         (x (car x-y))
         (y (cdr x-y))
         (pos (list (list x (+ y 20)) (selected-window))))
    pos))

(defun popup-menu-at-point (menu &optional prefix)
  "Popup the given menu at point.
This is similar to `popup-menu' and MENU and PREFIX has the same
meaning as there.  The position for the popup is however where
the window point is."
  (let ((where (point-to-coord (point))))
    (popup-menu menu where prefix)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Toggles in menus

(defmacro define-toggle (symbol value doc &rest args)
  "Declare SYMBOL as a customizable variable with a toggle function.
The purpose of this macro is to define a defcustom and a toggle
function suitable for use in a menu.

The arguments have the same meaning as for `defcustom' with these
restrictions:

- The :type keyword cannot be used.  Type is always 'boolean.
- VALUE must be t or nil.

A `defcustom' named SYMBOL and a function named SYMBOL-toggle is
defined.  The function toggles the value of SYMBOL.  It takes no
parameters.

To create a menu item something similar to this can be used:

    \(define-key map [SYMBOL]
      \(list 'menu-item \"Toggle nice SYMBOL\"
            'SYMBOL-toggle
            :button '(:toggle . SYMBOL)))"
  (declare (doc-string 3))
  (list
   'progn
   (let ((var-decl (list 'custom-declare-variable
                         (list 'quote symbol)
                         (list 'quote value)
                         doc)))
     (while args
       (let ((arg (car args)))
         (setq args (cdr args))
         (unless (symbolp arg)
           (error "Junk in args %S" args))
         (let ((keyword arg)
               (value (car args)))
           (unless args
             (error "Keyword %s is missing an argument" keyword))
           (setq args (cdr args))
           (cond ((not (memq keyword '(:type)))
                  (setq var-decl (append var-decl (list keyword value))))
                 (t
                  (lwarn '(define-toggle) :error "Keyword %s can't be used here" keyword))))))
     (when (assoc :type var-decl) (error ":type is set. Should not happen!"))
     (setq var-decl (append var-decl (list :type '(quote boolean))))
     var-decl)
   (let* ((SYMBOL-toggle (intern (concat (symbol-name symbol) "-toggle")))
          (SYMBOL-name (symbol-name symbol))
          (fun-doc (concat "Toggles the \(boolean) value of `"
                           SYMBOL-name
                           "'.\n"
                           "For how to set it permanently see this variable.\n"
                           ;;"\nDescription of `" SYMBOL-name "':\n" doc
                           )))
     `(defun ,SYMBOL-toggle ()
        ,fun-doc
        (interactive)
        (customize-set-variable (quote ,symbol) (not ,symbol)))
     )))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Indentation of regions

;; From an idea by weber <hugows@gmail.com>
(defun indent-line-or-region ()
  "Indent line or region.
Only do this if indentation seems bound to \\t.

Call `indent-region' if region is active, otherwise
`indent-according-to-mode'."
  (interactive)
  ;; Do a wild guess if we should indent or not ...
  (let* ((indent-region-mode)
         ;; The above hides the `indent-line-or-region' binding
         (t-bound (key-binding [?\t])))
    (if (not
         (save-match-data
           (string-match "indent" (symbol-name t-bound))))
        (call-interactively t-bound t)
      (if (and mark-active ;; there is a visible region selected
               transient-mark-mode)
          (indent-region (region-beginning) (region-end))
        (indent-according-to-mode))))) ;; indent line

;; (define-minor-mode indent-region-mode
;;   "Use \\t to indent line or region.
;; The key \\t is bound to `indent-line-or-region' if this mode is
;; on."
;;   :global t
;;   :keymap '(([?\t] . indent-line-or-region)))
;; (when indent-region-mode (indent-region-mode 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Minor modes

(defmacro define-globalized-minor-mode-with-on-off (global-mode mode turn-on turn-off &rest keys)
  "Make a global mode GLOBAL-MODE corresponding to buffer-local minor MODE.
This is a special variant of `define-globalized-minor-mode' for
mumamo.  It let bounds the variable GLOBAL-MODE-checking before
calling TURN-ON or TURN-OFF.

TURN-ON is a function that will be called with no args in every buffer
  and that should try to turn MODE on if applicable for that buffer.
TURN-OFF is a function that turns off MODE in a buffer.
KEYS is a list of CL-style keyword arguments.  As the minor mode
  defined by this function is always global, any :global keyword is
  ignored.  Other keywords have the same meaning as in `define-minor-mode',
  which see.  In particular, :group specifies the custom group.
  The most useful keywords are those that are passed on to the
  `defcustom'.  It normally makes no sense to pass the :lighter
  or :keymap keywords to `define-globalized-minor-mode', since these
  are usually passed to the buffer-local version of the minor mode.

If MODE's set-up depends on the major mode in effect when it was
enabled, then disabling and reenabling MODE should make MODE work
correctly with the current major mode.  This is important to
prevent problems with derived modes, that is, major modes that
call another major mode in their body."

  (let* ((global-mode-name (symbol-name global-mode))
         (pretty-name (easy-mmode-pretty-mode-name mode))
         (pretty-global-name (easy-mmode-pretty-mode-name global-mode))
         (group nil)
         (extra-keywords nil)
         (MODE-buffers (intern (concat global-mode-name "-buffers")))
         (MODE-enable-in-buffers
          (intern (concat global-mode-name "-enable-in-buffers")))
         (MODE-check-buffers
          (intern (concat global-mode-name "-check-buffers")))
         (MODE-cmhh (intern (concat global-mode-name "-cmhh")))
         (MODE-major-mode (intern (concat (symbol-name mode) "-major-mode")))
         (MODE-checking (intern (concat global-mode-name "-checking")))
         keyw)

    ;; Check keys.
    (while (keywordp (setq keyw (car keys)))
      (setq keys (cdr keys))
      (case keyw
        (:group (setq group (nconc group (list :group (pop keys)))))
        (:global (setq keys (cdr keys)))
        (t (push keyw extra-keywords) (push (pop keys) extra-keywords))))

    (unless group
      ;; We might as well provide a best-guess default group.
      (setq group
            `(:group ',(intern (replace-regexp-in-string
                                "-mode\\'" "" (symbol-name mode))))))

    `(progn

       ;; Define functions for the global mode first so that it can be
       ;; turned on during load:

       ;; List of buffers left to process.
       (defvar ,MODE-buffers nil)

       ;; The function that calls TURN-ON in each buffer.
       (defun ,MODE-enable-in-buffers ()
         (let ((,MODE-checking nil))
           (dolist (buf ,MODE-buffers)
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (if ,mode
                     (unless (eq ,MODE-major-mode major-mode)
                       (setq ,MODE-checking t)
                       (,mode -1)
                       (,turn-on)
                       (setq ,MODE-checking nil)
                       (setq ,MODE-major-mode major-mode))
                   (setq ,MODE-checking t)
                   (,turn-on)
                   (setq ,MODE-checking nil)
                   (setq ,MODE-major-mode major-mode)))))))
       (put ',MODE-enable-in-buffers 'definition-name ',global-mode)

       (defun ,MODE-check-buffers ()
         (,MODE-enable-in-buffers)
         (setq ,MODE-buffers nil)
         (remove-hook 'post-command-hook ',MODE-check-buffers))
       (put ',MODE-check-buffers 'definition-name ',global-mode)

       ;; The function that catches kill-all-local-variables.
       (defun ,MODE-cmhh ()
         (add-to-list ',MODE-buffers (current-buffer))
         (add-hook 'post-command-hook ',MODE-check-buffers))
       (put ',MODE-cmhh 'definition-name ',global-mode)


       (defvar ,MODE-major-mode nil)
       (make-variable-buffer-local ',MODE-major-mode)

       ;; The actual global minor-mode
       (define-minor-mode ,global-mode
         ,(format "Toggle %s in every possible buffer.
With prefix ARG, turn %s on if and only if ARG is positive.
%s is enabled in all buffers where `%s' would do it.
See `%s' for more information on %s."
                  pretty-name pretty-global-name pretty-name turn-on
                  mode pretty-name)
         :global t ,@group ,@(nreverse extra-keywords)

         ;; Setup hook to handle future mode changes and new buffers.
         (if ,global-mode
             (progn
               (add-hook 'after-change-major-mode-hook
                         ',MODE-enable-in-buffers)
               ;;(add-hook 'find-file-hook ',MODE-check-buffers)
               (add-hook 'find-file-hook ',MODE-cmhh)
               (add-hook 'change-major-mode-hook ',MODE-cmhh))
           (remove-hook 'after-change-major-mode-hook ',MODE-enable-in-buffers)
           ;;(remove-hook 'find-file-hook ',MODE-check-buffers)
           (remove-hook 'find-file-hook ',MODE-cmhh)
           (remove-hook 'change-major-mode-hook ',MODE-cmhh))

         ;; Go through existing buffers.
         (let ((,MODE-checking t))
           (dolist (buf (buffer-list))
             (with-current-buffer buf
               ;;(if ,global-mode (,turn-on) (when ,mode (,mode -1)))
               (if ,global-mode (,turn-on) (,turn-off))
               ))))

       )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Unfilling
;;
;; The idea is from
;;   http://interglacial.com/~sburke/pub/emacs/sburke_dot_emacs.config

(defun unfill-paragraph ()
  "Unfill the current paragraph."
  (interactive) (with-unfilling 'fill-paragraph))
;;(defalias 'unwrap-paragraph 'unfill-paragraph)

(defun unfill-region ()
  "Unfill the current region."
  (interactive) (with-unfilling 'fill-region))
;;(defalias 'unwrap-region 'unfill-region)

(defun unfill-individual-paragraphs ()
  "Unfill individual paragraphs in the current region."
  (interactive) (with-unfilling 'fill-individual-paragraphs))
;;(defalias 'unwrap-individual-paragraphs 'unfill-individual-paragraphs)

(defun with-unfilling (fn)
  (let ((fill-column 10000000)) (call-interactively fn)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Widgets


;; (major-modep 'nxhtml-mode)
(defun major-modep (value)
  (let ((sym-name (symbol-name value)))
    ;; Do some reasonable test to find out if it is a major mode.
    ;; Load autoloaded mode functions.
    ;;
    ;; Fix-me: Maybe test for minor modes? How was that done?
    (when (and (fboundp value)
               (not (memq value '(flyspell-mode
                                  isearch-mode
                                  savehist-mode
                                  )))
               (< 5 (length sym-name))
               (string= "-mode" (substring sym-name (- (length sym-name) 5)))
               (if (and (listp (symbol-function value))
                        (eq 'autoload (car (symbol-function value))))
                   (progn
                     (message "loading ")
                     (load (cadr (symbol-function value)) t t))
                 t)
               (or (memq value
                         ;; Fix-me: Complement this table of known major modes:
                         '(fundamental-mode
                           xml-mode
                           nxml-mode
                           nxhtml-mode
                           css-mode
                           javascript-mode
                           php-mode
                           ))
                   (and (intern-soft (concat sym-name "-hook"))
                        (boundp (intern-soft (concat sym-name "-hook"))))
                   (progn (message "Not a major mode: %s" value)
                          ;;(sit-for 4)
                          nil)
                   ))
      t)))

(define-widget 'major-mode-function 'function
  "A major mode lisp function."
  :complete-function (lambda ()
                       (interactive)
                       (lisp-complete-symbol 'major-modep))
  :prompt-match 'major-modep
  :prompt-history 'widget-function-prompt-value-history
  :match-alternatives '(major-modep)
  :validate (lambda (widget)
              (unless (major-modep (widget-value widget))
                (widget-put widget :error (format "Invalid function: %S"
                                                  (widget-value widget)))
                widget))
  :value 'fundamental-mode
  :tag "Major mode function")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Lines

;; Changed from move-beginning-of-line to beginning-of-line to support
;; physical-line-mode.
(defun ourcomments-move-beginning-of-line(arg)
  "Move point to beginning of line or indentation.
See `beginning-of-line' for ARG.

If `physical-line-mode' is on then the visual line beginning is
first tried."
  (interactive "p")
  (let ((pos (point)))
    (call-interactively 'beginning-of-line arg)
    (when (= pos (point))
      (skip-chars-forward " \t"))))
(put 'ourcomments-move-beginning-of-line 'CUA 'move)

(defun ourcomments-move-end-of-line(arg)
  "Move point to end of line or indentation.
See `end-of-line' for ARG.

If `physical-line-mode' is on then the visual line ending is
first tried."
  (interactive "p")
  (or arg (setq arg 1))
  (let ((pos (point)))
    (call-interactively 'end-of-line arg)
    (when (= pos (point))
      (skip-chars-backward " \t"))))
(put 'ourcomments-move-end-of-line 'CUA 'move)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Misc.

(defun buffer-narrowed-p ()
  "Return non-nil if the current buffer is narrowed."
  (/= (buffer-size)
      (- (point-max)
         (point-min))))

(defadvice ido-mode (after
                     ourcomments-util-ido-add-ctrl-tab)
  "Add C-tab to ido buffer completion."
  (when (memq ido-mode '(both buffer))
    (let ((the-ido-minor-map (cdr ido-minor-mode-map-entry)))
      (define-key the-ido-minor-map [(control tab)] 'ido-switch-buffer))
    (define-key ido-buffer-completion-map [(control tab)] 'ido-next-match)
    (define-key ido-buffer-completion-map [(control shift tab)]
      'ido-prev-match))
  ad-return-value)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; New Emacs instance

(defun emacs()
  "Start a new Emacs."
  (interactive)
  (call-process (concat exec-directory "emacs") nil 0 nil)
  (message "Started 'emacs' - it will be ready soon ..."))

(defun emacs-buffer-file()
  "Start a new Emacs showing current buffer file."
  (interactive)
  (let ((file (buffer-file-name)))
    (unless file (error "No buffer file name"))
    (call-process (concat exec-directory "emacs") nil 0 nil file)
    (message "Started 'emacs buffer-file-name' - it will be ready soon ...")))

(defun emacs--debug-init()
  (interactive)
  (call-process (concat exec-directory "emacs") nil 0 nil "--debug-init")
  (message "Started 'emacs --debug-init' - it will be ready soon ..."))

(defun emacs--disable-font-backend () (interactive)
  "Start new Emacs with font backend disabled.
Stop server in this instant and start it in the new Emacs instead."
  (server-mode -1)
  (setenv "EMACSCLIENT_STARTING_SERVER" "yes")
  (call-process (concat exec-directory "emacs") nil 0 nil "--disable-font-backend" "-l" "auto-server")
  (message "Started 'emacs --disable-font-backend -l auto-server' - it will be ready soon\n ... killing this Emacs in 9 seconds")
  (sit-for 9)
  (kill-emacs))

(defun emacs-Q()
  "Start new Emacs without any customization whatsoever."
  (interactive)
  (call-process (concat exec-directory "emacs") nil 0 nil "-Q")
  (message "Started 'emacs -Q' - it will be ready soon ..."))


(provide 'ourcomments-util)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ourcomments-util.el ends here
