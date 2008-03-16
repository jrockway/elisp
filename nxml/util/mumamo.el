;;; mumamo.el --- Multiple major modes in a buffer
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Maintainer:
;; Created: Fri Mar 09 18:15:25 2007
(defconst mumamo:version "0.74") ;;Version:
;; Last-Updated: 2008-03-10T08:08:11+0100 Mon
;; URL: http://OurComments.org/Emacs/Emacs.html
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   `cl', `sgml-mode'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;; Commentary:
;;
;; In some cases you may find that it is quite hard to write one major
;; mode that does everything for the type of file you want to handle.
;; That is the case for example for a PHP file where there comes
;; useful major modes with Emacs for the html parts, and where you can
;; get a major mode for PHP from other sources (see EmacsWiki for
;; Aaron Hawleys php-mode.el, or the very similar version that comes
;; with nXhtml).
;;
;; Using one major mode for the HTML part and another for the PHP part
;; sounds like a good solution.  But this means you want to use (at
;; least) two major modes in the same buffer.
;;
;; This file implements just that, support for MUltiple MAjor MOdes
;; (mumamo) in a buffer.
;;
;;
;;;; Usage:
;;
;; The multiple major mode support is turned on by calling special
;; functions which are used nearly the same way as major modes.  See
;; `mumamo-defined-turn-on-functions' for more information about those
;; functions.
;;
;; Each such function defines how to take care of a certain mix of
;; major functions in the buffer. We call them "multi major modes".
;;
;; You may call those functions directly (like you can with major mode
;; functions) or you may use them in for example `auto-mode-alist'.
;;
;; You can load mumamo in your .emacs with
;;
;;   (require 'mumamo)
;;
;; or you can generate an autoload file.
;;
;; Note however that no multi major mode functions are defined in this
;; file.  Together with this file comes the file mumamo-fun.el that
;; defines some such functions.  All of such functions defined in that
;; file are marked for autoload.
;;
;;
;;
;; Thanks to Stefan Monnier for beeing a good and knowledgeable
;; speaking partner for some difficult parts while I was trying to
;; develop this.
;;
;; Thanks to RMS for giving me support and ideas about the programming
;; interface.  That simplified the code and usage quite a lot.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;; How to add support for a new mix of major modes
;;
;; This is done by creating a new function using
;; `define-mumamo-multi-major-mode'.  See that function for more information.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;; Information for major modes authors
;;
;; There are a few special requirements on major modes to make them
;; work with mumamo:
;;
;; - fontification-functions should be '(jit-lock-function). However
;;   nxml-mode derivates can work too, see the code for more info.
;;
;; - narrowing should be respected during fontification and
;;   indentation when font-lock-dont-widen is non-nil.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;; Information for minor modes authors
;;
;; Some minor modes are written to be specific for the file edited in
;; the buffer and some are written to be specific for a major
;; modes.  Others are emulating another editor.  Those are probably
;; global, but might still have buffer local values.
;;
;; Those minor modes that are not meant to be specific for a major
;; mode should probably survive changing major mode in the
;; buffer.  That is mostly not the case in Emacs today.
;;
;; There are (at least) two type of values for those minor modes that
;; sometimes should survive changing major mode: buffer local
;; variables and functions added locally to hooks.
;;
;; * Some buffer local variables are really that - buffer local. Other
;;   are really meant not for the buffer but for the major mode or
;;   some minor mode that is local to the buffer.
;;
;;   If the buffer local variable is meant for the buffer then it is
;;   easy to make them survive changing major mode: just add
;;
;;    (put 'VARIABLE 'permanent-local t)
;;
;;   to those variables.  That will work regardless of the way major
;;   mode is changed.
;;
;;   If one only wants the variables to survive the major mode change
;;   that is done when moving between chunks with different major
;;   modes then something different must be used.  To make a variable
;;   survive this, but not a major mode change for the whole buffer,
;;   call any of these functions
;;
;;;; PLEASE IGNORE THESE AT THE MOMENT!
;;
(defun add-to-global-list (list-var element &optional append compare-fn)
  (let ((global-val (default-value list-var)))
    (add-to-list 'global-val element append compare-fn)
    (set-default list-var global-val)))

;;     (mumamo-make-variable-buffer-local 'VARIABLE)
(defun mumamo-make-variable-buffer-local (var)
  (add-to-global-list 'mumamo-survive-a-bit-strange var))
(defun mumamo-make-local-variable (var)
  (add-to-list 'mumamo-survive-a-bit-strange var))
;;     (mumamo-make-local-variable 'VARIABLE)
;;
;;   There is yet some other possibilities.  You might want that the
;;   variables retain the values they had when you last time where in
;;   a chunk with the same major mode.  Then use
;;
;;     (mumamo-make-variable-buffer-local-major 'VARIABLE)
(defun mumamo-make-variable-buffer-local-major (var)
  (mumamo-make-variable-buffer-local var))
;;     (mumamo-make-local-major-variable 'VARIABLE)
(defun mumamo-make-local-major-variable (var)
  (mumamo-make-local-variable var))
;;     ;; NOT YET IMPLEMENTED, will fallback to the functions above
;;
;;   If you want the variable to be specific to one particular chunk then use
;;
;;     (mumamo-make-variable-buffer-local-chunk 'VARIABLE)
(defun mumamo-make-variable-buffer-local-chunk (var)
  (mumamo-make-variable-buffer-local-major var))
;;     (mumamo-make-local-chunk-variable 'VARIABLE)
(defun mumamo-make-local-chunk-variable (var)
  (mumamo-make-local-major-variable var))
;;     ;; NOT YET IMPLEMENTED, will fallback to the functions above
;;
;; * It is a bit more problematic with the values entered to local
;;   hooks.  There is currently no mechanism for that in Emacs.  I
;;   have suggested adding such a mechanism, see
;;
;;   http://lists.gnu.org/archive/html/emacs-devel/2007-12/msg00169.html
;;
;;   but it is not yet there (and beside older versions of Emacs will
;;   not have it).
;;
;;   A solution pretty similar to what I suggested will be in next
;;   version of Emacs (version 22.2). I have added a temporary
;;   solution here which does the same thing. To use this you should
;;   do something like this:
;;
;;     (put 'FUNSYM 'permanent-local-hook t)
;;     (add-hook 'HOOKSYM 'FUNSYM nil t)
;;
;;   where HOOKSYM is the hook and FUNSYM is the function.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;; Comments on code etc:
;;
;; This is yet another way to try to get different major modes for
;; different chunks of a buffer to work.  (I borrowed the term "chunk"
;; here from multi-mode.el.)  I am aware of two main previous elisp
;; packages that tries to do this, multi-mode.el and mmm-mode.el.
;; (See http://www.emacswiki.org/cgi-bin/wiki/MultipleModes where
;; there are also some other packages mentioned.)  The solutions in
;; those are a bit different from the approach here.
;;
;; The idea of doing it the way mumamo does it is of course based on a
;; hope that switching major mode in 1 should be quick.  I found that
;; it took from 0 - 62 000 ms, typically 0 - 16 000 ms on a 3ghz cpu.
;; However unfortunately this is not the whole truth.  It could take
;; longer time, depending on what is run in the hooks: The major mode
;; specific hook, `after-change-major-mode-hook' and
;; `change-major-mode-hook'.
;;
;; Because it currently may take long enough time switching major mode
;; when moving between chunks to disturb smooth moving around in the
;; buffer I have added a way to let the major mode switching be done
;; after moving when Emacs is idle.  This is currently the default, but
;; see the custom variable `mumamo-set-major-mode-delay'.
;;
;; Since the intention is to set up the new major mode the same way as
;; it should have been done if this was a major mode for the whole
;; buffer these hooks must be run.  However if this idea is developed
;; further some of the things done in these hooks (like switching on
;; minor modes) could perhaps be streamlined so that switching minor
;; modes off and then on again could be avoided.  In fact there is
;; already tools for this in mumamo.el, see the section below named
;; "Information for minor mode authors".
;;
;; Another problem is that the major modes must use
;; `font-lock-fontify-region-function'.  Currently the only major
;; modes I know that does not do this are `nxml-mode' and its
;; derivatives.
;;
;; The indentation is currently working rather ok, but with the price
;; that buffer modified is sometimes set even though there are no
;; actual changes.  That seems a bit unnecessary and it could be
;; avoided if the indentation functions for the the various major
;; modes were rewritten so that you could get the indentation that
;; would be done instead of actually doing the indentation.  (Or
;; mumamo could do this better, but I do not know how right now.)
;;
;; See also "Known bugs and problems etc" below.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;; Known bugs:
;;
;; - See the various FIX-ME for possible bugs.  See also below.
;;
;;
;;;; Known problems:
;;
;; - There is no way in Emacs to tell a mode not to change
;;   fontification when changing to or from that mode.
;;
;; - The dividing into chunk is not that simple as I first thought.  I
;;   have not gone through the logic of this very carefully.  Perhaps
;;   that is needed.  The current logic is mainly in
;;   `mumamo-get-chunk-at' and `mumamo-find-possible-chunk'.  (Some
;;   other routines tries to behave like `mumamo-find-possible-chunk'
;;   too: `mumamo-chunk-attr=' and `mumamo-easy-make-chunk-fun'.)
;;
;;   Fix-me: One idea that I currently have not used is to check outer
;;   major mode while dividing into chunks.  This could probably be
;;   done with a small change to `mumamo-create-chunk-values-at'.
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;; Change log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;; Code:

(eval-when-compile (require 'cl))
(eval-when-compile (require 'sgml-mode)) ;; For sgml-xml-mode
;; For `define-globalized-minor-mode-with-on-off':
;;(require 'ourcomments-util)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Debugging etc

;; (defun dbg-smarty-err ()
;; ;;   (insert "}{")

;; ;;   (insert "}{")
;; ;;   (backward-char)
;; ;;   (backward-char)
;; ;;   (search-backward "}")

;;   ;; This gives an error rather often, but not always:
;;   (delete-char 3)
;;   (search-backward "}")
;;   )

;; (defun dbg-smarty-err2 ()
;;   (forward-char 5)
;;   (insert "}{")
;;   ;; Start in nxhtml part and make sure the insertion is in smarty
;;   ;; part.  Gives reliably an error if moved backward so point stay in
;;   ;; the new nxhtml-mode part, otherwise not.
;;   ;;
;;   ;; Eh, no.  If chunk family is changed and reset there is no more an
;;   ;; error.
;;   ;;
;;   ;; Seems to be some race condition, but I am unable to understand
;;   ;; how.  I believed that nxml always left in a reliable state.  Is
;;   ;; this a state problem in mumamo or nxml? I am unable to make it
;;   ;; happen again now.
;;   ;;
;;   ;; I saw one very strange thing: The error message got inserted in
;;   ;; the .phps buffer once.  How could this happen? Is this an Emacs
;;   ;; bug? Can't see how this could happen since it is the message
;;   ;; function that outputs the message.  A w32 race condition? Are
;;   ;; people aware that the message queue runs in parallell? (I have
;;   ;; tried to ask on the devel list, but got no answer at that time.)
;;   (backward-char 2)
;;   )


(defmacro mumamo-msgfntfy (format-string &rest args)
  "Give some messages during fontification.
This macro should just do nothing during normal use.  However if
there are any problems you can uncomment one of the lines in this
macro and recompile/reeval mumamo.el to get those messages.

You have to search the code to see where you will get them.  All
uses are in this file.

FORMAT-STRING and ARGS have the same meaning as for the function
`message'."
  ;;(list 'apply (list 'quote 'message) format-string (append '(list) args))
  ;;(list 'apply (list 'quote 'message) (list 'concat "%s: " format-string) (list 'get-internal-run-time) (append '(list) args))
  )
;;(mumamo-msgfntfy "my-format=%s" (get-internal-run-time))

(defvar mumamo-display-error-lwarn nil
  "Set to t to call `lwarn' on fontification errors.
If this is t then `*Warnings*' buffer will popup on fontification
errors.")
(defvar mumamo-display-error-stop nil
  "Set to t to stop fontification on errors.")

(defun mumamo-message-with-face (msg face)
  "Put MSG with face FACE in *Message* buffer."
  (let ((start (+ (with-current-buffer "*Messages*"
                    (point-max))
                  1))
        ;; This is for the echo area:
        (msg-with-face (propertize (format "%s" msg)
                                   'face face)))

    (message "%s" msg-with-face)
    ;; This is for the buffer:
    (with-current-buffer "*Messages*"
      (goto-char (point-max))
      (backward-char)
      (put-text-property start (point)
                         'face face))))

;;(run-with-idle-timer 1 nil 'mumamo-show-report-message)
(defun mumamo-show-report-message ()
  "Tell the user there is a long error message."
  (mumamo-message-with-face
   "MuMaMo error, please look in the *Message* buffer"
    'highlight))

;; (defmacro mumamo-get-backtrace-if-error (bodyform)
;;   "Evaluate BODYFORM, return a list with error message and backtrace.
;; If there is an error in BODYFORM then return a list with the
;; error message and the backtrace as a string.  Otherwise return
;; nil."
;;   `(let* ((debugger
;;            (lambda (&rest debugger-args)
;;              (let ((debugger-ret (with-output-to-string (backtrace))))
;;                ;; I believe we must put the result in a buffer,
;;                ;; otherwise `condition-case' might erase it:
;;                (with-current-buffer (get-buffer-create "TEMP GET BACKTRACE")
;;                  (erase-buffer)
;;                  (insert debugger-ret)))))
;;           (debug-on-error t)
;;           (debug-on-signal t))
;;      (mumamo-condition-case err
;;          (progn
;;            ,bodyform
;;            nil)
;;        (error
;;         (let* ((errmsg (error-message-string err))
;;                (dbg1-ret
;;                 (with-current-buffer
;;                     (get-buffer "TEMP GET BACKTRACE") (buffer-string)))
;;                ;; Remove lines from this routine:
;;                (debugger-lines (split-string dbg1-ret "\n"))
;;                (dbg-ret (mapconcat 'identity (nthcdr 6 debugger-lines) "\n"))
;;                )
;;           (list errmsg (concat errmsg "\n" dbg-ret)))))))

;;(mumamo-display-error 'test-lwarn-type "testing 1=%s, 2=%s" "one" 'two)
(defun mumamo-display-error (lwarn-type format-string &rest args)
  "Display a message plus traceback in the *Message* buffer.
Use this for errors that happen during fontification or when
running a timer.

LWARN-TYPE is used as the type argument to `lwarn' if warnings
are displayed.  FORMAT-STRING and ARGS are used as the
corresponding arguments to `message' and `lwarn'.

All the output from this function in the *Message* buffer is
displayed with the highlight face.  After the message printed by
`message' is traceback from where this function was called.  Note:
There is no error generated, just a traceback that is put in
*Message* as above.

A simple error message is shown by `message'.  A backtrace colored
with the 'highlight face is placed in the message buffer.

If `mumamo-display-error-lwarn' is non-nil, indicate the error by
calling `lwarn'.  This will display the `*Warnings*' buffer and
thus makes it much more easy to spot that there was an error.

If `mumamo-display-error-stop' is non-nil an error that may stop
fontification is raised."

  ;;(message "========== MUMAMO-DISPLAY-ERROR HERE! %s" lwarn-type)
  ;; Warnings are sometimes disturbning, make it optional:
  (when mumamo-display-error-lwarn
    (apply 'lwarn lwarn-type :error format-string args))

  (let ((format-string2 (concat "%s: " format-string))
        (bt (with-output-to-string (backtrace)))
        ;;(start (+ (with-current-buffer "*Messages*" (point-max)) 0))
        )

    ;; Output message together with backtrace:
    ;;(apply 'message format-string2 lwarn-type args)

    ;; Does not work here, why?
    ;;(put-text-property 0 (length bt) 'face 'highlight bt)

    ;; Backtrace to message buffer:
;;     (message "%s" bt)
;;     (with-current-buffer "*Messages*"
;;       (goto-char (point-max))
;;       (backward-char)
;;       (put-text-property start (point)
;;                          'face 'highlight))
    (mumamo-message-with-face
     (concat
      (apply 'format format-string2 lwarn-type args)
      "\n"
      (format "** In buffer %s\n" (current-buffer))
      bt)
     'highlight)

    ;; Output message once again so the user can see it:
    (apply 'message format-string2 lwarn-type args)
    (run-with-idle-timer 1 nil 'mumamo-show-report-message)

    ;; Stop fontifying:
    (when mumamo-display-error-stop
      (apply 'error format-string2 lwarn-type args))))




(defun mumamo-command-error-function (error-data context signaling-function)
  "During fontification `command-error-function' should be this function.
This function displays the error with a backtrace in the message buffer.

For an explanation of ERROR-DATA, CONTEXT and SIGNALING-FUNCTION
see `command-error-function'.

Note: This function is obsolete, it does not work the way I
thought.  I thought this could be used during fontification, but
`command-error-function' is called only in the command loop.  See
`mumamo-debugger' instead."
  (setq command-error-function nil)
  (mumamo-display-error 'mumamo-command-error-function
                        "%s: %s in %s"
                        context
                        (error-message-string error-data)
                        signaling-function))

;; (defun my-err() (let ((x (/ 1 0)))))
;; ;; (let ((command-error-function 'mumamo-command-error-function)) (my-err))
;; (defun my-test-err ()
;;   (interactive)
;;   (setq command-error-function 'mumamo-command-error-function)
;;   (my-err)
;;   (setq command-error-function nil))

;; (defun my-test-err2 ()
;;   (interactive)
;;   (let((command-error-function 'mumamo-command-error-function))
;;     (my-err)))

(defun mumamo-debug-to-backtrace (&rest debugger-args)
  "This function should give a backtrace during fontification errors.
The variable `debugger' should then be this function.  See the
function `debug' for an explanation of DEBUGGER-ARGS.

Fix-me: Can't use this function yet since the display routines
uses safe_eval and safe_call."
  (mumamo-display-error 'mumamo-debug-to-backtrace
                        "%s"
                        (nth 1 debugger-args)))

;; (defun my-test-err3 ()
;;   (interactive)
;;   (let ((debugger 'mumamo-debug-to-backtrace)
;;         (debug-on-error t))
;;     (my-err)
;;     ))
;;(my-test-err3()

;;(set-default 'mumamo-use-condition-case nil)
;;(set-default 'mumamo-use-condition-case t)
(defvar mumamo-use-condition-case t)
(make-variable-buffer-local 'mumamo-use-condition-case)
(put 'mumamo-use-condition-case 'permanent-local t)

(defvar mumamo-debugger 'mumamo-debug-to-backtrace)
(make-variable-buffer-local 'mumamo-debugger)
(put 'mumamo-debugger 'permanent-local t)

(defmacro mumamo-condition-case (var body-form &rest handlers)
  "Like `condition-case', but optional.
If `mumamo-use-condition-case' is non-nil then do

  (condition-case VAR
      BODY-FORM
    HANDLERS).

Otherwise just evaluate BODY-FORM."
  (declare (indent 2) (debug t))
  `(if (not mumamo-use-condition-case)
       (let* ((debugger mumamo-debugger)
              (debug-on-error (if debugger t debug-on-error)))
         ,body-form)
    (condition-case ,var
        ,body-form
      ,@handlers)))

;; (defun my-test-err4 ()
;;   (interactive)
;;   (mumamo-condition-case err
;;       (my-errx)
;;     (arith-error (message "here"))
;;     (error (message "%s, %s" err (error-message-string err)))
;;     ))

(defvar mumamo-warned-once nil)
(make-variable-buffer-local 'mumamo-warned-once)
(put 'mumamo-warned-once 'permanent-local t)

; (append '(0 1) '(a b))
(defun mumamo-warn-once (type message &rest args)
  "Warn only once with TYPE, MESSAGE and ARGS.
If the same problem happens again then do not warn again."
  (let ((msgrec (append (list type message) args)))
    (unless (member msgrec mumamo-warned-once)
      (setq mumamo-warned-once
            (cons msgrec mumamo-warned-once))
      (apply 'lwarn type :warning message args))))

(defun mumamo-add-help-tabs ()
  "Add key bindings for moving between buttons.
Add bindings similar to those in `help-mode' for moving between
text buttons."
  (local-set-key [tab] 'forward-button)
  (local-set-key [(meta tab)] 'backward-button)
  (local-set-key [(shift tab)] 'backward-button))

(defun mumamo-insert-describe-button (symbol type)
  "Insert a text button that describes SYMBOL of type TYPE."
  (let ((func `(lambda (btn)
                 (funcall ',type ',symbol))))
    (mumamo-add-help-tabs)
    (insert-text-button
     (symbol-name symbol)
     :type 'help-function
     'face 'link
     'action func)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Compiler pacifiers

(defvar mumamo-original-syntax-begin-function nil "Silence compiler.")
(make-variable-buffer-local 'mumamo-original-syntax-begin-function)
(defvar mumamo-original-fill-paragraph-function nil "Silence compiler.")
(make-variable-buffer-local 'mumamo-original-fill-paragraph-function)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Custom group

(defgroup mumamo nil
  "Customization group for multiple major modes in a buffer."
  :group 'editing
  :group 'languages
  :group 'sgml
  :group 'nxhtml
  )

;;(setq mumamo-set-major-mode-delay -1)
(defcustom mumamo-set-major-mode-delay idle-update-delay
  "Delay this number of seconds before setting major mode.
When point enters a region where the major mode should be
different than the current major mode, wait until Emacs has been
idle this number of seconds before switching major mode.

If negative switch major mode immediately.

Ideally the switching of major mode should occur immediately when
entering a region.  However this can make movements a bit unsmooth
for some major modes on a slow computer.  Therefore on a slow
computer use a short delay.

If you have a fast computer and want to use mode specific
movement commands then set this variable to -1.

I tried to measure the time for switching major mode in mumamo.
For most major modes it took 0 ms, but for `nxml-mode' and its
derivate it took 20 ms on a 3GHz CPU."
  :type 'number
  :group 'mumamo)

;; FIX-ME: probably usable colors only for light background now
(defface mumamo-background-chunk-submode
  '((((class color) (min-colors 88) (background dark))
     ;;:background "blue3")
     :background "gray34")
    (((class color) (min-colors 88) (background light))
     ;;:background "lightgoldenrod2")
     :background "azure")
    (((class color) (min-colors 16) (background dark))
     :background "blue3")
    (((class color) (min-colors 16) (background light))
     :background "azure")
    (((class color) (min-colors 8))
     :background "blue")
    (((type tty) (class mono))
     :inverse-video t)
    (t :background "gray"))
  "Background colors for chunks in major mode.
You should only specify :background here, otherwise it will
interfere with syntax highlighting.

FIX-ME: background colors for dark cases."
  :group 'mumamo)

(defcustom mumamo-background-chunk-major 'mumamo-background-chunk-major
  "Background colors for chunks in major mode.
Pointer to face with background color.

If you do not want any special background color use the face named
default."
  :type 'face
  :group 'mumamo)

(defface mumamo-background-chunk-major
  '((((class color) (min-colors 88) (background dark))
     ;;:background "blue3")
     :background "gray34")
    (((class color) (min-colors 88) (background light))
     ;;:background "lightgoldenrod2")
     :background "cornsilk")
    (((class color) (min-colors 16) (background dark))
     :background "blue3")
    (((class color) (min-colors 16) (background light))
     :background "cornsilk")
    (((class color) (min-colors 8))
     :background "blue")
    (((type tty) (class mono))
     :inverse-video t)
    (t :background "gray"))
  "Background colors for chunks in sub modes.
You should only specify :background here, otherwise it will
interfere with syntax highlighting.

FIX-ME: background colors for dark cases."
  :group 'mumamo)

(defcustom mumamo-background-chunk-submode 'mumamo-background-chunk-submode
  "Background colors for chunks in sub modes.
Pointer to face with background color.

If you do not want any special background color use the face named
default."
  :type 'face
  :group 'mumamo)

(defcustom mumamo-chunk-coloring 'both-colored
  "What chunks to color."
  :type '(choice (const :tag "Color only submode chunks" submode-colored)
                 (const :tag "No coloring of chunks" no-chunks-colored)
                 (const :tag "Color both submode and main major mode chunks"
                        both-colored))
  :group 'mumamo)

(defcustom mumamo-submode-indent-offset 2
  "Indentation of submode relative main major mode.
See also `mumamo-submode-indent-offset-0'."
  :type 'integer
  :group 'mumamo)

(defcustom mumamo-submode-indent-offset-0 0
  "Indentation of submode at column 0.
This value overrides `mumamo-submode-indent-offset' when the main
major mode above has indentation 0."
  :type 'integer
  :group 'mumamo)

(defcustom mumamo-check-chunk-major-same nil
  "Check if main major mode is the same as normal mode."
  :type 'boolean
  :group 'mumamo)

;; (customize-option 'mumamo-major-modes)
;;(require 'django)
;; Fix-me: Should this really be a defcustom?
(defcustom mumamo-major-modes
  '(
    (php-mode
     php-mode)
    (css-mode
     css-mode)
    (java-mode
     java-mode)
    (ruby-mode
     ruby-mode)
    (perl-mode
     perl-mode)
    (django-mode
     django-mode)
    (asp-js-mode
     javascript-mode)
    (asp-vb-mode
     visual-basic-mode)
    (javascript-mode
     javascript-mode
     ecmascript-mode)
    )
  "Alist for conversion of chunk mode specifier to major mode.
Each entry has the form

  \(MAJOR-SPEC MAJORMODE ...)

where MAJOR-SPEC specifies the code type and should match the
value returned from `mumamo-find-possible-chunk'.  The MAJORMODEs
are major modes that can be used for editing that code type.  The
first available MAJORMODE is the one that is used.

This list is used by the chunk specifications in
`define-mumamo-multi-major-mode'."
;; Fix-me: currently not used for the main major mode there.
  :type '(alist
          :key-type symbol
          :value-type (repeat (choice function symbol))
          )
  :group 'mumamo)

(defcustom mumamo-perl-here-doc-modes
  '(
    ("_HTML_" html-mode)
    )
  "Matches for perl here doc modes.
The entries in this list have the form

  (REGEXP MAJOR-MODE)

where REGEXP is a regular expression that should match the perl
here document marker and MAJOR-MODE is the major mode to use in
the perl here document.

The entries are tried from the beginning until the first match."
  :type '(repeat
          (list
           regexp
           (function :tag "Major mode")))
  :group 'mumamo)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; JIT lock functions

(defun mumamo-jit-lock-function (start)
  "This function is added to `fontification-functions' by mumamo.
START is a parameter given to functions in that hook."
  (mumamo-msgfntfy "mumamo-jit-lock-function %s, ff=%s, just-changed=%s" start (get-text-property start 'fontified) mumamo-just-changed-major)
  (if mumamo-just-changed-major
      (setq mumamo-just-changed-major nil)
    (jit-lock-function start)))

(defun mumamo-jit-lock-register (fun &optional contextual)
  "Replacement for `jit-lock-register'.
Avoids refontification, otherwise same.  FUN and CONTEXTUAL has
the some meaning as there."
  (add-hook 'jit-lock-functions fun nil t)
  (when (and contextual jit-lock-contextually)
    (set (make-local-variable 'jit-lock-contextually) t))

  ;;(jit-lock-mode t)
  ;;
  ;; Replace this with the code below from jit-lock-mode t part:
  (setq jit-lock-mode t)

  ;; Mark the buffer for refontification.
  ;; This is what we want to avoid in mumamo:
  ;;(jit-lock-refontify)

  ;; Install an idle timer for stealth fontification.
  (when (and jit-lock-stealth-time (null jit-lock-stealth-timer))
    (setq jit-lock-stealth-timer
          (run-with-idle-timer jit-lock-stealth-time t
                               'jit-lock-stealth-fontify)))

  ;; Create, but do not activate, the idle timer for repeated
  ;; stealth fontification.
  (when (and jit-lock-stealth-time (null jit-lock-stealth-repeat-timer))
    (setq jit-lock-stealth-repeat-timer (timer-create))
    (timer-set-function jit-lock-stealth-repeat-timer
                        'jit-lock-stealth-fontify '(t)))

  ;; Init deferred fontification timer.
  (when (and jit-lock-defer-time (null jit-lock-defer-timer))
    (setq jit-lock-defer-timer
          (run-with-idle-timer jit-lock-defer-time t
                               'jit-lock-deferred-fontify)))

  ;; Initialize contextual fontification if requested.
  (when (eq jit-lock-contextually t)
    (unless jit-lock-context-timer
      (setq jit-lock-context-timer
            (run-with-idle-timer jit-lock-context-time t
                                 'jit-lock-context-fontify)))
    (setq jit-lock-context-unfontify-pos
          (or jit-lock-context-unfontify-pos (point-max))))

  ;; Setup our hooks.
  ;;(add-hook 'after-change-functions 'jit-lock-after-change t t)
  (add-hook 'after-change-functions 'mumamo-jit-lock-after-change t t)
  ;; Set up fontification to call jit:
  (let ((ff (reverse fontification-functions)))
    (mapc (lambda (f)
            ;;(unless (eq f 'jit-lock-function)
            (remove-hook 'fontification-functions f t))
          ;;)
          ff))
  (add-hook 'fontification-functions 'mumamo-jit-lock-function nil t)
  )

;; From jit-lock.el:
(defmacro mumamo-jit-with-buffer-unmodified (&rest body)
  "Eval BODY, preserving the current buffer's modified state."
  (declare (debug t))
  (let ((modified (make-symbol "modified")))
    `(let ((,modified (buffer-modified-p)))
       (unwind-protect
           (progn ,@body)
         (unless ,modified
           (restore-buffer-modified-p nil))))))

(defmacro mumamo-with-buffer-prepared-for-jit-lock (&rest body)
  "Execute BODY in current buffer, overriding several variables.
Preserves the `buffer-modified-p' state of the current buffer."
  (declare (debug t))
  `(mumamo-jit-with-buffer-unmodified
    (let ((buffer-undo-list t)
          (inhibit-read-only t)
          (inhibit-point-motion-hooks t)
          (inhibit-modification-hooks t)
          deactivate-mark
          buffer-file-name
          buffer-file-truename)
      ,@body)))

;; Fix-me: handle jit-lock-context-unfontify-pos, but how?
(defun mumamo-jit-lock-after-change (min max old-len)
  "Replacement for `jit-lock-after-change'.
Does the nearly the same thing as that function, but takes
care of that there might be different major modes at MIN and MAX.
It also marks for refontification only in the current mumamo chunk.

OLD-LEN is the pre-change length.

Jit-lock after change functions is organized this way:

`jit-lock-after-change' (doc: Mark the rest of the buffer as not
fontified after a change) is added locally to the hook
`after-change-functions'.  This function runs
`jit-lock-after-change-extend-region-functions'."
  (when (and jit-lock-mode (not memory-full))
    (mumamo-msgfntfy "mumamo-jit-lock-after-change %s %s %s" min max len)
    ;; Why is this nil?:
    (mumamo-msgfntfy "  mumamo-jit-lock-after-change: font-lock-extend-after-change-region-function=%s" font-lock-extend-after-change-region-function)
    (let* ((ovl-min (mumamo-get-existing-chunk-at min))
           (ovl-max (when (or (not ovl-min)
                              (< (overlay-end ovl-min) max))
                      (mumamo-get-existing-chunk-at max)))
           (major-min (when ovl-min (mumamo-chunk-major-mode ovl-min)))
           (major-max (when ovl-max (mumamo-chunk-major-mode ovl-max)))
           (r-min nil)
           (r-max nil)
           (new-min min)
           (new-max max))
      (if (and major-min (eq major-min major-max))
          (setq r-min (when major-min (mumamo-jit-lock-after-change-1 min max old-len major-min)))
        (setq r-min (when major-min (mumamo-jit-lock-after-change-1 min max old-len major-min)))
        (setq r-max (when major-max (mumamo-jit-lock-after-change-1 min max old-len major-max))))
      (mumamo-msgfntfy "mumamo-jit-lock-after-change r-min,max=%s,%s major-min,max=%s,%s" r-min r-max major-min major-max)
      (when r-min
        (setq new-min (min new-min (car r-min)))
        (setq new-max (max new-max (cdr r-min))))
      (when r-max
        (setq new-min (min new-min (car r-max)))
        (setq new-max (max new-max (cdr r-max))))
      (setq new-min (max new-min (point-min)))
      (setq new-max (min new-max (point-max)))
;;;     ;; Fix-me: This is a kind of desperate move.  I can not find out
;;;     ;; why comments are not fontified correctly in php-mode when they
;;;     ;; have been changed:
;;;     (let* ((old-point (point))
;;;            (chunk (mumamo-get-existing-chunk-at old-point))
;;;            (major (when chunk (mumamo-chunk-major-mode chunk))))
;;;       (when major
;;;         (goto-char new-min)
;;;         (mumamo-with-major-mode-fontification major (point)
;;;           (progn
;;;             (mumamo-beginning-of-syntax)
;;;             (setq new-min (point))
;;;             (goto-char old-point)))))
      ;; Make sure we change at least one char (in case of deletions).
      (setq new-max (min (max new-max (1+ new-min)) (point-max)))
      (mumamo-msgfntfy "mumamo-jit-lock-after-change new-min,max=%s,%s" new-min new-max)
      (mumamo-mark-for-refontification new-min new-max)

      ;; Fix-me: Should something be changed here due to chunks?
      ;; Mark the change for deferred contextual refontification.
      (when jit-lock-context-unfontify-pos
        (setq jit-lock-context-unfontify-pos
              ;; Here we use `start' because nothing guarantees that the
              ;; text between start and end will be otherwise refontified:
              ;; usually it will be refontified by virtue of being
              ;; displayed, but if it's outside of any displayed area in the
              ;; buffer, only jit-lock-context-* will re-fontify it.
              (min jit-lock-context-unfontify-pos new-min))))))

(defun mumamo-jit-lock-after-change-1 (min max old-len major)
  "Extend the region the same way jit-lock does it.
This function tries to extend the region between MIN and MAX the
same way jit-lock does it after a change.  OLD-LEN is the
pre-change length.

The extending of the region is done as if MAJOR was the major
mode."
  (mumamo-with-major-mode-fontification major min
    (progn
      (let ((jit-lock-start min)
            (jit-lock-end   max))
        (mumamo-msgfntfy "mumamo-mumamo-jit-lock-after-change-1 jlacer=%s" jit-lock-after-change-extend-region-functions)
        (mumamo-with-buffer-prepared-for-jit-lock
         (font-lock-extend-jit-lock-region-after-change min max old-len)
         ;;(run-hook-with-args 'jit-lock-after-change-extend-region-functions min max len)
         ;;(setq jit-lock-end (min (max jit-lock-end (1+ min)) (point-max)))
         )
        (setq min jit-lock-start)
        (setq max jit-lock-end)
        )))
  (cons min max))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Font lock functions

;; Borrowed from font-lock.el
(defmacro mumamo-save-buffer-state (varlist &rest body)
  "Bind variables according to VARLIST and eval BODY restoring buffer state.
Do not record undo information during evaluation of BODY."
  (declare (indent 1) (debug let))
  (let ((modified (make-symbol "modified")))
    `(let* ,(append varlist
                    `((,modified (buffer-modified-p))
                      (buffer-undo-list t)
                      (inhibit-read-only t)
                      (inhibit-point-motion-hooks t)
                      (inhibit-modification-hooks t)
                      deactivate-mark
                      buffer-file-name
                      buffer-file-truename))
       (progn
         ,@body)
       (unless ,modified
         (restore-buffer-modified-p nil)))))

(defun mumamo-mark-for-refontification (min max)
  "Mark region between MIN and MAX for refontification."
  (mumamo-msgfntfy "mumamo-mark-for-refontification A min,max=%s,%s point-min,max=%s,%s" min max (point-min) (point-max))
  ;;(when (< min (point-min)) (setq min (point-min)))
  ;;(when (> max (point-max)) (setq max (point-max)))
  (when (< min max)
    (save-restriction
      (widen)
      (mumamo-msgfntfy "mumamo-mark-for-refontification B min,max=%s,%s point-min,max=%s,%s" min max (point-min) (point-max))
      ;;(lwarn t :warning "mumamo-mark-for-refontification %s %s" min max )
      (mumamo-save-buffer-state nil (put-text-property min max 'fontified nil)))))

;; (defmacro mumamo-save-buffer-state-with-undo (varlist &rest body)
;;   "Bind variables according to VARLIST and eval BODY restoring buffer state.
;; Also record undo information and undo the changes made in BODY
;; immediately."
;; ;; Fix-me: Should not the undo part be moved here???
;; ;; Fix-me: TEST!
;;   (declare (indent 1) (debug let))
;;   (let ((modified (make-symbol "modified")))
;;     `(let* ,(append varlist
;;                     `((,modified (buffer-modified-p))
;;                       (buffer-undo-list nil)
;;                       ;;(inhibit-read-only t)
;;                       ;;(inhibit-point-motion-hooks t)
;;                       ;;(inhibit-modification-hooks t)
;;                       deactivate-mark
;;                       buffer-file-name
;;                       buffer-file-truename))
;;        (progn
;;          ,@body
;;          (undo-start)
;;          (while (and (not (eq t pending-undo-list))
;;                      pending-undo-list)
;;            (undo-more 1))
;;          )
;;        (unless ,modified
;;          (restore-buffer-modified-p nil)))))


(defconst mumamo-internal-major-modes-alist nil
  "Alist with info for different major modes.
Internal use only.  This is automatically set up by
`mumamo-get-major-mode-setup'.")

(defvar mumamo-ppss-flush-last-chunk nil
  "Internal variable used to avoid unnecessary flushing.")
(defvar mumamo-ppss-flush-last-major nil
  "Internal variable used to avoid unnecessary flushing.")
(defsubst mumamo-ppss-flush-cache (beg)
  "Flush the ppss cache from BEG and remember the major mode.
To avoid flushing the ppss cache the value of `major-mode' is saved
and compared with next call."
  ;; Stefan Monnier suggested to flush the ppss cache to try to avoid
  ;; the problem that the border of strings sometimes was negated (so
  ;; that the regions outside strings were supposed to be the strings
  ;; by the font locking functions).  Flushing the cache is indeed the
  ;; cure to many problems I have seen so I have put it here.
  ;;
  ;; It might be worth for performance reason to cache ppss keeping a
  ;; cached copy on each chunk, but I have not tried that.  However I
  ;; doubt it and hope that the test below will work instead.
  ;;
  ;; Note that the flushing only is done when major mode has
  ;; changed.  jit-lock will take care of the rest of the flushing (I
  ;; hope).
  (let ((chunk (mumamo-get-existing-chunk-at beg)))
    (unless (and (eq mumamo-ppss-flush-last-major major-mode)
                 (eq mumamo-ppss-flush-last-chunk chunk))
      ;;(message "mumamo-ppss-flush-last-major %s -> %s" mumamo-ppss-flush-last-major major-mode)
      (setq mumamo-ppss-flush-last-major major-mode)
      (setq mumamo-ppss-flush-last-chunk chunk)
      (syntax-ppss-flush-cache beg) (setq syntax-ppss-last nil))))

;;(mumamo-get-major-mode-substitute 'nxhtml-mode 'fontification)
;;(mumamo-get-major-mode-substitute 'nxhtml-mode 'indentation)
;;(mumamo-get-major-mode-substitute 'css-mode 'fontification)
;;(mumamo-get-major-mode-substitute 'css-mode 'indentation)
;; (assq 'nxml-mode mumamo-major-mode-substitute)
(defconst mumamo-major-mode-substitute
  '(
    (nxhtml-mode (html-mode html-mode))
    (nxml-mode (sgml-mode))
    )
  "Major modes substitute to use for fontification and indentation.
The entries in this list has either of the formats

  \(MAJOR (FONT-MODE INDENT-MODE))
  \(MAJOR (FONT-MODE))

where major is the major mode in a mumamo chunk and FONT-MODE is
the major mode for fontification of that chunk and INDENT-MODE is
dito for indentation.  In the second form the same mode is used
for indentation as for fontification.")

(defun mumamo-get-major-mode-substitute (major for-what)
  "For major mode MAJOR return major mode to use for FOR-WHAT.
FOR-WHAT can be either 'fontification or indentation.

mumamo must handle fontification and indentation for `major-mode'
by using other major mode if the functions for this in
`major-mode' are not compatible with mumamo.  This functions
looks in the table `mumamo-major-mode-substitute' for get major
mode to use."
  (let ((m (assq major mumamo-major-mode-substitute)))
    (if (not m)
        major
      (setq m (nth 1 m) major)
      (cond
       ((eq for-what 'fontification)
        (nth 0 m))
       ((eq for-what 'indentation)
        (nth 1 m))
       (t
        (mumamo-display-error 'mumamo-get-major-mode-substitute
                              "Bad parameter, for-what=%s" for-what))))))

(defmacro mumamo-with-major-mode-setup (major for-what ppss-point &rest body)
  "Run code with some local variables set as in specified major mode.
Set variables as needed for major mode MAJOR when doing FOR-WHAT,
flush ppss cach at PPSS-POINT and then run BODY using
`with-syntax-table'.

FOR-WHAT is used to choose another major mode than MAJOR in
certain cases.  It should be 'fontification or 'indentation.

Note: We must let-bind the variables here instead of make them buffer
local since they otherwise could be wrong at \(point) in top
level \(ie user interaction level)."
  (declare (indent 4) (debug t))
  `(let ((need-major-mode (mumamo-get-major-mode-substitute ,major ,for-what)))
     (mumamo-msgfntfy "mumamo-with-major-mode-setup %s" ,major)
     (mumamo-msgfntfy "mumamo-with-major-mode-setup <<<<<<<<<< body=%S\n>>>>>>>>>>" '(progn ,@body))
     (if nil ;;(eq ,major need-major-mode)
         ;; Fix-me: going this way loops when
         ;; body=(progn (mumamo-do-fontify start end verbose narrow-to-chunk chunk-min chunk-max major))
         (progn
           (mumamo-msgfntfy "mumamo-with-major-mode-setup.major=%s, major-mode=%s, need-major-mode=%s" major major-mode need-major-mode)
           (let ((syntax-begin-function 'mumamo-beginning-of-syntax))
             (mumamo-ppss-flush-cache ,ppss-point)
             (progn ,@body)))
       (let ((major-mode need-major-mode)
             (major-func (mumamo-get-major-mode-setup need-major-mode))

             ;; The let bound variables below should be all those fetched with
             ;; `mumamo-fetch-major-mode-setup':
             (sgml-xml-mode)

             (major-syntax-table)

             (font-lock-syntactic-keywords)
             (font-lock-multiline)
             (font-lock-extend-after-change-region-function)
             (font-lock-keywords-alist)
             (font-lock-extend-region-functions)
             (font-lock-comment-start-skip)
             (font-lock-comment-end-skip)

             (font-lock-mode-major-mode)
             (font-lock-set-defaults)

;;; Set from font-lock-defaults normally:
             ;; Syntactic Font Lock
             (font-lock-keywords-only)
             (font-lock-syntax-table)
             (font-lock-beginning-of-syntax-function)
             ;; Other
             (font-lock-keywords)
             (font-lock-removed-keywords-alist)
             (font-lock-keywords-case-fold-search)
             (font-lock-syntactic-face-function)

             ;; Other Font Lock Variables
             (font-lock-mark-block-function)
             (font-lock-extra-managed-props)
             (font-lock-fontify-buffer-function)
             (font-lock-unfontify-buffer-function)
             (font-lock-fontify-region-function)
             (font-lock-unfontify-region-function)

             ;; Jit Lock Variables
             (jit-lock-after-change-extend-region-functions)

             (mumamo-original-syntax-begin-function)
             (mumamo-original-fill-paragraph-function)

             ;; From cc-mode.el:
             (parse-sexp-ignore-comments)
             (indent-line-function)
             (indent-region-function)
             (normal-auto-fill-function)
             (comment-start)
             (comment-end)
             (comment-start-skip)
             (comment-multi-line)
             (comment-line-break-function)
             (paragraph-start)
             (paragraph-separate)
             (paragraph-ignore-fill-prefix)
             (adaptive-fill-mode)
             (adaptive-fill-regexp)
             )
         (funcall major-func)
         (with-syntax-table major-syntax-table
           (let ((syntax-begin-function 'mumamo-beginning-of-syntax))
             (mumamo-ppss-flush-cache ,ppss-point)
             (progn ,@body)))))))

(defmacro mumamo-with-major-mode-fontification (major ppss-point &rest body)
  "With fontification variables set as in major mode MAJOR run BODY.
This is used during font locking and indentation.  The variables
affecting those are set as they are in major mode MAJOR when BODY
is run.

PPSS-POINT is used to keep track of ppss cache.

See the code in `mumamo-fetch-major-mode-setup' for exactly which
local variables that are set."
  (declare (indent 2) (debug t))
  `(mumamo-with-major-mode-setup ,major 'fontification ,ppss-point ,@body))

(defmacro mumamo-with-major-mode-indentation (major ppss-point &rest body)
  "With indentation variables set as in major mode MAJOR run BODY.
Same as `mumamo-with-major-mode-fontification' but for
indentation."
  (declare (indent 2) (debug t))
  `(mumamo-with-major-mode-setup ,major 'indentation ,ppss-point ,@body))

(defvar mumamo-do-fontify-last-pos nil)
(make-variable-buffer-local 'mumamo-do-fontify-last-pos)
(put 'mumamo-do-fontify-last-pos 'permanent-local t)

(defvar mumamo-do-fontify-last-major nil)
(make-variable-buffer-local 'mumamo-do-fontify-last-major)
(put 'mumamo-do-fontify-last-major 'permanent-local t)

;; Keep this separate for easier debugging.
(defun mumamo-do-fontify (start end verbose narrow-to-chunk chunk-min chunk-max major)
  "Fontify region between START and END.
If VERBOSE is non-nil then print status messages during
fontification.

This function takes care of `font-lock-dont-widen' and
`font-lock-extend-region-functions'.  Normally
`font-lock-default-fontify-region' does this, but that function
is not called when mumamo is used!

NARROW-TO-CHUNK is not used any more, see above.

CHUNK-MIN, CHUNK-MAX and MAJOR are the chunk's min point, max
point and major mode."
  (mumamo-msgfntfy "mumamo-do-fontify <<<<<<< %s %s %s %s %s %s %s" start end verbose narrow-to-chunk chunk-min chunk-max major)
  ;;
  ;; Fix-me: Move this to where a new chunk is started?
  ;;
  ;; If the first char is a " then skip to next character:
  (when (eq ?\" (char-after start))
    ;;(lwarn 't :warning "eq \" at %s" start)
    (setq start (1+ start)))

  ;;(lwarn t :warning "major=%s fld=%s %s %s" major-mode font-lock-set-defaults start end)
  (mumamo-condition-case err
  ;;(condition-case err
      (if (not font-lock-set-defaults)
          ;; Fix-me: maybe wrong place to do it! Must use font-lock for unfontifying to, or?
          (progn
            ;;(lwarn t :warning "major=%s unfontify %s %s" major-mode start end)
            ;;(font-lock-default-unfontify-region start end)
            (put-text-property start end 'face nil)
            )
        (when t
          (let ((font-lock-dont-widen font-lock-dont-widen)
                (font-lock-extend-region-functions nil) ;font-lock-extend-region-functions)
                )
            ;; Extend like in `font-lock-default-fontify-region':
            (let ((funs font-lock-extend-region-functions)
                  (font-lock-beg start)
                  (font-lock-end end))
              (while funs
                (setq funs (if (or (not (funcall (car funs)))
                                   (eq funs font-lock-extend-region-functions))
                               (cdr funs)
                             ;; If there's been a change, we should go through
                             ;; the list again since this new position may
                             ;; warrant a different answer from one of the fun
                             ;; we've already seen.
                             font-lock-extend-region-functions)))
              ;; But we must restrict to the chunk here:
              (let ((new-start (max chunk-min font-lock-beg))
                    (new-end (min chunk-max font-lock-end)))
                (mumamo-msgfntfy "mumamo-do-fontify %s %s, chunk-min,max=%s,%s, new: %s %s" start end chunk-min chunk-max new-start new-end)

                ;; Narrowing and extending depends on where we finished last:
                (if (or ;(boundp 'mumamo-at-dq)
                     (and (= new-start chunk-min)
                          (= new-end   chunk-max)))
                    (setq narrow-to-chunk t)
                  (setq narrow-to-chunk nil))
                (if (and
                     ;;nil
                     mumamo-do-fontify-last-pos
                     (= mumamo-do-fontify-last-pos new-start)
                     (eq mumamo-do-fontify-last-major major)
                     )
                    (setq narrow-to-chunk nil)
                  (mumamo-msgfntfy "mumamo-do-fontify FFFFFFFFFFFFFFFFFFFFFFFF Flushing syntax cache")
                  ;; Fix-me: Clean up narrowing!
                  ;; Fix-me: commenting out this call cures the "" problem (if nil above):
                  ;;(setq narrow-to-chunk t)
                  )
                (setq mumamo-do-fontify-last-pos new-end)
                (setq mumamo-do-fontify-last-major major)

                ;; fix-me: Adding this spoils the fontifying of the comment in install.php
                ;;(setq narrow-to-chunk t)
                (mumamo-condition-case err
                    (if narrow-to-chunk
                        (save-restriction
                          (mumamo-msgfntfy "mumamo-do-fontify NARROW TO CHUNK!: %s-%s" new-start new-end)
                          (narrow-to-region new-start new-end)
                          ;; FIX-me: Seems like font-lock-dont-widen is not
                          ;; handled by css-mode:
                          (setq font-lock-dont-widen t)
                          ;;(setq font-lock-extend-region-functions nil)
                          (font-lock-fontify-region new-start new-end verbose)
                          )
                      (font-lock-fontify-region new-start new-end verbose))
                  (error
                   (mumamo-display-error 'mumamo-do-fontify-2
                           "mumamo-do-fontify m=%s, s=%s, e=%s: %s"
                           major start end (error-message-string err)))
                  ))))))
    (error
     (mumamo-display-error 'mumamo-do-fontify
                           "mumamo-do-fontify m=%s, s=%s, e=%s: %s"
                           major start end (error-message-string err)))
    )
  (mumamo-msgfntfy "mumamo-do-fontify exit >>>>>>> %s %s %s %s %s %s %s" start end verbose narrow-to-chunk chunk-min chunk-max major)
  )

(defun mumamo-do-unfontify (start end)
  "Unfontify region between START and END."
  (mumamo-condition-case err
      (font-lock-unfontify-region start end)
    (error
     (mumamo-display-error 'mumamo-do-unfontify "%s"
                           (error-message-string err)))))

(defun mumamo-fontify-region-with (start end verbose major narrow-to-chunk chunk-min chunk-max)
  "Fontify from START to END.
If VERBOSE is non-nil then print status messages during
fontification.

Do the fontification as in major mode MAJOR.

If NARROW-TO-CHUNK then restrict font locking to the region
between START and END.  See `mumamo-do-fontify' for more
information about this.

CHUNK-MIN and CHUNK-MAX are region to narrow to if
NARROW-TO-CHUNK is non-nil."
  ;; The text property 'fontified is always t here due to the way
  ;; jit-lock works!

  (mumamo-msgfntfy "mumamo-fontify-region-with %s %s %s %s, ff=%s" start end verbose major (get-text-property start 'fontified))
;;   (when mumamo-just-changed-major
;;     (mumamo-display-error 'mumamo-just-changed-major
;;                           "mumamo-fontify-region-with %s %s %s %s, ff=%s"
;;                           start end verbose major
;;                           (get-text-property start 'fontified)))
  (mumamo-condition-case err
      (progn
        (mumamo-with-major-mode-fontification major start
          (mumamo-do-fontify start end verbose narrow-to-chunk chunk-min chunk-max major)))
    (error
     (mumamo-display-error 'mumamo-fontify-region-with "%s"
                           (error-message-string err)))))

(defun mumamo-unfontify-region-with (start end major)
  "Unfontify from START to END as in major mode MAJOR."
  (mumamo-msgfntfy "mumamo-unfontify-region-with %s %s %s, ff=%s" start end major (get-text-property start 'fontified))
  ;;(when (and (= 1 start) (get-text-property start 'fontified)) (bt-to-msg))
  (mumamo-with-major-mode-fontification major start
    (mumamo-do-unfontify start end)))

;; Fix-me: Is this needed?
(defun mumamo-unfontify-region (start end)
  "Do nothing.
This function does nothing and it is not clear to me whether it
is needed at all.

START and END are the borders of the region to unfontify.

The value of `font-lock-unfontify-region-function' is this
function when mumamo is used.  For most major mode
`font-lock-default-unfontify-buffer' will call a corresponding
function.  However `font-lock-unfontify-buffer-function' is
`mumamo-unfontify-buffer' when mumamo is used so this does not
happen then.  \(The reason for this is that unfontification must
be done using mumamo chunk information and
`mumamo-unfontify-buffer' does that.)

Also, during fontification unfontification must be done
first.  However it is much easier to do that directly in
`mumamo-fontify-region-with' using `mumamo-unfontify-chunk' since
the chunks needed for unfontification are known there."
  (mumamo-msgfntfy "mumamo-unfontify-region %s %s" start end))


(defun mumamo-unfontify-buffer ()
  "Unfontify buffer and turn off mumamo.
Function called when the minor mode `font-lock-mode' is turned
off \(through the variable
`font-lock-unfontify-buffer-function').

This function also turns off mumamo since this needs
`font-lock-mode'."
  (mumamo-msgfntfy "===> mumamo-unfontify-buffer called")
  ;;(when mumamo-mode (mumamo-mode 0))
;;   (unless mumamo-mode
;;     (mumamo-msgfntfy "--- removing 'fontified from mumamo-unfontify-buffer")
;;     (mumamo-save-buffer-state nil
;;         (remove-text-properties (point-min) (point-max) '(fontified))))
  )

(defun mumamo-fontify-buffer ()
  "For `font-lock-fontify-buffer-function' call."
  (mumamo-msgfntfy "===> mumamo-fontify-buffer-function called")
  ;;(font-lock-default-fontify-buffer)
  (font-lock-fontify-region (point-min) (point-max))
  )


(defun mumamo-unfontify-chunk (chunk) ; &optional start end)
  "Unfontify mumamo chunk CHUNK."
  (let ((major (mumamo-chunk-major-mode chunk))
        (start (overlay-start chunk))
        (end   (overlay-end   chunk))
        (font-lock-dont-widen t))
    ;;(unless start (setq start (overlay-start chunk)))
    ;;(unless end   (setq end   (overlay-end   chunk)))
    (save-restriction
      (narrow-to-region start end)
      (mumamo-unfontify-region-with start end major))))

(defvar mumamo-just-changed-major nil
  "Avoid refontification when switching major mode.
Set to t by `mumamo-set-major'.  Checked and reset to nil by
`mumamo-jit-lock-function'.")
(make-variable-buffer-local 'mumamo-just-changed-major)

(defun mumamo-fontify-region (start end &optional verbose)
  "Fontify between START and END.
Take the major mode chunks into account while doing this.

If VERBOSE do the verbously.

The value of `font-lock-fontify-region-function' when
mumamo is used is this function."
  (mumamo-msgfntfy "++++++ mumamo-fontify-region %s %s %s, skip=%s" start end verbose mumamo-just-changed-major)
  ;; If someone else tries to fontify the buffer ...
  (if (and mumamo-just-changed-major
           ;; The above variable is reset in `post-command-hook' so
           ;; check if we are in a recursive search.  (Note: There are
           ;; other situation when this can occur.  It might be best to
           ;; remove this test later, or make it optional.)
           (= 0 (recursion-depth)))
      (mumamo-display-error 'mumamo-fontify-region
                            "Just changed major, should not happen")
    (mumamo-condition-case err
        (let ((debugger 'mumamo-debug-to-backtrace)
              (debug-on-error t))
          (mumamo-fontify-region-1 start end verbose))
      (error
       (mumamo-display-error 'mumamo-fontify-region "%s"
                             (error-message-string err))))))

(defconst mumamo-dbg-pretend-fontified nil
  "Set this to t to be able to debug more easily.
This is for debugging `mumamo-fontify-region-1' more easily by
just calling it.  It will make that function believe that the text
has a non-nil 'fontified property.")

(defun mumamo-exc-mode (chunk)
  "Return sub major mode for CHUNK.
If chunk is a main major mode chunk return nil, otherwise return
the major mode for the chunk."
  (let ((major (mumamo-chunk-major-mode chunk)))
    (unless (eq major (mumamo-main-major-mode))
      major)))

;; (let ((l '(1 2))) (setcar (nthcdr 1 l) 10) l)
(defsubst mumamo-chunk-value-set-min (chunk-values min) (setcar (nthcdr 0 chunk-values) min))
(defsubst mumamo-chunk-value-set-max (chunk-values max) (setcar (nthcdr 1 chunk-values) max))
(defsubst mumamo-chunk-value-min    (chunk-values) (nth 0 chunk-values))
(defsubst mumamo-chunk-value-max    (chunk-values) (nth 1 chunk-values))
(defsubst mumamo-chunk-value-narrow (chunk-values) (nth 2 chunk-values))
(defsubst mumamo-chunk-value-major  (chunk-values) (nth 3 chunk-values))

;(defun mumamo-unfontify-chunk (chunk)
(defun mumamo-adjust-old-chunks (new-chunk-values start end)
  "Adjust old chunks to NEW-CHUNK-VALUES.
This must be run whenever adding a new chunk.

If START and END is non-nil then mark ranges outside of NEW-CHUNK
for refontification."
  ;;(lwarn t :warning "here 0")
  (let* ((new-start (mumamo-chunk-value-min   new-chunk-values))
         (new-end   (mumamo-chunk-value-max   new-chunk-values))
         (new-major (mumamo-chunk-value-major new-chunk-values))
         (new-is-closed new-end)
         (new-real-end (or new-end (point-max)))
         (old-chunks (delq nil
                           (mapcar
                            (lambda (ovl)
                              (when (mumamo-chunk-major-mode ovl)
                                ovl))
                            (overlays-in new-start new-real-end)))))
    ;;(lwarn t :warning "here")
    (setq old-chunks (delq nil old-chunks))
    (when (and start (< new-start start))
      (let ((must-refontify t))
        (dolist (old old-chunks)
          (when (and (= (overlay-start old) new-start)
                     (>= (overlay-end old) start)
                     (eq new-major (mumamo-chunk-major-mode old)))
            (setq must-refontify nil)))
        (when must-refontify
          ;;(mumamo-mark-for-refontification new-start (- start 1))
          )))
    ;;(lwarn t :warning "here 1")
    (when (and end new-real-end (> new-real-end end))
      (mumamo-mark-for-refontification (+ end 0) new-real-end))
    ;;(lwarn t :warning "here 2")
    (dolist (old old-chunks)
      (let ((old-start (overlay-start old))
            (old-end   (overlay-end   old))
            (old-major (mumamo-chunk-major-mode old)))
      (assert old)
      (assert old-major)
      ;; Fix-me: I forget to refontify something here.
      ;;
      ;; Fix-me: Clean up among the overlay removal.
      ;;
      ;;(lwarn t :warning "here 3")
      (when (< old-start new-start)
        ;;(assert (mumamo-chunk-is-closed old))
        (move-overlay old old-start (1- new-start)))
      ;;(lwarn t :warning "here 4")
      (when (and (<= new-start old-start)
                 (<= old-end new-real-end))
        (delete-overlay old))
      ;;(when (< old-start new-start))
      ;; There is nothing to do in this case since the conditions
      ;; for fontifying have not changed in this region.
      (when (< new-real-end old-end)
        (if new-is-closed
            (progn
              (delete-overlay old)
              (mumamo-mark-for-refontification (+ new-real-end 1) old-end))
          (if (eq (mumamo-chunk-major-mode old) new-major)
              (progn
                ;;(move-overlay new-chunk new-start old-end)
                (mumamo-chunk-value-set-min new-chunk-values new-start)
                (mumamo-chunk-value-set-max new-chunk-values old-end)
                (delete-overlay old))
            (delete-overlay old)
            (mumamo-mark-for-refontification (+ new-real-end 1) old-end))))))))

;; Fix-me: Is it really meaningful to save old overlays like this? It
;; is supposed to be an optimization.
(defvar mumamo-chunk-ovls nil
  "Internal.  Unused overlays.")

(defvar mumamo-chunks-to-remove nil
  "Internal.  Chunk overlays marked for removal.")
(make-variable-buffer-local 'mumamo-chunks-to-remove)

(defun mumamo-fontify-region-1 (start end verbose)
  "Fontify region between START and END.
If VERBOSE is non-nil then print status messages during
fontification.

This is called from `mumamo-fontify-region' which is the value of
`font-lock-fontify-region-function' when mumamo is used.  \(This
means that it ties into the normal font lock framework in Emacs.)

Note: The purpose of extracting this function from
`mumamo-fontify-region' \(which is the only place where it is
called) is to make debugging easier.  Edebug will without this
function just step over the `condition-case' in
`mumamo-fontify-region'.

The fontification is done in steps:

- First a mumamo chunk is found or created at the start of the
  region with `mumamo-get-chunk-at'.
- Then this chunk is fontified according to the major mode for
  that chunk.
- If the chunk did encompass the whole region then this procedure
  is repeated with the rest of the region.

If some mumamo chunk in the region between START and END has been
marked for removal \(for example by `mumamo-jit-lock-after-change') then
they are removed by this function.

For some main major modes \(see `define-mumamo-multi-major-mode') the
main major modes is first used to fontify the whole region.  This
is because otherwise the fontification routines for that mode may
have trouble finding the correct starting state in a chunk.

Special care has been taken for chunks that are strings, ie
surrounded by \"...\" since they are fontified a bit special in
most major modes."
  ;;(lwarn t :warning "\nmumamo-fontify-region-1 %s %s" start end)
  ;;(with-current-buffer "*Warnings*" (insert (with-output-to-string (backtrace))))
  ;; Fix-me: unfontifying should be done using the correct syntax table etc.
  ;; Fix-me: refontify when new chunk
  (save-match-data
    (let* ((old-point (point))
           (here start)
           (main-major (mumamo-main-major-mode))
           (all-main nil)
           narrow-to-chunk
           (fontified-t ;;(or mumamo-dbg-pretend-fontified
            ;;    (get-text-property here 'fontified))
            t)
           (first-new-ovl nil)
           (last-new-ovl nil)
           (chunk-at-start-1 (mumamo-get-existing-chunk-at start)))
      (when chunk-at-start-1
        (unless (= start (1- (overlay-end chunk-at-start-1)))
          (setq chunk-at-start-1 nil)))
      (while (and fontified-t
                  (< here end))
        ;; Check where new chunks should be, adjust old chunks as
        ;; necessary.  Refontify inside end-start and outside of
        ;; start-end mark for refontification when major-mode has
        ;; changed or there was no old chunk.
        ;;
        ;; Fix-me: Join chunks!
        ;;(lwarn t :warning "----while here=%s end=%s" here end)
        (let* ((chunk (mumamo-get-existing-chunk-at here))
               (old-chunk chunk)
               (chunk-min (when chunk (overlay-start chunk)))
               (chunk-max (when chunk (overlay-end chunk)))
               (chunk-min-1 (when chunk (if (> chunk-min (point-min)) (1- chunk-min) (point-min)))) ;chunk-min
               (chunk-max-1 (when chunk (if (< chunk-max (point-max)) (1+ chunk-max) (point-max)))) ;chunk-max
               (chunk-min-face (when chunk (get-text-property chunk-min-1 'face)))
               (chunk-max-face (when chunk (get-text-property chunk-max-1 'face)))
               (chunk-major (when chunk (mumamo-chunk-major-mode chunk)))

               ;; If old chunks exists we can assume that we come here
               ;; from after-change-functions.  - Eh, no...  This can
               ;; be a refontification done further down in the file
               ;; after a change...
               ;;
               ;; But we still can do this: If and old chunk exist
               ;; right before START position then during this call to
               ;; this function we can rely on that chunk when
               ;; determining chunk boundaries backwards.
               ;;
               ;; In this case we can safely narrow to min-(point-max)
               ;; when we check for new overlay boundaries - if we
               ;; let-bind mumamo-known-chunk-start.  (And assume that
               ;; all chunk routines uses this variable ...)
               ;;
               ;; When there are no old chunks we can narrow to
               ;; min-(point-max) if there is a mumamo chunk that ends at
               ;; min-1.
               ;;
               ;; And actually we might use a value other than
               ;; (point-max), but then we have to take care of that
               ;; the end of the chunk is "open".
               ;;
               ;; Otherwise we can not narrow when searching for new
               ;; chunk boundaries.
               (narrow-cv-min
                ;; Fix-me: Something more sensibel here, maybe the
                ;; length of the dividers must be saved somehow:
                ;; Fix-me: I've mixed start/here here.
                (if chunk-at-start-1
                    (- start 0)
                  (if chunk
                      (if (> (overlay-start chunk) start)
                          (- (overlay-start chunk) 9)
                        ;; Fix-me: This is a disaster.  Fixing this
                        ;; requires probably rewriting the chunk
                        ;; dividing routines ...
                        (overlay-start chunk)))))
               (narrow-cv-max (if chunk
                                  (progn
                                    (goto-char end)
                                    (line-end-position))
                                (point-max)))
               (chunk-values (if nil ;narrow-cv-min
                                 (save-restriction
                                   (narrow-to-region narrow-cv-min narrow-cv-max)
                                   (let ((mumamo-known-chunk-start
                                          (if chunk-at-start-1
                                              (cons start (mumamo-exc-mode chunk-at-start-1))
                                            (cons (overlay-start chunk) (mumamo-exc-mode chunk)))))
                                     (mumamo-create-chunk-values-at here)))
                               (mumamo-create-chunk-values-at here)))
               (cv-min          (mumamo-chunk-value-min          chunk-values))
               (cv-max          (mumamo-chunk-value-max          chunk-values))
               ;;(cv-major-normal (mumamo-chunk-value-major-normal chunk-values))
               (cv-is-closed    cv-max) ;(mumamo-chunk-value-is-closed    chunk-values))
               (cv-major-narrow (mumamo-chunk-value-narrow chunk-values))
               (cv-major-sub    (mumamo-chunk-value-major  chunk-values))

               (max)                    ; (min chunk-max end))
               (narrow-to-chunk nil) ;(overlay-get chunk 'mumamo-narrow))
               prev-major
               prev-chunk
               (need-new-chunk nil)
               )
          ;;(lwarn t :warning "old-chunk=%s cv-min/cv-max %s/%s" old-chunk cv-min cv-max)
          (unless cv-max (setq cv-max end))
          ;;(setq old-chunk nil)
          (unless old-chunk
            (mumamo-adjust-old-chunks chunk-values nil end)
            (setq chunk (mumamo-create-chunk-from-chunk-values chunk-values))
            )
          ;; Fix-me: unfontify if new major mode does not have fontification (and how do you detect that? font-lock-defaults?)
          (when old-chunk
            ;; Compare with cv:
            ;;(setq need-new-chunk t)
            ;;(lwarn t :warning "xchunk-major=%s, cv-major-sub=%s, main-major=%s" chunk-major cv-major-sub main-major)
            (let ((old-start (overlay-start old-chunk))
                  (old-end (overlay-end old-chunk)))
              (unless (and (eq chunk-major (or cv-major-sub main-major))
                           (= old-end cv-max))
                (setq need-new-chunk t)
                ;; Mark everything outside start-end for refontification
                ;; Fix-me: +-1 ...
                (when (< old-start start)
                  ;; Fix-me: move refontification, do it when
                  ;; comparing with new chunk.  Remember is-closed.
                  ;;(mumamo-mark-for-refontification old-start (1- start))
                  (setq need-new-chunk t)
                  )
                (when (> old-end end)
                  ;;(mumamo-mark-for-refontification (1+ end) old-end)
                  (setq need-new-chunk t)
                  )))
            )

          (when need-new-chunk
            (mumamo-adjust-old-chunks chunk-values start end)
            (setq chunk (mumamo-create-chunk-from-chunk-values chunk-values))
            (setq mumamo-chunk-ovls (cons old-chunk mumamo-chunk-ovls)))

          ;;(lwarn t :warning "need-new-chunk=%s chunk-values=%s" need-new-chunk chunk-values)
          (setq chunk-min (when chunk (overlay-start chunk)))
          (setq chunk-max (when chunk (overlay-end chunk)))
          (setq chunk-min-1 (when chunk (if (> chunk-min (point-min)) (1- chunk-min) (point-min)))) ;chunk-min
          (setq chunk-max-1 (when chunk (if (< chunk-max (point-max)) (1+ chunk-max) (point-max)))) ;chunk-max
          (setq chunk-min-face (when chunk (get-text-property chunk-min-1 'face)))
          (setq chunk-max-face (when chunk (get-text-property chunk-max-1 'face)))
          (setq chunk-major (when chunk (mumamo-chunk-major-mode chunk)))

          (if first-new-ovl
              (setq last-new-ovl chunk)
            (setq last-new-ovl chunk)
            (setq first-new-ovl chunk))
          ;; Only first loop:
          (when (= here start)
            ;; FIX-ME: Testing handling this:
            ;;(setq font-lock-syntactically-fontified (1- here))
            ;; FIX-ME: Is all-main=t a performance problem?
            (when (eq chunk-major main-major) ;;(and (= chunk-max end)
              (setq all-main t))
            (when (and all-main
                       ;;(or (>= chunk-max end)
                       ;;(eq chunk-major main-major)
                       )
              ;; Set chunk-min/max for `mumamo-do-fontify':
              ;;(let ((mumamo-let-chunk-min start)
              ;;      (mumamo-let-chunk-max end))
                (mumamo-fontify-region-with start end verbose main-major nil start end)
                ;;(let ((use-dialog-box nil)) (y-or-n-p "all main-ready"))
              ;;  )
              ))
          (mumamo-msgfntfy "mumamo-fontify-region-1 FACE FACE FACE chunk-min: %s %s, chunk-max: %s %s" chunk-min chunk-min-face chunk-max chunk-max-face)

          ;;(lwarn t :warning "chunk-max=%s" chunk-max)
          (setq max (min chunk-max end))

          ;; Do not refontify sub-chunks entirely in a comment:
          (unless (and nil
                       all-main
                       (memq chunk-min-face '(font-lock-comment-face font-lock-comment-face))
                       (memq chunk-max-face '(font-lock-comment-face font-lock-comment-face)))
                                        ;)

            ;;(when (eq ?\" (char-after here)) (lwarn 't :warning "eq \" at %s" (point)) (setq here (1+ here)))
            (mumamo-msgfntfy "*** mumamo-fontify-region-1.here=%s, chunk=%s" here chunk)
            ;;(when (< end chunk-max) (setq end chunk-max))

            (when narrow-to-chunk
              (mumamo-msgfntfy "mumamo-fontify-region-1: narrow-to-chunk %s" narrow-to-chunk))
            (assert chunk) (assert (overlay-buffer chunk)) (assert chunk-min) (assert chunk-max) (assert chunk-major)
            ;; Fix-me: The next assertion sometimes fails.  Could it be
            ;; that this loop is continuing even after a change in the
            ;; buffer? How do I stop that? When?:
            (mumamo-msgfntfy "*** mumamo-fontify-region-1: here 2 here=%s, start=%s, chunk-min=%s,max=%s end=%s chunk-major=%s" here start chunk-min chunk-max end chunk-major)
            ;;(assert (or (= here start) (= here chunk-min)) nil "h=%s, s=%s, cm=%s-%s, e=%s, chunk-major=%s" here start chunk-min chunk-max end chunk-major)
            (assert (not (eq prev-major chunk-major)))
            (when prev-chunk (assert (= (overlay-end prev-chunk) (overlay-start chunk))))

            (unless (and (eq chunk-major main-major)
                         all-main)
              ;; Take care of "...".  This is necessary since strings
              ;; are fontified separetely in most major modes:
              (if t ;(not (and (eq ?\" (char-after here)) narrow-to-chunk))
                  (progn
                    ;; Fix-me: It looks like this path always
                    ;; works.  Test for a while to try to get rid of
                    ;; narrowing info here.
                    (mumamo-fontify-region-with here max verbose chunk-major narrow-to-chunk chunk-min chunk-max)
                    ;;(lwarn t :warning "NOT using dq")
                    )
                ;; First ":
                (let ((mumamo-at-dq t))
                  ;;(lwarn t :warning "using dq")
                  (mumamo-fontify-region-with here here verbose chunk-major narrow-to-chunk chunk-min chunk-max)
                  (mumamo-fontify-region-with (1+ here) max verbose chunk-major narrow-to-chunk chunk-min chunk-max)
                  )
                ;; Second ":
                (setq max (1+ max))
                ))
            )
          (setq prev-major chunk-major)
          (setq prev-chunk chunk)
          (setq here max)
          (setq fontified-t (or mumamo-dbg-pretend-fontified
                                (get-text-property here 'fontified)))
          ;;(lwarn t :warning "max->here=%s" here)
          ))
      (goto-char old-point)
      (unless fontified-t
        ;; Fix-me: I am not sure what to do here.  Probably just
        ;; refontify the rest between start and end.  But does not
        ;; this lead to unnecessary refontification?
        (mumamo-mark-for-refontification here end))
      ;; Check if more should be refontified due to major mode
      ;; changes.  Compare with old overlays.
      (let ((ovl-start (min start (overlay-start first-new-ovl)))
            (ovl-end   (max end   (overlay-end   last-new-ovl)))
            (first-new-major (overlay-get first-new-ovl 'mumamo-major))
            (last-new-major  (overlay-get last-new-ovl  'mumamo-major)))
        (mumamo-msgfntfy "*** mumamo-fontify-region-1: here 3 ovl-start=%s,end=%s, start=%s, chunks-to-remove=%s" ovl-start ovl-end start mumamo-chunks-to-remove)
        ;;(lwarn 't :warning "p=%s ovl-start=%s start=%s end=%s ovl-end=%s" (point) ovl-start start end ovl-end)
        (when (< ovl-start start)
          ;; Check all old overlays in this region
          (dolist (old-o mumamo-chunks-to-remove)
            (when (overlay-buffer old-o)
              (let ((old-start (overlay-start old-o))
                    (old-end   (overlay-end   old-o))
                    min-refont
                    max-refont)
                (mumamo-msgfntfy "*** mumamo-fontify-region-1: here 3a old-start=%s,end=%s" old-start old-end)
                ;; The trick here is writing this in a manner so that
                ;; you do not have to use paper and pencil to check it:
                (when (< ovl-start old-end)
                  (setq max-refont (min ovl-start old-start)))
                (when (< old-start start)
                  (setq min-refont (max start old-end)))
                (and min-refont
                     max-refont
                     (< min-refont max-refont)
                     (not (eq first-new-major (overlay-get old-o 'mumamo-old-major-mode)))
                     (progn
                       ;;(lwarn t :warning "mumamo-mark-for-refontification min-refont max-refont/%s %s" min-refont max-refont)
                       (mumamo-mark-for-refontification min-refont max-refont)
                       )
                     )))))
        (mumamo-msgfntfy "*** mumamo-fontify-region-1: here 4")
        (when (< end ovl-end)
          ;; Check all old overlays in this region
          (dolist (old-o mumamo-chunks-to-remove)
            (when (overlay-buffer old-o)
              (let ((old-start (overlay-start old-o))
                    (old-end   (overlay-end   old-o))
                    min-refont
                    max-refont)
                (when (< end old-end)
                  (setq max-refont (min ovl-end old-end)))
                (when (< old-start ovl-end)
                  (setq min-refont (max end old-start)))
                (and min-refont
                     max-refont
                     (< min-refont max-refont)
                     (not (eq last-new-major (overlay-get old-o 'mumamo-old-major-mode)))
                     (progn
                       ;;(lwarn 't :warning " min-refont=%s max-refont=%s" min-refont max-refont)
                       (mumamo-mark-for-refontification min-refont max-refont))))))))
      ;;(mumamo-remove-old-overlays)
      )))

(defun mumamo-remove-old-overlays ()
  "Remove mumamo overlays marked for removal from the buffer."
  (while mumamo-chunks-to-remove
    (let ((ovl (car mumamo-chunks-to-remove)))
      (setq mumamo-chunks-to-remove (cdr mumamo-chunks-to-remove))
      ;;(unless (overlay-get ovl 'mumamo-old-major-mode) (error "Chunk overlay was not marked for removal"))
      (overlay-put ovl 'mumamo-old-major-mode nil)
      (delete-overlay ovl)
      (setq mumamo-chunk-ovls (cons ovl mumamo-chunk-ovls)))))



(defun mumamo-fetch-major-mode-setup (major)
  "Return a function that set some local variables of major mode MAJOR.
The variables affecting font locking and indentation are fetched
by calling the major mode MAJOR in a temporary buffer and then
making a function that sets this variable values when it is
called.

The function returned is meant to be run from
`mumamo-with-major-mode-setup' where they are let bound so that
they will only affect that macro call."
  ;; FIX-ME: Maybe just use `font-lock-defaults' + those below?
  ;;
  ;; (info "(elisp) Other Font Lock Variables")
  ;; (info "(elisp) Syntactic Font Lock)
  (let ((buf (get-buffer "mumamo-fetch-major-mode-setup")))
    (when buf (kill-buffer buf)))
  ;; font-lock-mode can't be turned on in buffers whose names start
  ;; with a char with white space syntax.  It is not possible to change
  ;; name of a temp buffer.
  ;;
  ;;(with-temp-buffer
  (with-current-buffer (get-buffer-create "mumamo-fetch-major-mode-setup")
    (mumamo-msgfntfy "mumamo-fetch-major-mode-setup %s" major)
    (let ((mumamo-fetching-major t))
      (funcall major)
      )
    ;;(make-local-variable 'font-lock-mode)
    ;; Must give buffer a name, otherwise we can't turn on font-lock-mode:
    ;;(rename-buffer "mumamo-fetch-major-mode-setup")
    (mumamo-msgfntfy ">>> mumamo-fetch-major-mode-setup A font-lock-mode=%s" font-lock-mode)
    (font-lock-mode 1)
    (mumamo-msgfntfy "<<< mumamo-fetch-major-mode-setup B font-lock-mode=%s" font-lock-mode)
    (mumamo-msgfntfy "mumamo-fetch-major-mode-setup: fetching jit-lock-after-change-extend-region-functions A=%s" jit-lock-after-change-extend-region-functions)
    (font-lock-fontify-buffer)
    (mumamo-msgfntfy "mumamo-fetch-major-mode-setup: fetching jit-lock-after-change-extend-region-functions B=%s" jit-lock-after-change-extend-region-functions)
    (let* ((func-sym (intern (concat "mumamo-evaled-set-" (symbol-name major))))
           (jit-lock-after-change-extend-region-functions jit-lock-after-change-extend-region-functions)
           (fetch-func-definition
            `(defun ,func-sym ()
               ;; Be XML compliant:
               (when (mumamo-derived-from-mode ',major 'sgml-mode)
                 (set 'sgml-xml-mode t))
               ;;; Don't set major-mode, it is set in the with functions:
               ;;(set 'major ,(custom-quote major))

               ;; FIX-ME: How to handle `font-lock-fontified' when switching major modes?
               ;;(list 'font-lock-fontified nil) ; Whether we have fontified the buffer.
               ;;(list 'font-lock-dont-widen ,(custom-quote font-lock-dont-widen))
               ;;(list 'font-lock-beg) (defvar font-lock-end)
               ;;(list 'jit-lock-start) (defvar jit-lock-end)

               ;;(list 'forward-sexp-function ,(custom-quote forward-sexp-function))

               ;; FIX-ME: How to handle `font-lock-syntactically-fontified' when
               ;; switching major modes?  Probably set this to chunk min.
               ;;(list 'font-lock-syntactically-fontified 0) ; Check this

               ;;; Maybe these below are a minimum?

               ;; Fix-me: Should we copy here or?
               (set 'major-syntax-table ,(custom-quote (copy-syntax-table (syntax-table))))

               (set 'font-lock-syntactic-keywords ,(custom-quote font-lock-syntactic-keywords))
               (set 'font-lock-multiline ,(custom-quote font-lock-multiline))
               (set 'font-lock-extend-after-change-region-function ,(custom-quote font-lock-extend-after-change-region-function))
               (set 'font-lock-keywords-alist ,(custom-quote font-lock-keywords-alist))
               (set 'font-lock-extend-region-functions ,(custom-quote font-lock-extend-region-functions))
               (set 'font-lock-comment-start-skip ,(custom-quote font-lock-comment-start-skip))
               (set 'font-lock-comment-end-skip ,(custom-quote font-lock-comment-end-skip))

               ;; We cache the values computed from this:
               (set 'font-lock-mode-major-mode ,(custom-quote font-lock-mode-major-mode))
               (set 'font-lock-set-defaults ,(custom-quote t)) ; whether we have set up defaults.

               ;; Set from font-lock-defaults normally:
               ;; Syntactic Font Lock
               (set 'font-lock-keywords-only ,(custom-quote font-lock-keywords-only))
               (set 'font-lock-syntax-table ,(custom-quote font-lock-syntax-table))
               (set 'font-lock-beginning-of-syntax-function ,(custom-quote font-lock-beginning-of-syntax-function))
               ;; Other
               (set 'font-lock-keywords ,(custom-quote font-lock-keywords))
               (set 'font-lock-removed-keywords-alist ,(custom-quote font-lock-removed-keywords-alist))
               (set 'font-lock-keywords-case-fold-search ,(custom-quote font-lock-keywords-case-fold-search))
               (set 'font-lock-syntactic-face-function ,(custom-quote font-lock-syntactic-face-function))

               ;; Other Font Lock Variables
               (set 'font-lock-mark-block-function ,(custom-quote font-lock-mark-block-function))
               (set 'font-lock-extra-managed-props ,(custom-quote font-lock-extra-managed-props))
               ;; This value is fetched from font-lock:
               (set 'font-lock-fontify-buffer-function ,(custom-quote font-lock-fontify-buffer-function))
               (set 'font-lock-unfontify-buffer-function ,(custom-quote font-lock-unfontify-buffer-function))
               (set 'font-lock-fontify-region-function ,(custom-quote font-lock-fontify-region-function))
               (set 'font-lock-unfontify-region-function ,(custom-quote font-lock-unfontify-region-function)) ;; FIX-ME

               ;; Jit Lock Variables
               (set 'jit-lock-after-change-extend-region-functions ,(custom-quote jit-lock-after-change-extend-region-functions))

               (set 'mumamo-original-syntax-begin-function ,(custom-quote syntax-begin-function))
               (set 'mumamo-original-fill-paragraph-function ,(custom-quote fill-paragraph-function))

               ;; From cc-mode.el:
               (set 'parse-sexp-ignore-comments ,(custom-quote parse-sexp-ignore-comments))
               (set 'indent-line-function ,(custom-quote indent-line-function))
               (set 'indent-region-function ,(custom-quote indent-region-function))
               (set 'normal-auto-fill-function ,(custom-quote normal-auto-fill-function))
               (set 'comment-start ,(custom-quote comment-start))
               (set 'comment-end ,(custom-quote comment-end))
               (set 'comment-start-skip ,(custom-quote comment-start-skip))
               (set 'comment-multi-line ,(custom-quote comment-multi-line))
               (set 'comment-line-break-function ,(custom-quote comment-line-break-function))
               (set 'paragraph-start ,(custom-quote paragraph-start))
               (set 'paragraph-separate ,(custom-quote paragraph-separate))
               (set 'paragraph-ignore-fill-prefix ,(custom-quote paragraph-ignore-fill-prefix))
               (set 'adaptive-fill-mode ,(custom-quote adaptive-fill-mode))
               (set 'adaptive-fill-regexp ,(custom-quote adaptive-fill-regexp))
               )))
      (eval fetch-func-definition)
      ;; Silence the byte compiler:
      (let ((major-syntax-table))
        (byte-compile func-sym))
      (put func-sym 'mumamo-defun fetch-func-definition)
      func-sym)))

(defun mumamo-new-beginning-of-syntax ()
  (let* ((chunk (mumamo-get-existing-chunk-at (point)))
         (major (when chunk (mumamo-chunk-major-mode chunk))))
    (when major
      (mumamo-with-major-mode-fontification major (point)
        (when mumamo-original-syntax-begin-function
          (funcall mumamo-original-syntax-begin-function))))))

;; Fix-me: Check use of ppss
;; Fix-me: Check where to use the version below and the version above
(defun mumamo-beginning-of-syntax ()
  "Move to beginning of syntax.
Call the original `syntax-begin-function'.  If this moved point to
before current mumamo chunk then move point to beginning of
chunk."
  (unless (boundp 'mumamo-original-syntax-begin-function)
    (error "Uh? mosbf not bound=%s" mumamo-original-syntax-begin-function))
  (when mumamo-original-syntax-begin-function
    (let* ((chunk (mumamo-get-chunk-at (point)))
           (chunk-start (when chunk (overlay-start chunk)))
           (major (when chunk (mumamo-chunk-major-mode chunk)))
           )
      (when chunk
        (mumamo-with-major-mode-fontification major chunk-start
          (when mumamo-original-syntax-begin-function
            (funcall mumamo-original-syntax-begin-function)
            (when (and chunk
                       (< (point) chunk-start))
              (goto-char chunk-start))))))))

(defun mumamo-get-major-mode-setup (use-major)
  "Get local variable values for major mode USE-MAJOR.
These variables are used for indentation and fontification.  The
variables are returned in a list with the same format as
`mumamo-fetch-major-mode-setup'.

The list of local variable values which is returned by this
function is also cached in `mumamo-internal-major-modes-alist'."
  (let* (;;(use-major (mumamo-get-major-mode-for-fontification major))
         (fontify-info (assq use-major mumamo-internal-major-modes-alist)))
    ;;(unless (eq major use-major) (setq use-major 'php-mode))
    ;;(unless (eq major use-major) (mumamo-msgfntfy "mumamo-get-major-mode-setup.use-major=%s, major=%s" use-major major))
    (setq fontify-info (assq use-major mumamo-internal-major-modes-alist))
    (unless fontify-info
      (add-to-list 'mumamo-internal-major-modes-alist
                   ;;(list use-major (mumamo-fetch-major-mode-setup use-major))
                   (list use-major (mumamo-fetch-major-mode-setup use-major))
                   )
      (setq fontify-info (assq use-major mumamo-internal-major-modes-alist)))
    (setq fontify-info (cadr fontify-info))
    ;;(mumamo-msgfntfy "  mumamo-get-major-mode-setup: not fontify-info=%s" (not fontify-info))
    fontify-info))

;; Fix-me: This is to drastic since after-change-functions are run
;; immediately after a change.  It breaks indentation for
;; example.  Change this to not remove the chunks but just mark them
;; for maybe removing.  They should still be used until new chunks are
;; created by fontification.
(defun mumamo-remove-chunk-overlays (min max)
  "Mark chunk overlays in MIN to MAX as old.
Return as a cons region covered by those overlays if greater than
MIN to MAX, otherwise MIN to MAX."
  (mumamo-msgfntfy "mumamo-remove-chunk-overlays %s %s" min max)
  (let ((min-min min)
        (max-max max)
        (did-remove nil))
    (dolist (o (overlays-in min max))
      (when (mumamo-chunk-major-mode o)
        (when (< max (overlay-end o))
          (setq max-max (overlay-end o)))
        (when (> min (overlay-start o))
          (setq min-min (overlay-start o)))
        ;; Save the old major mode so that we can compare with it:
        (overlay-put o 'mumamo-old-major-mode (mumamo-chunk-major-mode o))
        (overlay-put o 'mumamo-major-mode nil)
        (delete-overlay o)
        (setq did-remove t)
        (setq mumamo-chunks-to-remove (cons o mumamo-chunks-to-remove))))
    (when did-remove (cons min-min max-max))))

(defun mumamo-remove-all-chunk-overlays ()
  "Remove all CHUNK overlays from the current buffer."
  (save-restriction
    (widen)
    (mumamo-remove-chunk-overlays (point-min) (point-max))
    (mumamo-remove-old-overlays)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Creating and accessing chunks

(defun mumamo-create-chunk-values-at (pos)
  "Return a list of values to be used to create a chunk at POS."
  ;; FIX-ME: Maybe remove the narrow support here? This seems to me to
  ;; be overriden now by the narrowing done by
  ;; `mumamo-fontify-region-1'.
  ;;
  ;; Fix-me: rename and clean here:
  (let* ((chunk-info (cdr mumamo-current-chunk-family))
         ;;(major-normal (mumamo-main-major-mode))
         (chunk-fns    (cadr chunk-info))
         min
         max
         (max-found nil)
         major-sub
         major-narrow
         chunk-ovl
         )
    (dolist (fn chunk-fns)
      (let* ((r (funcall fn pos (point-min) (point-max)))
             (rmin      (nth 0 r))
             (rmax      (nth 1 r))
             (is-exc    (nth 2 r))
             (nrrow     (nth 3 r))
             (rmax-found rmax))
        ;;(lwarn t :warning "%s %s %s r=%s" fn (point-min) (point-max) r)
        (unless rmin (setq rmin (point-min)))
        (unless rmax (setq rmax (point-max)))
        ;; comparision have to be done differently if we are in an
        ;; exception part or not.  since we are doing this from top to
        ;; bottom the rules are:
        ;;
        ;; - exception parts always outrules non-exception part.  when
        ;;   in exception part the min start point should be used.
        ;; - when in non-exception part the max start point and the
        ;;   min end point should be used.
        ;;
        ;; check if first run:
        (if (not min)
            (progn
              (setq min rmin)
              (setq max rmax)
              (setq max-found rmax-found)
              (setq major-sub is-exc)
              (setq major-narrow nrrow))
          (if is-exc
              (if major-sub
                  (when (or (not min)
                            (< rmin min))
                    (setq min rmin)
                    (setq max rmax)
                    (when rmax-found (setq max-found t))
                    (setq major-sub is-exc))
                (setq min rmin)
                (setq max rmax)
                (when rmax-found (setq max-found t))
                (setq major-sub is-exc)
                (setq major-narrow nrrow))
            (unless major-sub
              (when (< min rmin) (setq min rmin))
              (when (< rmax max)
                (setq max-found rmax-found)
                (setq max rmax))
              )))))
    ;; check!
    (assert (<= min pos)) (assert (<= pos max))
    ;;(lwarn t :warning "ret=%s" (list min (when max-found max) major-narrow major-sub))
    (list min (when max-found max) major-narrow major-sub)
    ))

(defun mumamo-major-mode-from-modespec (major-spec)
  "Translate MAJOR-SPEC to a major mode.
See `mumamo-major-modes'."
  (let ((modes (cdr (assq major-spec mumamo-major-modes)))
        (mode 'fundamental-mode))
    (setq mode
          (catch 'mode
            (dolist (m modes)
              (when (functionp m)
                (let ((def (symbol-function m)))
                  (when (and (listp def)
                             (eq 'autoload (car def)))
                    (mumamo-condition-case err
                        (load (nth 1 def))
                      (error (setq m nil)))))
                (throw 'mode m)))
            nil))
    (unless mode
      (if (functionp major-spec)
          ;; As a last resort allow spec to be a major mode too:
          (setq mode major-spec)
        (if modes
            (mumamo-warn-once '(mumamo-major-mode-from-modespec)
                              "\n  Couldn't find an available major mode for specification %s,\n  alternatives are:\n    %s"
                              major-spec modes)
          (lwarn '(mumamo-major-mode-from-modespec)
                 :error
                 "\n  Couldn't find an available major mode for spec %s"
                 major-spec))
        (setq mode 'fundamental-mode)))
    (mumamo-msgfntfy " mumamo-major-mode-from-modespec %s => %s" major-spec mode)
    mode))
;(mumamo-major-mode-from-modespec 'ruby-mode)

(defun mumamo-create-chunk-from-chunk-values (chunk-values)
  "Create a chunk from CHUNK-VALUES and return it.
CHUNK-VALUES should be in the format returned by
`mumamo-create-chunk-values-at'."
  ;; Fix-me: Move adjusting of old chunks to here since it must always be done.
  (let* ((min          (mumamo-chunk-value-min    chunk-values))
         (max          (mumamo-chunk-value-max    chunk-values))
         (major-narrow (mumamo-chunk-value-narrow chunk-values))
         (major-sub    (mumamo-chunk-value-major  chunk-values))
         (major-normal (mumamo-main-major-mode))
         (max-found    (when max t))
         chunk-ovl)
    (assert major-normal)
    ;;(assert major-sub)
    ;; remove all old chunk overlays between min and max
    ;; Fix-me: Must keep track of those to know how much to refontify:
    (unless max-found  (setq max (point-max)))
    (mumamo-remove-chunk-overlays min max)
    (if nil ;mumamo-chunk-ovls
        (progn
          (setq chunk-ovl (car mumamo-chunk-ovls))
          (setq mumamo-chunk-ovls (cdr mumamo-chunk-ovls))
          (overlay-put chunk-ovl 'face nil)
          (overlay-put chunk-ovl 'mumamo-old-major-mode nil)
          (overlay-put chunk-ovl 'mumamo-major-mode nil))
      (setq chunk-ovl (make-overlay (point-min) (point-min)))
      ;; Fix-me: Why should these overlays have a priority?
      ;;(overlay-put chunk-ovl 'priority 10)
      )
    (move-overlay chunk-ovl min max)
    (overlay-put chunk-ovl 'mumamo-is-closed max-found)
    (overlay-put chunk-ovl 'mumamo-narrow major-narrow)
    (when (and (= min (point-min))
               (= max (point-max)))
      ;;(setq major-sub mumamo-main-major-mode)
      (setq major-sub nil)
      )
    (if major-sub
        (let ((major-sub-to-use (mumamo-major-mode-from-modespec major-sub)))
          (overlay-put chunk-ovl 'mumamo-major-mode major-sub-to-use)
          (overlay-put chunk-ovl
                       'face
                       (when (memq mumamo-chunk-coloring
                                   '(submode-colored both-colored))
                         mumamo-background-chunk-submode)))
      (overlay-put chunk-ovl 'mumamo-major-mode major-normal)
      (overlay-put chunk-ovl
                   'face
                   (when (memq mumamo-chunk-coloring '(both-colored))
                     mumamo-background-chunk-major)))
;;;     (unless (and (<= (overlay-start chunk-ovl) pos)
;;;                  (<= pos (overlay-end chunk-ovl)))
;;;       (mumamo-display-error 'mumamo-create-chunk-at "start=%s, pos=%s, end=%s"
;;;              (overlay-start chunk-ovl) pos (overlay-end chunk-ovl)))
;;;     (mumamo-msgfntfy "mumamo-create-chunk-at %s, chunk-ovl=%s, major=%s" pos chunk-ovl (overlay-get chunk-ovl 'mumamo-major-mode))
    (assert (mumamo-chunk-major-mode chunk-ovl))
    chunk-ovl))

(defun mumamo-create-chunk-at (pos)
  "Create and return a new chunk at POS.
There must not be an old chunk there.  Mark for refontification."
  (assert (not (mumamo-get-existing-chunk-at pos)))
  (mumamo-msgfntfy "mumamo-create-chunk-at %s" pos)
  (let ((new-chunk-values (mumamo-create-chunk-values-at pos))
        (new-chunk))
    (mumamo-adjust-old-chunks new-chunk-values nil nil)
    (setq new-chunk (mumamo-create-chunk-from-chunk-values new-chunk-values))
    (mumamo-mark-for-refontification (overlay-start new-chunk) (overlay-end new-chunk))
    new-chunk))

(defun mumamo-get-existing-chunk-at (pos)
  "Return existing chunk at POS if any."
  (let ((chunk-ovl))
    (when (= pos (point-max))
      (setq pos (1- pos)))
    (dolist (o (overlays-at pos))
      (unless chunk-ovl
        (when (mumamo-chunk-major-mode o)
          (setq chunk-ovl o))))
    chunk-ovl))

(defun mumamo-get-chunk-at (pos)
  "Return chunk overlay at POS.
Create it if it does not exist.  How to do this is governed by
`mumamo-current-chunk-family'.

A mumamo chunk is an Emacs overlay with some properties telling
how mumamo should handle the chunk during fontification,
indentation etc."
  (let ((chunk-ovl (mumamo-get-existing-chunk-at pos)))
    (if chunk-ovl
        ;;(mumamo-msgfntfy "existing %s %s" pos chunk-ovl)
        (unless (and (<= (overlay-start chunk-ovl) pos)
                     (<= pos (overlay-end chunk-ovl)))
          (error "mumamo-get-chunk-at: start=%s, pos=%s, end=%s"
                   (overlay-start chunk-ovl) pos (overlay-end chunk-ovl)))
      (setq chunk-ovl (mumamo-create-chunk-at pos)))
    chunk-ovl))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Chunk and chunk family properties

(defsubst mumamo-chunk-major-mode (chunk)
  "Get major mode specified in CHUNK."
  (assert chunk)
  (assert (overlay-buffer chunk))
  (overlay-get chunk 'mumamo-major-mode))

;; (defsubst mumamo-chunk-is-closed (chunk)
;;   (assert chunk)
;;   (assert (overlay-buffer chunk))
;;   (overlay-get chunk 'mumamo-is-closed))

(defvar mumamo-current-chunk-family nil
  "The currently used chunk family.
See `mumamo-set-chunk-family' for more information.")
(make-variable-buffer-local 'mumamo-current-chunk-family)
(put 'mumamo-current-chunk-family 'permanent-local t)

(defvar mumamo-main-major-mode nil)
(make-variable-buffer-local 'mumamo-main-major-mode)
(put 'mumamo-main-major-mode 'permanent-local t)

(defun mumamo-main-major-mode ()
  "Return major mode used when there are no chunks."
  (let ((main (cadr mumamo-current-chunk-family)))
    (if main
        main
      mumamo-main-major-mode)))

(defun mumamo-unset-chunk-family ()
  "Set chunk family to nil, ie undecided."
  (interactive)
  (setq mumamo-current-chunk-family nil))

(defun mumamo-define-chunks (chunk-family)
  "Set the CHUNK-FAMILY used to dived the buffer."
  (setq mumamo-current-chunk-family chunk-family)
;;;   (require 'rngalt nil t)
;;;   (when (featurep 'rngalt)
;;;     (when (and (boundp 'rngalt-validation-header)
;;;                rngalt-validation-header)
;;;       (rngalt-update-validation-header-overlay)))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; General chunk search routines

;; search start forward

;;(defun mumamo-search-fw-exc-start-str (pos max marker)
(defun mumamo-chunk-start-fw-str (pos max marker)
  "General chunk function helper.
A chunk function helper like this can be used in
`mumamo-find-possible-chunk' to find the borders of a chunk.
There are several functions like this that comes with mumamo.
Their names tell what they do.  Lets look at the parts of the
name of this function:

  mumamo-chunk: All this helper functions begins so
  -start-: Search for the start of a chunk
  -fw-: Search forward
  -str: Search for a string

Instead of '-start-' there could be '-end-', ie end.
Instead of '-fw-' there could be '-bw-', ie backward.
Instead of '-str' there could be '-re', ie regular expression.

There could also be a '-inc' at the end of the name.  If the name
ends with this then the markers should be included in the chunks,
otherwise not.

The argument POS means where to start the search.  MAX means how
far to search (when searching backwards the argument is called
'min' instead).  MARKER is a string or regular expression (see
the name) to search for."
  (assert (stringp marker))
  (goto-char (- pos (length marker)))
  (search-forward marker max t))

;;(defun mumamo-re-search-fw-exc-start-str (pos max marker)
(defun mumamo-chunk-start-fw-re (pos max marker)
  "General chunk function helper.
See `mumamo-chunk-start-fw-str' for more information and the
meaning of POS, MAX and MARKER."
  (assert (stringp marker))
  (goto-char (- pos (length marker)))
  (re-search-forward marker max t))

;;(defun mumamo-search-fw-exc-start-str-inc (pos max marker)
(defun mumamo-chunk-start-fw-str-inc (pos max marker)
  "General chunk function helper.
See `mumamo-chunk-start-fw-str' for more information and the
meaning of POS, MAX and MARKER."
  (assert (stringp marker))
  (goto-char pos)
  (let ((start (search-forward marker max t)))
    (when start (setq start (- start (length marker))))))

;; search start backward

;;(defun mumamo-search-bw-exc-start-str (pos min marker)
(defun mumamo-chunk-start-bw-str (pos min marker)
  "General chunk function helper.
See `mumamo-chunk-start-fw-str' for more information and the
meaning of POS, MIN and MARKER."
  (assert (stringp marker))
  (let (start-in)
    (goto-char pos)
    (setq start-in (search-backward marker min t))
    (when start-in
      ;; do not include the marker
      (setq start-in (+ start-in (length marker))))
    start-in))

;;(defun mumamo-re-search-bw-exc-start-str (pos min marker)
(defun mumamo-chunk-start-bw-re (pos min marker)
  "General chunk function helper.
See `mumamo-chunk-start-fw-str' for more information and the
meaning of POS, MIN and MARKER."
  (assert (stringp marker))
  (let (start-in)
    (goto-char pos)
    (setq start-in (re-search-backward marker min t))
    (when start-in
      ;; do not include the marker
      (setq start-in (match-end 0)))
    start-in))

;;(defun mumamo-search-bw-exc-start-str-inc (pos min marker)
(defun mumamo-chunk-start-bw-str-inc (pos min marker)
  "General chunk function helper.
See `mumamo-chunk-start-fw-str' for more information and the
meaning of POS, MIN and MARKER."
  (assert (stringp marker))
  (goto-char (+ pos (length marker)))
  (search-backward marker min t))

;; search end forward

;;(defun mumamo-search-fw-exc-end-str (pos max marker)
(defun mumamo-chunk-end-fw-str (pos max marker)
  "General chunk function helper.
See `mumamo-chunk-start-fw-str' for more information and the
meaning of POS, MAX and MARKER."
  (assert (stringp marker))
  (goto-char (1+ pos)) ;; 1+ cause otherwise ?> is at point
  (let (end-in)
    (setq end-in (search-forward marker max t))
    (when end-in
      ;; do not include the marker
      (setq end-in (- end-in (length marker))))
    end-in))

;;(defun mumamo-re-search-fw-exc-end-str (pos max marker)
(defun mumamo-chunk-end-fw-re (pos max marker)
  "General chunk function helper.
See `mumamo-chunk-start-fw-str' for more information and the
meaning of POS, MAX and MARKER."
  (assert (stringp marker))
  (goto-char (1+ pos)) ;; 1+ cause otherwise ?> is at point
  (let (end-in)
    (setq end-in (re-search-forward marker max t))
    (when end-in
      ;; do not include the marker
      (setq end-in (match-beginning 0)))
    end-in))

;;(defun mumamo-search-fw-exc-end-str-inc (pos max marker)
(defun mumamo-chunk-end-fw-str-inc (pos max marker)
  "General chunk function helper.
See `mumamo-chunk-start-fw-str' for more information and the
meaning of POS, MAX and MARKER."
  (assert (stringp marker))
  ;;(goto-char (1+ pos)) ;; 1+ cause otherwise ?> is at point
  (goto-char (1+ (- pos (length marker))))
  (search-forward marker max t))

;; search start backward

;;(defun mumamo-search-bw-exc-end-str (pos min marker)
(defun mumamo-chunk-end-bw-str (pos min marker)
  "General chunk function helper.
See `mumamo-chunk-start-fw-str' for more information and the
meaning of POS, MIN and MARKER."
  (assert (stringp marker))
  (goto-char (+ pos (length marker)))
  (search-backward marker min t))

;;(defun mumamo-re-search-bw-exc-end-str (pos min marker)
(defun mumamo-chunk-end-bw-re (pos min marker)
  "General chunk function helper.
See `mumamo-chunk-start-fw-str' for more information and the
meaning of POS, MIN and MARKER."
  (assert (stringp marker))
  (goto-char (+ pos (length marker)))
  (re-search-backward marker min t))

;;(defun mumamo-search-bw-exc-end-str-inc (pos min marker)
(defun mumamo-chunk-end-bw-str-inc (pos min marker)
  "General chunk function helper.
See `mumamo-chunk-start-fw-str' for more information and the
meaning of POS, MIN and MARKER."
  (assert (stringp marker))
  (goto-char pos)
  (let ((end (search-backward marker min t)))
    (when end (setq end (+ end (length marker))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; General chunk routines

(defvar mumamo-known-chunk-start nil "Internal use only!.")

;;; The main generic chunk routine
(defun mumamo-find-possible-chunk (pos
                                   min max
                                   bw-exc-start-fun
                                   bw-exc-end-fun
                                   fw-exc-start-fun
                                   fw-exc-end-fun)
  "Return list describing a possible chunk that includes POS.
No notice is taken about existing chunks and no chunks are
created.  The description returned is for the smallest possible
chunk which is delimited by the function parameters.

POS must be between MIN and MAX.

The function BW-EXC-START-FUN takes two parameters, POS and
MIN.  It should search backward from POS, bound by MIN, for
exception start and return a cons \(found-pos . exception-mode).

The functions BW-EXC-END-FUN, FW-EXC-START-FUN and FW-EXC-END-FUN
should search for exception start or end, forward resp
backward.  Those three should return just the position found.

For all four functions the position returned should be nil if
search fails.

Return as a list with values

  \(START END EXCEPTION-MODE NARROW END-OF-EXCEPTION POS)

The bounds START and END are where the exception starts or stop.
Either of them may be nil, in which case this is equivalent to
`point-min' respectively `point-max'.

If EXCEPTION-MODE is non-nil that is the submode for this
range.  Otherwise the main major mode should be used for this
chunk.

This routine is used by to create new members for chunk
families.  If you want to add a new chunk family you could most
often do that by writing functions for this routine.  Please see
the many examples in mumamo.el for how this can be done."
  (mumamo-condition-case err
      (progn
        (assert (and (<= min pos) (<= pos max))
                nil
                "mumamo-chunk: min=%s, pos=%s, max=%s, bt=%S"
                min pos max (with-output-to-string (backtrace)))
        (let (
              start-in-cons
              exc-mode
              start-in start-out
              end-in end-out
              start end
              end-of-exception
              wants-end-type
              )
          ;; ;; find start of range
          ;;
          ;; start normal
          ;;
          ;; FIX-ME: Quick fix to allow the use of known chunk starts
          ;; (mainly for after change fontification):
          (if mumamo-known-chunk-start
              (let ((known-start (car mumamo-known-chunk-start)))
                (setq exc-mode (cdr mumamo-known-chunk-start))
                (if exc-mode
                    (setq start-out known-start)
                  (setq start-in known-start)))
            (setq start-out (funcall bw-exc-end-fun pos min))
            (when start-out
              (assert (<= start-out pos))
              (assert (<= min start-out)))
            (when start-out (setq min start-out)) ;; minimize next search bw
            ;; start exception
            (setq start-in-cons (funcall bw-exc-start-fun pos min))
            (setq start-in (car start-in-cons))
            (when start-in
              (assert (<= start-in pos))
              (assert (<= min start-in)))
            ;; compare
            (cond
             ((and start-in start-out)
              (if (< start-in start-out)
                  (setq start start-out)
                (setq exc-mode (cdr start-in-cons))
                (setq start start-in)))
             (start-in
              (setq exc-mode (cdr start-in-cons))
              (setq start start-in))
             (start-out
              (setq start start-out))
             ))
          ;; ; find end of range
          ;;
          ;; what end type is acceptable?  three possible values: nil means
          ;; any end type, the other values are 'end-normal and
          ;; 'end-exception.
          (when start
            (if exc-mode
                (setq wants-end-type 'end-exception)
              (setq wants-end-type 'end-normal)))
          ;; end exception
          (when (or (not wants-end-type)
                    (eq wants-end-type 'end-exception))
            (setq max end-in) ;; minimize next search fw
            (setq end-in (funcall fw-exc-end-fun pos max)))
          ;; end normal
          (when (or (not wants-end-type)
                    (eq wants-end-type 'end-normal))
            (setq end-out (funcall fw-exc-start-fun pos max)))
          ;; compare
          (cond
           ((and end-in end-out)
            (if (> end-in end-out)
                (setq end end-out)
              (setq end-of-exception t)
              (setq end end-in)))
           (end-in
            (setq end-of-exception t)
            (setq end end-in))
           (end-out
            (setq end end-out))
           )
          ;; check
          (when start (assert (<= start pos)))
          (when end   (assert (<= pos end)))
          (goto-char pos)
          (list start end exc-mode nil end-of-exception pos)))
    (error
     (mumamo-display-error 'mumamo-chunk "%s"
                           (error-message-string err)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Easy chunk defining

;; Fix-me: test this more
(eval-when-compile
  (defvar mumamo-easy-start-static)
  (defvar mumamo-easy-end-static)
  (defvar mumamo-easy-exc-mode)
  )

(defun mumamo-easy-bw-exc-start (pos min)
  "Helper for `mumamo-easy-make-chunk-fun.
POS is where to start search and MIN is where to stop."
  (let ((exc-start (mumamo-chunk-start-bw-str pos min mumamo-easy-start-static)))
    (when (and exc-start
               (<= exc-start pos))
      (cons exc-start mumamo-easy-exc-mode))))

(defun mumamo-easy-bw-exc-end (pos min)
  "Helper for `mumamo-easy-make-chunk-fun.
POS is where to start search and MIN is where to stop."
   (mumamo-chunk-end-bw-str pos min mumamo-easy-end-static))

(defun mumamo-easy-fw-exc-start (pos max)
  "Helper for `mumamo-easy-make-chunk-fun.
POS is where to start search and MAX is where to stop."
  (mumamo-chunk-start-fw-str pos max mumamo-easy-start-static))

(defun mumamo-easy-fw-exc-end (pos max)
  "Helper for `mumamo-easy-make-chunk-fun.
POS is where to start search and MAX is where to stop."
   (mumamo-chunk-end-fw-str pos max mumamo-easy-end-static))

(defmacro mumamo-easy-make-chunk-fun (chunk-fun-symbol
                                      start-static
                                      end-static
                                      chunk-mode)
  "Create a new chunk function.
This is supposed to be an easy and quick way to create a new chunk
function named CHUNK-FUN-SYMBOL.

The boundary strings START-STATIC and END-STATIC are just plain
strings with no regular expressions so it is more easy than
flexible.  You have to choose this as something that does not
interfere with your text.

The mode in the chunk will be CHUNK-MODE.

See also `mumamo-quick-static-chunk'."
  (declare (indent 1) (debug t))
  `(defun ,chunk-fun-symbol (pos min max)
     (let ((mumamo-easy-start-static ,(if (symbolp start-static) (symbol-value start-static) start-static))
           (mumamo-easy-end-static   ,(if (symbolp end-static  ) (symbol-value end-static)   end-static  ))
           (mumamo-easy-exc-mode     ,(if (symbolp chunk-mode  ) (symbol-value chunk-mode)   chunk-mode  )))
       (mumamo-find-possible-chunk pos min max
                                   'mumamo-easy-bw-exc-start
                                   'mumamo-easy-bw-exc-end
                                   'mumamo-easy-fw-exc-start
                                   'mumamo-easy-fw-exc-end))))

(defun mumamo-chunk-attr=(pos min max attr= attr=is-regex attr-regex submode)
  "This should work similar to `mumamo-find-possible-chunk'.
See `mumamo-chunk-style=' for an example of use."
  ;; 1- for the case that pos is at the " before the attribute value.
  (mumamo-condition-case err
      (progn
        (goto-char (1- pos))
        (let ((prev-attr= (if attr=is-regex
                              (re-search-backward attr= min t)
                            (search-backward attr= min t)))
              prev-attr-sure
              next-attr=
              start
              end
              exc-mode
              exc-start-prev
              exc-end-prev
              exc-start-next
              exc-end-next
              )
          ;; make sure if we have find prev-attr= or not
          (while (and prev-attr=
                      (not prev-attr-sure))
            (if (not (search-backward "<" min t))
                (setq prev-attr-sure 'none)
              (if (looking-at attr-regex)
                  (setq prev-attr-sure 'found)
                (setq prev-attr= (if attr=is-regex
                                     (re-search-backward attr= min t)
                                   (search-backward attr= min t))))))
          ;; find prev change and if inside style= the next change
;;;           (when (and prev-attr=
;;;                      (search-backward "<" min t))
;;;             (when (looking-at attr-regex)
          (when (and prev-attr= (eq prev-attr-sure 'found))
              (setq exc-start-prev (match-beginning 1))
              (setq exc-end-prev   (match-end 1))
              (when (<= exc-start-prev pos)
                (if (>= pos exc-end-prev)
                    (setq start exc-end-prev)
                  (setq exc-mode submode)
                  (setq start exc-start-prev)
                  (setq end exc-end-prev))))
            ;;)
          ;; find next change
          (unless end
            (if start
                (goto-char start)
              (goto-char pos)
              (search-backward "<" min t))
            (setq next-attr= (if attr=is-regex
                                 (re-search-forward attr= max t)
                               (search-forward attr= max t)))
            (when (and next-attr=
                       (search-backward "<" min t))
              (when (looking-at attr-regex)
                (setq end (match-beginning 1)))))
          (when start (assert (<= start pos) t))
          (when end   (assert (<= pos end) t))
          (goto-char pos)
          (list start end exc-mode t nil pos)))
    (error
     (mumamo-display-error 'mumamo-chunk-attr= "%s"
                           (error-message-string err)))))

(defun mumamo-quick-static-chunk (pos min max
                                  begin-mark end-mark inc mode)
  "Quick way to make a chunk function with static dividers.
Here is an example of how to use it:

  (defun mumamo-chunk-embperl-<- (pos min max)
    \"Find [- ... -], return range and perl-mode.\"
    (mumamo-quick-static-chunk pos min max \"[-\" \"-]\" nil 'perl-mode))

As you can see POS, MIN and MAX comes from the function you define.

BEGIN-MARK should be a string that begins the chunk.
END-MARK should be a string that ends the chunk.
If INC is non-nil then the dividers are included in the chunks.
MODE should be the major mode for the chunk.

See also `mumamo-easy-make-chunk-fun'."
  (let ((search-bw-exc-start
         (lambda (pos min)
           (let ((exc-start
                  (if inc
                      (mumamo-chunk-start-bw-str-inc pos min begin-mark)
                    (mumamo-chunk-start-bw-str pos min begin-mark))))
             (when (and exc-start
                        (<= exc-start pos))
               (cons exc-start mode)))))
        (search-bw-exc-end
         (lambda (pos min)
           (if inc
               (mumamo-chunk-end-bw-str-inc pos min end-mark)
             (mumamo-chunk-end-bw-str pos min end-mark))))
        (search-fw-exc-start
         (lambda (pos max)
           (if inc
               (mumamo-chunk-start-fw-str-inc pos max begin-mark)
             (mumamo-chunk-start-fw-str pos max begin-mark))))
        (search-fw-exc-end
         (lambda (pos max)
           (if inc
               (mumamo-chunk-end-fw-str-inc pos max end-mark)
             (mumamo-chunk-end-fw-str pos max end-mark))))
        )
    (mumamo-find-possible-chunk pos min max
                                search-bw-exc-start
                                search-bw-exc-end
                                search-fw-exc-start
                                search-fw-exc-end)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Specific chunk search routines


;;;; asp

;; fix-me: make this separate since it is proprietory?
(defun mumamo-chunk-asp (pos min max)
  "Find <% ... %>.  Return range and 'asp-js-mode."
  (mumamo-find-possible-chunk pos min max
                              'mumamo-search-bw-exc-start-asp
                              'mumamo-search-bw-exc-end-jsp
                              'mumamo-search-fw-exc-start-jsp
                              'mumamo-search-fw-exc-end-jsp))

(defconst mumamo-asp-lang-marker
  (rx "<%@"
      (0+ space)
      "language"
      (0+ space)
      "="
      (0+ space)
      "\""
      (submatch (1+ (not (any "\""))))
      "\""
      (0+ space)))

(defun mumamo-search-bw-exc-start-asp (pos min)
  (let ((exc-start (mumamo-chunk-start-bw-str pos min "<%")))
    (when (and exc-start
               (<= exc-start pos))
      (let ((here (point))
            (mode 'asp-vb-mode)
            lang)
        (when (re-search-backward mumamo-asp-lang-marker nil t)
          (setq lang (downcase (match-string-no-properties 1)))
          (lwarn 't :warning "lang=%s" lang)
          (cond
           ((string= lang "javascript")
            (setq mode 'asp-js-mode))
           )
          )
        (cons exc-start mode)))))


;;;; asp <script ...>

(defconst mumamo-asp-script-tag-start-regex
  (rx "<script"
      space
      (0+ (not (any ">")))
      "language"
      (0+ space)
      "="
      (0+ space)
      ?\"
      ;;(or "text" "application")
      ;;"/"
      ;;(or "javascript" "ecmascript")
      ;; "text/javascript"
      (or "javascript" "vbscript")
      ?\"
      (0+ (not (any ">")))
      ">"
      ;; FIX-ME: Commented out because of bug in Emacs
      ;;
      ;;(optional (0+ space) "<![CDATA[" )
      ))

(defun mumamo-asp-search-bw-exc-start-inlined-script (pos min)
  (goto-char (+ pos 7))
  (let ((marker-start (search-backward "<script" min t))
        (exc-mode 'asp-vb-mode)
        exc-start
        lang)
    (when marker-start
      (when (looking-at mumamo-script-tag-start-regex)
        (setq lang (downcase (match-string-no-properties 1)))
        (cond
         ((string= lang "javascript")
          (setq exc-mode 'asp-js-mode))
         )
        (setq exc-start (match-end 0))
        (goto-char exc-start)
        (when (<= exc-start pos)
          (cons (point) exc-mode))
        ))))

(defun mumamo-asp-chunk-inlined-script (pos min max)
  "Find <script language=...  runat=...>...</script>.  Return 'asp-js-mode."
  (mumamo-find-possible-chunk pos min max
                              'mumamo-asp-search-bw-exc-start-inlined-script
                              'mumamo-search-bw-exc-end-inlined-script
                              'mumamo-search-fw-exc-start-inlined-script
                              'mumamo-search-fw-exc-end-inlined-script))






;; TeX Meta

(defun mumamo-search-bw-textext-start (pos min)
  (let ((exc-start (mumamo-chunk-start-bw-str pos min "textext(\""))
        (exc-mode 'plain-tex-mode))
    (when exc-start
      (when (<= exc-start pos)
        (cons exc-start exc-mode)))))

(defconst mumamo-textext-end-regex
  (rx "textext("
      (0+
       (0+ (not (any "\"()")))
       ?\"
       (0+ (not (any "\"")))
       ?\"
       )
      (0+ (not (any "\"()")))
      ")"))

(defun mumamo-textext-test-is-end (pos)
  (when pos
    (let ((here (point))
          hit)
      (goto-char (+ 2 pos))
      (when (looking-back mumamo-textext-end-regex)
        (setq hit t))
      (goto-char here)
      (when hit pos))))

(defun mumamo-search-bw-textext-end (pos min)
  (let ((end (mumamo-chunk-end-bw-str pos min "\")"))
        res)
    (while (and end
                (not (setq res (mumamo-textext-test-is-end end))))
      (setq end (mumamo-chunk-end-bw-str (1- end) min "\")")))
    res))

(defun mumamo-search-fw-textext-start (pos max)
  (mumamo-chunk-start-fw-str pos max "textext(\""))

(defun mumamo-search-fw-textext-end (pos max)
  (let ((end (mumamo-chunk-end-fw-str pos max "\")")))
    (mumamo-textext-test-is-end end)))

(defun mumamo-chunk-textext (pos min max)
  "Find textext or TEX chunks.  Return range and 'plain-tex-mode."
  (mumamo-find-possible-chunk pos min max
                              'mumamo-search-bw-textext-start
                              'mumamo-search-bw-textext-end
                              'mumamo-search-fw-textext-start
                              'mumamo-search-fw-textext-end))

(defun mumamo-search-bw-verbatimtex-start (pos min)
  (let ((exc-start (mumamo-chunk-start-bw-str pos min "\nverbatimtex"))
        (exc-mode 'plain-tex-mode))
    (when exc-start
      (when (<= exc-start pos)
        (cons exc-start exc-mode)))))

(defun mumamo-search-bw-verbatimtex-end (pos min)
  (mumamo-chunk-end-bw-str pos min "\netex"))

(defun mumamo-search-fw-verbatimtex-start (pos max)
  (mumamo-chunk-start-fw-str pos max "\nverbatimtex"))

(defun mumamo-search-fw-verbatimtex-end (pos max)
  (mumamo-chunk-end-fw-str pos max "\netex"))

(defun mumamo-chunk-verbatimtex (pos min max)
  "Find verbatimtex - etex chunks.  Return range and 'plain-tex-mode."
  (mumamo-find-possible-chunk pos min max
                              'mumamo-search-bw-verbatimtex-start
                              'mumamo-search-bw-verbatimtex-end
                              'mumamo-search-fw-verbatimtex-start
                              'mumamo-search-fw-verbatimtex-end))

(defun mumamo-search-bw-btex-start (pos min)
  (let ((exc-start (mumamo-chunk-start-bw-str pos min "\nverbatimtex"))
        (exc-mode 'plain-tex-mode))
    (when exc-start
      (when (<= exc-start pos)
        (cons exc-start exc-mode)))))

(defun mumamo-search-bw-btex-end (pos min)
  (mumamo-chunk-end-bw-str pos min "\netex"))

(defun mumamo-search-fw-btex-start (pos max)
  (mumamo-chunk-start-fw-str pos max "\nverbatimtex"))

(defun mumamo-search-fw-btex-end (pos max)
  (mumamo-chunk-end-fw-str pos max "\netex"))

(defun mumamo-chunk-btex (pos min max)
  "Find btex - etex chunks."
  (mumamo-find-possible-chunk pos min max
                              'mumamo-search-bw-btex-start
                              'mumamo-search-bw-btex-end
                              'mumamo-search-fw-btex-start
                              'mumamo-search-fw-btex-end))


;;;; perl here doc

(defun mumamo-chunk-perl-here-html (pos min max)
  "Find perl here docs, still a little bit EXPERIMENTAL.
See `mumamo-perl-here-doc-modes' for how to customize the choice
of major mode in the perl here document.

* Note: This used to be slow, but after fixing a bug in perl here
  doc chunk dividing it seems to work ok."
  (mumamo-msgfntfy "mumamo-chunk-perl-here-html start")
  (let ((r (mumamo-chunk-perl-here-doc pos min max)))
    (mumamo-msgfntfy "  perl-here stop")
    r))

(defun mumamo-perl-here-doc-mode (marker)
  (let ((mode (catch 'mode
                (dolist (rec mumamo-perl-here-doc-modes)
                  (let ((regexp (nth 0 rec))
                        (mode   (nth 1 rec)))
                    (when (string-match regexp marker)
                      (throw 'mode mode)))))))
    (if mode
        mode
      'fundamental-mode)))

(defun mumamo-chunk-perl-here-doc (pos min max)
  "This should work similar to `mumamo-find-possible-chunk'."
  (mumamo-condition-case err
      (let ((old-point (point)))
        (goto-char pos)
        ;; Adjust for beeing inside an <<...;
        ;;(beginning-of-line)
        ;;(when (looking-at (rx (1+ (not (any "<"))) "<<" (submatch (1+ (not (any "^;")))) ";" line-end)
        (end-of-line)
        (beginning-of-line)
        (let ((prev-<< (search-backward "<<" min t))
              next-<<
              here-doc-end-mark-end
              here-doc-end-mark-start
              here-doc-start-mark-start
              here-doc-start-mark-end
              here-doc-mark
              start
              end
              exc-mode
              )
          ;; find prev change and end of that perl here doc
          (when prev-<<
            (when (looking-at (concat "<<[[:space:]]*\\([^\n;]*\\);"))
              (setq here-doc-start-mark-start (match-beginning 0))
              (setq here-doc-start-mark-end   (match-end 0))
              (setq here-doc-mark  (buffer-substring-no-properties
                                    (match-beginning 1)
                                    (match-end 1)))
              (end-of-line)
              (setq here-doc-end-mark-end (search-forward here-doc-mark max t))
              (when (and here-doc-end-mark-end
                         (eolp))
                (beginning-of-line)
                (if (looking-at here-doc-mark)
                    (setq here-doc-end-mark-start (point))
                  (setq here-doc-end-mark-end nil)))
              (if (and here-doc-end-mark-start
                       (<= here-doc-end-mark-start pos))
                  (progn
                    (setq start here-doc-end-mark-start)
                    )
                (setq exc-mode (mumamo-perl-here-doc-mode here-doc-mark))
                (setq start (1+ here-doc-start-mark-end))
                (when here-doc-end-mark-start
                  (setq end here-doc-end-mark-start))
                )))
          (unless end
            (goto-char pos)
            (beginning-of-line)
            (setq next-<< (search-forward "<<" max t))
            (when next-<<
              (when (looking-at (concat "[[:space:]]*\\([^\n;]*\\);"))
                (setq end (1+ (match-end 0))))))
          (when start (assert (<= start pos) t))
          (when end   (assert (<= pos end) t))
          (goto-char old-point)
          (list start end exc-mode t nil pos)))
    (error (mumamo-display-error 'mumamo-chunk-perl-here-doc "%s" (error-message-string err)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Changing the major mode that the user sees

(defvar mumamo-unread-command-events-timer nil)
(make-variable-buffer-local 'mumamo-unread-command-events-timer)

(defun mumamo-unread-command-events (command-keys new-major old-last-command)
  "Sync new keymaps after changing major mode in a timer.
Also tell new major mode.

COMMAND-KEYS is the keys entered after last command and the call
to `mumamo-idle-set-major-mode' \(which is done in an idle
timer).  Those keys are added to `unread-command-events' so they
can be used in the new keymaps.

NEW-MAJOR mode is the new major mode.

OLD-LAST-COMMAND is the value of `last-command' after switching
major mode.  \(This is cleared by the function `top-level' so
this function will not see it since it is run in a timer.)"
  (mumamo-condition-case err
      (progn
        ;; last-command seems to be cleared by top-level:
        (unless last-command
          (setq last-command old-last-command))
        (when (< 0 (length command-keys))
          (setq unread-command-events (append command-keys nil)))
        (message "Switched to %s" new-major))
    (error
     (let ((mumamo-display-error-lwarn t))
       (mumamo-display-error 'mumamo-unread-command-events "err=%s" err)))))

(defvar mumamo-idle-set-major-mode-timer nil)
(make-variable-buffer-local 'mumamo-idle-set-major-mode-timer)
(put 'mumamo-idle-set-major-mode-timer 'permanent-local t)

(defun mumamo-idle-set-major-mode (buffer)
  "Set major mode from mumamo chunk when Emacs is idle.
This is done in buffer BUFFER."
  (mumamo-msgfntfy "mumamo-idle-set-major-mode b=%s" buffer)
  (with-current-buffer buffer
    (mumamo-condition-case err
        (let* ((ovl (mumamo-get-chunk-at (point)))
               (major (mumamo-chunk-major-mode ovl)))
          (unless (eq major major-mode)
            (mumamo-set-major major)
            ;; sync keymap
            (when (eq buffer (current-buffer))
              ;; Fix-me: Should unread-command-events be used here
              ;; instead of this-command-keys?
              (let ((this-command-keys (this-command-keys)))
                (when (timerp mumamo-unread-command-events-timer)
                  (cancel-timer mumamo-unread-command-events-timer))
                (setq mumamo-unread-command-events-timer
                      (run-with-idle-timer
                       0 nil
                       'mumamo-unread-command-events
                       this-command-keys major last-command))
                (top-level)
                ))))
      (error
       (mumamo-display-error 'mumamo-idle-set-major-mode
                             "cb=%s, err=%s" (current-buffer) err)))))

(defun mumamo-request-idle-set-major-mode ()
  "Setup to change major mode from chunk when Emacs is idle."
  (when (timerp mumamo-idle-set-major-mode-timer)
    (cancel-timer mumamo-idle-set-major-mode-timer))
  (setq mumamo-idle-set-major-mode-timer
        (run-with-idle-timer
         mumamo-set-major-mode-delay
         nil
         ;;'mumamo-idle-set-major-mode (mumamo-get-chunk-at (point)))))
         'mumamo-idle-set-major-mode (current-buffer))))

(defvar mumamo-done-first-set-major nil)
(make-variable-buffer-local 'mumamo-done-first-set-major)
(put 'mumamo-done-first-set-major 'permanent-local t)

(defvar mumamo-multi-major-mode nil
  "The function that handles multiple major modes.
If this is nil then multiple major modes in the buffer is not
handled by mumamo.

Set by functions defined by `define-mumamo-multi-major-mode'.")
(make-variable-buffer-local 'mumamo-multi-major-mode)
(put 'mumamo-multi-major-mode 'permanent-local t)

(defun mumamo-set-major-post-command ()
  "Change major mode if necessary after a command.
Just request a change of major mode when Emacs is idle if
`mumamo-set-major-mode-delay' is non-nil."
  (let* ((ovl (mumamo-get-chunk-at (point)))
         (major (mumamo-chunk-major-mode ovl)))
    (if (not major)
        (lwarn '(mumamo-set-major-post-command)
               :error "major=%s" major)
      (unless (and mumamo-done-first-set-major
                   (eq major-mode major))
        (if mumamo-done-first-set-major
            (if (<= 0 mumamo-set-major-mode-delay)
                (progn
                  (mumamo-request-idle-set-major-mode))
              (mumamo-set-major major)
              (message "ySwitched to %s" major-mode))
          (mumamo-set-major major))))))

(defun mumamo-post-command ()
  "Run this in `post-command-hook'.
Change major mode if necessary."
  (mumamo-msgfntfy "mumamo-post-command")
  (when mumamo-multi-major-mode
    (mumamo-condition-case err
        (if font-lock-mode
            (mumamo-set-major-post-command)
          (mumamo-on-font-lock-off))
      (error
       (message "mumamo-post-command %s" (error-message-string err))
       (lwarn 'mumamo-post-command :warning "%s"
                             (error-message-string err))))))

(defvar mumamo-set-major-running nil
  "Internal use.  Handling of mumamo turn off.")

(defun mumamo-change-major-function ()
  "Function added to `change-major-mode-hook'.
Remove mumamo when changing to a new major mode if the change is
not done because point was to a new chunk."
  (unless mumamo-set-major-running
    (mumamo-turn-off-actions)))

(defun mumamo-derived-from-mode (major from-mode)
  "Return t if major mode MAJOR is derived from FROM-MODE."
  (let ((major-mode major))
    (derived-mode-p from-mode)))

;; This is the new version of add-hook.
;; Fix-me: Is this in Emacs 22.2?
(unless (> emacs-major-version 22)
  (defun add-hook (hook function &optional append local)
    "Add to the value of HOOK the function FUNCTION.
FUNCTION is not added if already present.
FUNCTION is added (if necessary) at the beginning of the hook list
unless the optional argument APPEND is non-nil, in which case
FUNCTION is added at the end.

The optional fourth argument, LOCAL, if non-nil, says to modify
the hook's buffer-local value rather than its default value.
This makes the hook buffer-local if needed, and it makes t a member
of the buffer-local value.  That acts as a flag to run the hook
functions in the default value as well as in the local value.

HOOK should be a symbol, and FUNCTION may be any valid function.  If
HOOK is void, it is first set to nil.  If HOOK's value is a single
function, it is changed to a list of functions."
    (or (boundp hook) (set hook nil))
    (or (default-boundp hook) (set-default hook nil))
    (if local (unless (local-variable-if-set-p hook)
                (set (make-local-variable hook) (list t)))
      ;; Detect the case where make-local-variable was used on a hook
      ;; and do what we used to do.
      (unless (and (consp (symbol-value hook)) (memq t (symbol-value hook)))
        (setq local t)))
    (let ((hook-value (if local (symbol-value hook) (default-value hook))))
      ;; If the hook value is a single function, turn it into a list.
      (when (or (not (listp hook-value)) (eq (car hook-value) 'lambda))
        (setq hook-value (list hook-value)))
      ;; Do the actual addition if necessary
      (unless (member function hook-value)
        (setq hook-value
              (if append
                  (append hook-value (list function))
                (cons function hook-value))))
      ;; Set the actual variable
      (if local
          (progn
            ;; If HOOK isn't a permanent local,
            ;; but FUNCTION wants to survive a change of modes,
            ;; mark HOOK as partially permanent.
            (and (symbolp function)
                 (get function 'permanent-local-hook)
                 (not (get hook 'permanent-local))
                 (put hook 'permanent-local 'permanent-local-hook))
            (set hook hook-value))
        (set-default hook hook-value))))
  )


(defvar mumamo-survive-hooks
  '(
;;     activate-mark-hook after-change-functions after-save-hook
;;     before-save-functions auto-save-hook before-revert-hook
;;     buffer-access-fontify-functions calendar-load-hook
;;     command-line-functions compilation-finish-function
;;     deactivate-mark-hook find-file-hook
;;     find-file-not-found-functions first-change-hook
;;     kbd-macro-termination-hook kill-buffer-hook
;;     kill-buffer-query-functions menu-bar-update-hook
;;     post-command-hook pre-abbrev-expand-hook pre-command-hook
;;     write-contents-functions write-file-functions
;;     write-region-annotate-functions
;;     c-special-indent-hook
    ))

;;
;; Emulation modes
;;
;; These variables should have 'permanant-local t set in their
;; packages IMO, but now they do not have that.
(eval-after-load 'viper-cmd
  (progn
    (put 'viper-after-change-functions 'permanent-local t)
    (put 'viper-before-change-functions 'permanent-local t)
    ))
(eval-after-load 'viper
  (progn
    (put 'viper-post-command-hooks 'permanent-local t)
    (put 'viper-pre-command-hooks 'permanent-local t)
    ;;minor-mode-map-alist
    ;; viper-mode-string -- is already buffer local, globally void
    (put 'viper-mode-string 'permanent-local t)
    ))
    ;;viper-tut--part
(eval-after-load 'viper-init
  (progn
    (put 'viper-d-com 'permanent-local t)
    (put 'viper-last-insertion 'permanent-local t)
    (put 'viper-command-ring 'permanent-local t)
    (put 'viper-vi-intercept-minor-mode 'permanent-local t)
    (put 'viper-vi-basic-minor-mode 'permanent-local t)
    (put 'viper-vi-local-user-minor-mode 'permanent-local t)
    (put 'viper-vi-global-user-minor-mode 'permanent-local t)
    (put 'viper-vi-state-modifier-minor-mode 'permanent-local t)
    (put 'viper-vi-diehard-minor-mode 'permanent-local t)
    (put 'viper-vi-kbd-minor-mode 'permanent-local t)
    (put 'viper-insert-intercept-minor-mode 'permanent-local t)
    (put 'viper-insert-basic-minor-mode 'permanent-local t)
    (put 'viper-insert-local-user-minor-mode 'permanent-local t)
    (put 'viper-insert-global-user-minor-mode 'permanent-local t)
    (put 'viper-insert-state-modifier-minor-mode 'permanent-local t)
    (put 'viper-insert-diehard-minor-mode 'permanent-local t)
    (put 'viper-insert-kbd-minor-mode 'permanent-local t)
    (put 'viper-replace-minor-mode 'permanent-local t)
    (put 'viper-emacs-intercept-minor-mode 'permanent-local t)
    (put 'viper-emacs-local-user-minor-mode 'permanent-local t)
    (put 'viper-emacs-global-user-minor-mode 'permanent-local t)
    (put 'viper-emacs-kbd-minor-mode 'permanent-local t)
    (put 'viper-emacs-state-modifier-minor-mode 'permanent-local t)
    (put 'viper-vi-minibuffer-minor-mode 'permanent-local t)
    (put 'viper-insert-minibuffer-minor-mode 'permanent-local t)
    (put 'viper-automatic-iso-accents 'permanent-local t)
    (put 'viper-special-input-method 'permanent-local t)
    (put 'viper-intermediate-command 'permanent-local t)
    ;; already local: viper-undo-needs-adjustment
    (put 'viper-began-as-replace 'permanent-local t)
    ;; already local: viper-replace-overlay
    ;; already local: viper-last-posn-in-replace-region
    ;; already local: viper-last-posn-while-in-insert-state
    ;; already local: viper-sitting-in-replace
    (put 'viper-replace-chars-to-delete 'permanent-local t)
    (put 'viper-replace-region-chars-deleted 'permanent-local t)
    ;; Fix-me: Setting this loses the mark on the mode line, but it is
    ;; needed in mumamo.  Without it viper looses it state when moving
    ;; between chunks and a new major mode is set:
    (put 'viper-current-state 'permanent-local t)
    (put 'viper-cted 'permanent-local t)
    (put 'viper-current-indent 'permanent-local t)
    (put 'viper-preserve-indent 'permanent-local t)
    (put 'viper-auto-indent 'permanent-local t)
    (put 'viper-electric-mode 'permanent-local t)
    ;; already local: viper-insert-point
    ;; already local: viper-pre-command-point
    (put 'viper-com-point 'permanent-local t)
    (put 'viper-ex-style-motion 'permanent-local t)
    (put 'viper-ex-style-editing 'permanent-local t)
    (put 'viper-ESC-moves-cursor-back 'permanent-local t)
    (put 'viper-delete-backwards-in-replace 'permanent-local t)
    ;; already local: viper-related-files-and-buffers-ring
    (put 'viper-local-search-start-marker 'permanent-local t)
    (put 'viper-search-overlay 'permanent-local t)
    (put 'viper-last-jump 'permanent-local t)
    (put 'viper-last-jump-ignore 'permanent-local t)
    (put 'viper-minibuffer-current-face 'permanent-local t)
    ;; already local: viper-minibuffer-overlay
    (put 'viper-command-ring 'permanent-local t)
    (put 'viper-last-insertion 'permanent-local t)
    ))
(eval-after-load 'viper-keym
  (progn
    ;; already local: viper-vi-local-user-map
    ;; already local: viper-insert-local-user-map
    ;; already local: viper-emacs-local-user-map
    (put 'viper--key-maps 'permanent-local t)
    (put 'viper--intercept-key-maps 'permanent-local t)
    ;; already local: viper-need-new-vi-local-map
    ;; already local: viper-need-new-insert-local-map
    ;; already local: viper-need-new-emacs-local-map
    ))
(eval-after-load 'viper-mous
  (progn
    (put 'viper-mouse-click-search-noerror 'permanent-local t)
    (put 'viper-mouse-click-search-limit 'permanent-local t)
    ))
(eval-after-load 'viper-util
  (progn
    (put 'viper-syntax-preference 'permanent-local t)
    (put 'viper-non-word-characters 'permanent-local t)
    (put 'viper-ALPHA-char-class 'permanent-local t)
    ))

(eval-after-load 'cua-base
  (progn
    (put 'cua-inhibit-cua-keys 'permanent-local t)
    (put 'cua--explicit-region-start 'permanent-local t)
    (put 'cua--status-string 'permanent-local t)
    ))
;; This is for the defvar in ido.el:
(eval-after-load 'ido
  (progn
    (put 'cua-inhibit-cua-keys 'permanent-local t)
    ))
(eval-after-load 'cua-rect
  (progn
    (put 'cua--rectangle 'permanent-local t)
    (put 'cua--rectangle-overlays 'permanent-local t)
    ))
(eval-after-load 'edt
  (progn
    (put 'edt-select-mode 'permanent-local t)
    ))
(eval-after-load 'tpu-edt
  (progn
    (put 'tpu-newline-and-indent-p 'permanent-local t)
    (put 'tpu-newline-and-indent-string 'permanent-local t)
    (put 'tpu-saved-delete-func 'permanent-local t)
    (put 'tpu-buffer-local-map 'permanent-local t)
    (put 'tpu-mark-flag 'permanent-local t)
    ))
(eval-after-load 'vi
  (progn
    (put 'vi-add-to-mode-line 'permanent-local t)
    ;;Warning (mumamo-survive): Not a local variable: vi-scroll-amount
    ;;Warning (mumamo-survive): Not a local variable: vi-shift-width
    ;;Warning (mumamo-survive): Not a local variable: vi-ins-point
    ;;Warning (mumamo-survive): Not a local variable: vi-ins-length
    ;;Warning (mumamo-survive): Not a local variable: vi-ins-repetition
    ;;Warning (mumamo-survive): Not a local variable: vi-ins-overwrt-p
    ;;Warning (mumamo-survive): Not a local variable: vi-ins-prefix-code
    ;;Warning (mumamo-survive): Not a local variable: vi-last-change-command
    ;;Warning (mumamo-survive): Not a local variable: vi-last-shell-command
    ;;Warning (mumamo-survive): Not a local variable: vi-last-find-char
    ;;Warning (mumamo-survive): Not a local variable: vi-mark-alist
    ;;Warning (mumamo-survive): Not a local variable: vi-insert-state
    ;;Warning (mumamo-survive): Not a local variable: vi-mode-old-local-map
    ;;Warning (mumamo-survive): Not a local variable: vi-mode-old-mode-name
    ;;Warning (mumamo-survive): Not a local variable: vi-mode-old-major-mode
    ;;Warning (mumamo-survive): Not a local variable: vi-mode-old-case-fold
    ;;
    ))
(eval-after-load 'vi
  (progn
    (put 'vip-emacs-local-map 'permanent-local t)
    (put 'vip-insert-local-map 'permanent-local t)
    (put 'vip-insert-point 'permanent-local t)
    (put 'vip-com-point 'permanent-local t)
    (put 'vip-current-mode 'permanent-local t)
    (put 'vip-emacs-mode-line-buffer-identification 'permanent-local t)
    (put 'vip-current-major-mode 'permanent-local t)
    ))

;;
;; Minor modes that are not major mode specific
;;

;;(defconst mumamo-auto-survivors-hook nil)

;; (defmacro mumamo-make-change-major-survivor (name hook-fun-list var-list test-sym)
;;   "Define functions for major mode change survival.
;; Minor mode authors that want their buffer local
;; minor mode to survive when the major mode in the buffer is
;; changed can use this macro.

;; An example of such a minor mode may be flymake minor mode.  For
;; flymake this call can be used:

;;   (eval-after-load 'mumamo
;;     (mumamo-make-change-major-survivor
;;      flymake
;;      '((after-change-functions flymake-after-change-function)
;;        (after-save-hook flymake-after-save-hook)
;;        (kill-buffer-hook flymake-kill-buffer-hook))
;;      '(flymake-mode
;;        flymake-is-running
;;        flymake-timer
;;        flymake-last-change-time
;;        flymake-check-start-time
;;        flymake-check-was-interrupted
;;        flymake-err-info
;;        flymake-new-err-info
;;        flymake-output-residual
;;        flymake-mode-line
;;        flymake-mode-line-e-w
;;        flymake-mode-line-status
;;        flymake-temp-source-file-name
;;        flymake-master-file-name
;;        flymake-temp-master-file-name
;;        flymake-base-dir)
;;      flymake-mode))

;; The macro defines two functions for the user of this macro to
;; call, NAME-add-survivor and NAME-remove-survivor.  A typical use
;; is for a minor mode to use the first when turning on and the
;; second when turning off.  \(For flymake those functions will have
;; the names `flymake-add-survivor' and `flymake-remove-survivor'.)

;; NAME-add-survivor will add to local hooks according to the list
;; HOOK-FUN-LIST and arrange so that those additions to the local
;; hooks will be setup again after a major mode change.  Also make
;; sure that the local values of the variables in VAR-LIST survives
;; a major mode change.

;; NAME-remove-survivor does the opposite of this.

;; NAME should be a symbol.  HOOK-FUN-LIST should be a list where
;; each record has the format

;;   \(HOOK-NAME HOOK-FUNCTION)

;; where HOOK-NAME is the name of the hook to which HOOK-FUNCTION
;; should be added locally.

;; VAR-LIST should be a list of variable symbols.

;; If last argument TEST-SYM is a symbol then the functions setup by
;; `mumamo-mode' will call NAME-add-survivor and
;; NAME-remove-survivor.  This will be enough or the minor mode to
;; cooperate with `mumamo-mode'.  The functions will be called like
;; this:

;;   \(if TEST-SYM
;;        (NAME-add-survivor)
;;      (NAME-remove-survivor))

;; just before changing major mode."
;;   (let* ((sname (symbol-name name))
;;          (NAME-add-survivor    (intern (concat sname "-add-survivor")))
;;          (NAME-remove-survivor (intern (concat sname "-remove-survivor")))
;;          (NAME-acmmh-f         (intern (concat sname "-acmmh-f")))
;;          (NAME-cmmh-f          (intern (concat sname "-cmmh-f")))
;;          (test-sym-name        (when test-sym (symbol-name test-sym)))
;;          (NAME-auto-survivor   (when test-sym (intern (concat sname "-auto-survivor")))))

;;     `(progn

;;        (defun ,NAME-add-survivor ()
;;          "Add major mode change surviving.
;; This function should be called by the code that calls the macro
;; `make-change-major-survivor'."
;;          (dolist (rec ,hook-fun-list)
;;            (let ((hook (nth 0 rec))
;;                  (func (nth 1 rec)))
;;              (add-hook hook func nil t)))
;;          ;; Set up to survive major mode change
;;          (add-hook 'change-major-mode-hook ',NAME-cmmh-f nil t)
;;          ;;(lwarn t :warning "add survivor, cmmh=%S" change-major-mode-hook)
;;          )

;;        (defun ,NAME-remove-survivor ()
;;          "Remove major mode change surviving.
;; This function should be called by the code that calls the macro
;; `make-change-major-survivor'."
;;          (dolist (rec ,hook-fun-list)
;;            (let ((hook (nth 0 rec))
;;                  (func (nth 1 rec)))
;;              (remove-hook hook func t)))
;;          ;; Set up to survive major mode change
;;          (remove-hook 'change-major-mode-hook ',NAME-cmmh-f t)
;;          ;;(lwarn t :warning "rem survivor, cmmh=%S" change-major-mode-hook)
;;          )

;;        (when ,test-sym-name
;;          (defun ,NAME-auto-survivor ()
;;            (if ,test-sym-name
;;                (,NAME-add-survivor)
;;              (,NAME-remove-survivor)))
;;          (add-hook 'mumamo-auto-survivors-hook
;;                       ',NAME-auto-survivor))

;;        (defun ,NAME-acmmh-f ()
;;          "Restore after changing major mode.
;; This function is added locally to `after-change-major-mode-hook'."
;;          ;;(remove-hook 'after-change-major-mode-hook ',NAME-acmmh-f t)
;;          (,NAME-add-survivor)
;;          ;; Remove 'permanent-local t
;;          (dolist (sym ,var-list)
;;            (put sym 'permanent-local nil)))

;;        (defun ,NAME-cmmh-f ()
;;          "Set up to restore after changing major mode.
;; This function is added locally to `change-major-mode-hook'."
;;          (add-hook 'after-change-major-mode-hook ',NAME-acmmh-f nil t)
;;          ;;(lwarn t :warning "cmmh-f, acmmh=%S" after-change-major-mode-hook)
;;          (put 'after-change-major-mode-hook 'permanent-local t)
;;          ;; Note: I can see no way to later remove the
;;          ;; 'permanent-local property that is set here without
;;          ;; getting potential problems.
;;          ;;
;;          ;; Add 'permanent-local t
;;          (dolist (sym ,var-list)
;;            (put sym 'permanent-local t))))))

;; (eval-after-load 'flymake
;;   '(mumamo-make-change-major-survivor
;;    flymake
;;    '((after-change-functions flymake-after-change-function)
;;      (after-save-hook flymake-after-save-hook)
;;      (kill-buffer-hook flymake-kill-buffer-hook))
;;    '(flymake-mode
;;      flymake-is-running
;;      flymake-timer
;;      flymake-last-change-time
;;      flymake-check-start-time
;;      flymake-check-was-interrupted
;;      flymake-err-info
;;      flymake-new-err-info
;;      flymake-output-residual
;;      flymake-mode-line
;;      flymake-mode-line-e-w
;;      flymake-mode-line-status
;;      flymake-temp-source-file-name
;;      flymake-master-file-name
;;      flymake-temp-master-file-name
;;      flymake-base-dir
;;      )
;;    flymake-mode))

;; (eval-after-load 'imenu
;;   '(mumamo-make-change-major-survivor
;;     imenu
;;     ;; Fix-me: imenu is only useful for main major mode.  The menu
;;     ;; disappears in sub chunks because it is tighed to
;;     ;; local-map.  Don't know what to do about that.  I do not
;;     ;; understand the reason for binding it to local-map, but I
;;     ;; suspect the intent is to have different menu items for
;;     ;; different modes.  Could not that be achieved by deleting the
;;     ;; menu and creating it again when changing major mode? (That must
;;     ;; be implemented in imenu.el of course.)
;;     ;;
;;     ;; hook functions:
;;     '((imenu-update-menubar))
;;     ;; vars:
;;     '(imenu-generic-expression
;;       imenu-create-index-function
;;       imenu-prev-index-position-function
;;       imenu-extract-index-name-function
;;       imenu-name-lookup-function
;;       imenu-default-goto-function
;;       imenu--index-alist
;;       imenu--last-menubar-index-alist
;;       imenu-syntax-alist
;;       imenu-case-fold-search
;;       imenu-menubar-modified-tick)
;;     imenu-generic-expression))

;; (defun mumamo-handle-auto-survivors ()
;;   "Call auto survivors hook."
;;   (run-hooks 'mumamo-auto-survivors-hook))

(eval-after-load 'flymake
  (progn
    ;; hook functions:
    (put 'flymake-after-change-function 'permanent-local-hook t)
    (put 'flymake-after-save-hook 'permanent-local-hook t)
    (put 'flymake-kill-buffer-hook 'permanent-local-hook t)
    ;; hooks:
;;;     (put 'after-change-functions 'permanent-local 'permanent-local-hook)
;;;     (put 'after-save-hook 'permanent-local 'permanent-local-hook)
;;;     (put 'kill-buffer-hook 'permanent-local 'permanent-local-hook)
    ;; vars:
    (put 'flymake-mode 'permanent-local t)
    (put 'flymake-is-running 'permanent-local t)
    (put 'flymake-timer 'permanent-local t)
    (put 'flymake-last-change-time 'permanent-local t)
    (put 'flymake-check-start-time 'permanent-local t)
    (put 'flymake-check-was-interrupted 'permanent-local t)
    (put 'flymake-err-info 'permanent-local t)
    (put 'flymake-new-err-info 'permanent-local t)
    (put 'flymake-output-residual 'permanent-local t)
    (put 'flymake-mode-line 'permanent-local t)
    (put 'flymake-mode-line-e-w 'permanent-local t)
    (put 'flymake-mode-line-status 'permanent-local t)
    (put 'flymake-temp-source-file-name 'permanent-local t)
    (put 'flymake-master-file-name 'permanent-local t)
    (put 'flymake-temp-master-file-name 'permanent-local t)
    (put 'flymake-base-dir 'permanent-local t)))

(eval-after-load 'imenu
  (progn
    ;; Fix-me: imenu is only useful for main major mode.  The menu
    ;; disappears in sub chunks because it is tighed to
    ;; local-map.  Don't know what to do about that.  I do not
    ;; understand the reason for binding it to local-map, but I
    ;; suspect the intent is to have different menu items for
    ;; different modes.  Could not that be achieved by deleting the
    ;; menu and creating it again when changing major mode? (That must
    ;; be implemented in imenu.el of course.)
    ;;
    ;; hook functions:
;;;     (put 'imenu-update-menubar 'permanent-local-hook t)
    ;; hooks:
    (put 'menu-bar-update-hook 'permanent-local 'permanent-local-hook)
    ;; vars:
    (put 'imenu-generic-expression 'permanent-local t)
    (put 'imenu-create-index-function 'permanent-local t)
    (put 'imenu-prev-index-position-function 'permanent-local t)
    (put 'imenu-extract-index-name-function 'permanent-local t)
    (put 'imenu-name-lookup-function 'permanent-local t)
    (put 'imenu-default-goto-function 'permanent-local t)
    (put 'imenu--index-alist 'permanent-local t)
    (put 'imenu--last-menubar-index-alist 'permanent-local t)
    (put 'imenu-syntax-alist 'permanent-local t)
    (put 'imenu-case-fold-search 'permanent-local t)
    (put 'imenu-menubar-modified-tick 'permanent-local t)
    ))
;; (eval-after-load 'cc-vars
;;   (progn
;;     (put 'c-special-indent-hook 'permanent-local t)
;;     ))



;; Fix-me: Rails, many problematic things:

;;; Fix-me: No idea about these, where are they used?? Add them to
;;; mumamo-survive?:
;; predictive-main-dict
;; predictive-prog-mode-main-dict
;; predictive-use-auto-learn-cache
;; predictive-dict-autosave-on-kill-buffer
(eval-after-load 'inf-ruby
  (progn
    (put 'inferior-ruby-first-prompt-pattern 'permanent-local t)
    (put 'inferior-ruby-prompt-pattern 'permanent-local t)
    ))

;;; These are for the output buffer (no problems):
;; font-lock-keywords-only
;; font-lock-defaults -- always buffer local
;; scroll-margin
;; scroll-preserve-screen-position

(eval-after-load 'rails-script
  (progn
    (put 'rails-script:run-after-stop-hook 'permanent-local t)
    (put 'rails-script:show-buffer-hook 'permanent-local t)
    (put 'rails-script:output-mode-ret-value 'permanent-local t)
    ))

;;; No problems I believe (it is in output buffer):
;; compilation-error-regexp-alist-alist
;; compilation-error-regexp-alist

;;; Fix-me: This is in the minor mode, what to do? Looks like it
;;; should have 'permanent-local t - in this case.  I have added it to
;;; mumamo-survive for now.
;; tags-file-name

(eval-after-load 'rails
  (progn
    (put 'rails-primary-switch-func 'permanent-local t)
    (put 'rails-secondary-switch-func 'permanent-local t)
    ))



(defvar mumamo-survive
  '(
    tags-file-name
    nxhtml-minor-mode
    )
  "Local variables to survive the change of major mode.")

;; FIX-ME: Not sure this dividing is necessary.  My purpose was to
;; clean up a bit, but it is actually quite common for a variable to
;; not be buffer local at some moment and be set later!
(defvar mumamo-survive-a-bit-strange
  '(
    tags-file-name
    )
  "Some `mumamo-survive' variables have a bit strange presense.
Those are listed here.  They are maybe not always local and when
they are they may already have 'permanent-local t.")

(defvar mumamo-survive-done-by-me nil
  "Variables set by mumamo already.
Used to avoid unnecessary warnings if setting major mode fails.")

;; (mumamo-hook-p 'viper-pre-command-hooks)
;; (mumamo-hook-p 'viper-before-change-functions)
;; (mumamo-hook-p 'c-special-indent-hook)
(defun mumamo-hook-p (sym)
  "Try to detect if SYM is a hook variable.
Just check the name."
  (let ((name (symbol-name sym)))
    (or (string= "-hook" (substring name -5))
        (string= "-hooks" (substring name -6))
        (string= "-functions" (substring name -10)))))

(defvar mumamo-major-mode nil)
(make-variable-buffer-local 'mumamo-major-mode)
(put 'mumamo-major-mode 'permanent-local t)

;; FIX-ME: Clean up the different ways of surviving variables during
;; change of major mode.
(defun mumamo-set-major (major)
  "Set major mode to MAJOR for mumamo."
  (mumamo-msgfntfy "mumamo-set-major %s, %s" major (current-buffer))
  (let ((start-time (get-internal-run-time))
        end-time
        used-time
        ;; Tell `mumamo-change-major-function':
        (mumamo-set-major-running major)
        ;; Fix-me: Why check, font-lock-mode must be t here?!
        (old-font-lock-mode font-lock-mode)
        (font-lock-mode font-lock-mode)
        ;; Save cursor type:
        (old-cursor-type cursor-type)
        ;; Protect last-command:
        (old-last-command last-command)
        )
    (when (mumamo-derived-from-mode major 'nxml-mode)
      (set (make-local-variable 'nxml-syntax-highlight-flag) nil)
      ;; To avoid removing 'fontified flag:
      (put 'nxml-syntax-highlight-flag 'permanent-local t)
      (put 'nxml-ns-state 'permanent-local t)
      ;;(put 'rng-open-elements 'permanent-local t)
      ;;(put 'rng-pending-contents 'permanent-local t)
      ;;(put 'rng-collecting-text 'permanent-local t)
      ;;(put 'rng-current-schema-file-name 'permanent-local t)
      (put 'rng-error-count 'permanent-local t)
      ;;(put 'rng-validate-up-to-date-end 'permanent-local t)
      )

    ;; We are not changing mode from font-lock's point of view, so
    ;; do not tell font-lock:
    (remove-hook 'change-major-mode-hook 'font-lock-change-mode t)
    (remove-hook 'change-major-mode-hook 'global-font-lock-mode-cmhh)

    ;; We are not changing mode from hs-minor-mode's point of view:
    (remove-hook 'change-major-mode-hook 'turn-off-hideshow t)

    (dolist (sym (reverse mumamo-survive))
      (when (boundp sym)
        (unless (or (local-variable-if-set-p sym)
                    (memq sym mumamo-survive-a-bit-strange)
                    (memq sym mumamo-survive-done-by-me)
                    ;; Is it a hook?
                    (mumamo-hook-p sym))
          (delq sym mumamo-survive)
          (lwarn 'mumamo-survive :warning "Not a local variable: %s" sym))
        (when (and (get sym 'permanent-local)
                   (not (memq sym mumamo-survive-a-bit-strange))
                   (not (memq sym mumamo-survive-done-by-me))
                   (not (mumamo-hook-p sym)))
          (delq sym mumamo-survive)
          (lwarn 'mumamo-survive :warning "Already 'permanent-local t: %s" sym))))
    (dolist (sym mumamo-survive)
      (add-to-list 'mumamo-survive-done-by-me sym)
      (put sym 'permanent-local t))

    ;; Fix-me: Check hooks
    ;;
    ;; For all hooks that probably can have buffer local values, go
    ;; through the buffer local values and look for a permanent-local
    ;; property on each function.  Remove those functions that does not
    ;; have it.  Then make the buffer local value of the hook survive
    ;; by putting a permanent-local property on it.
    ;;
    ;; I have made a request that this way of handling it should be
    ;; implemented in Emacs.  RMS has agreed to this now, but for the
    ;; moment I have to handle this here.
    ;;
    ;; (progn (message "start %s %s" (current-time-string) (current-time))
    ;;
    (unless (> emacs-major-version 22)
      (dolist (hk mumamo-survive-hooks)
        (put hk 'permanent-local t)
        (when (local-variable-p hk)
          (let ((hkv (copy-sequence (symbol-value hk))))
            (dolist (v hkv)
              (unless (or (eq v t)
                          (get v 'permanent-local-hook))
                ;; (lwarn t :warning "remove %s %s" hk v)
                (remove-hook hk v t)
                )))))
      )
      ;; (message "end %s %s" (current-time-string) (current-time)) (display-buffer "*Messages*"))

    ;; Fix-me: Just testing with flymake and imenu here
    ;;(mumamo-handle-auto-survivors)

    (run-hooks 'mumamo-change-major-mode-hook)

    (setq mumamo-major-mode major)


    (funcall major) ;; <-----------------------------------------------
    (setq mode-name (concat (format-mode-line mode-name) "/m")) ;; mode-name is not always a string

    (mumamo-ppss-flush-cache (point))

    (dolist (hk mumamo-survive-hooks) (put hk 'permanent-local nil))

;;     (when (and (featurep 'flymake)
;;                flymake-mode)
;;       (add-hook 'after-change-functions 'flymake-after-change-function nil t)
;;       (add-hook 'after-save-hook 'flymake-after-save-hook nil t)
;;       (add-hook 'kill-buffer-hook 'flymake-kill-buffer-hook nil t))

    (dolist (sym mumamo-survive)
      (when (boundp sym)
        (put sym 'permanent-local nil)))
    (when (and (featurep 'mlinks)
               mlinks-mode)
      (add-hook 'after-change-functions 'mlinks-after-change t t))
    ;; This seems to be set in a special way somewhere so just reset it here:
    (add-hook 'c-special-indent-hook 'mumamo-c-special-indent t t)

    (add-to-list 'mumamo-indent-line-alist (list major-mode indent-line-function))

    ;; Some major modes deactivates the mark, we do not want that:
    (setq deactivate-mark nil)

    (setq cursor-type old-cursor-type)
    (unless (eq last-command old-last-command)
      (lwarn 'mumamo-set-major :error "last-command 3=%s, old-last-command" last-command old-last-command)
      (setq last-command old-last-command))
    (run-hooks 'mumamo-after-change-major-mode-hook)

    (when (boundp 'nxml-syntax-highlight-flag)
      (put 'nxml-syntax-highlight-flag 'permanent-local nil)
      (put 'nxml-ns-state 'permanent-local nil)
      ;;(put 'rng-open-elements 'permanent-local t)
      ;;(put 'rng-pending-contents 'permanent-local t)
      ;;(put 'rng-collecting-text 'permanent-local t)
      ;;(put 'rng-current-schema-file-name 'permanent-local t)
      ;;
      ;; FIX-ME: I Do not understand this, but I get rng-error-count
      ;; reset to nil if it is not always permanent-local t.
      ;;
      ;;(put 'rng-error-count 'permanent-local nil)
      ;;
      ;;(put 'rng-validate-up-to-date-end 'permanent-local nil)
      )

    (when global-font-lock-mode
      (add-hook 'change-major-mode-hook 'global-font-lock-mode-cmhh))
    (when old-font-lock-mode
      (add-hook 'change-major-mode-hook 'font-lock-change-mode nil t))

    ;;(setq mumamo-mode t)
    (mumamo-set-fontification-functions)

    ;; If user has used M-x flyspell-mode then we need to correct it:
    ;; Fix-me: This is inflexible. Need flyspell to cooperate.
    (when (featurep 'flyspell)
      (setq flyspell-generic-check-word-predicate 'mumamo-flyspell-verify))

    (if mumamo-done-first-set-major
        (setq mumamo-just-changed-major t)
      (mumamo-msgfntfy "mumamo-set-major: ----- removing 'fontified")
      ;; Set up to fontify buffer
      (mumamo-save-buffer-state nil
        (remove-text-properties (point-min) (point-max) '(fontified)))
      (setq mumamo-done-first-set-major t))

    ;; Timing, on a 3ghz cpu:
    ;;
    ;;   used-time=(0 0 0), major-mode=css-mode
    ;;   used-time=(0 0 0), major-mode=ecmascript-mode
    ;;   used-time=(0 0 0), major-mode=html-mode
    ;;   used-time=(0 0 203000), major-mode=nxhtml-mode
    ;;
    ;; After some changes 2007-04-25:
    ;;
    ;;   used-time=(0 0 15000), major-mode=nxhtml-mode
    ;;
    ;; which is 15 ms.  That seems acceptable though I am not sure
    ;; everything is correct when switching to nxhtml-mode yet.  I
    ;; will have to wait for bug reports ;-)
    ;;
    ;; The delay is clearly noticeable and disturbing IMO unless you
    ;; change major mode in an idle timer.
    ;;
    (setq end-time (get-internal-run-time))
    (setq used-time (time-subtract end-time start-time))
    ;;(message ";;   used-time=%s, major-mode=%s" used-time major-mode)
    ;;(lwarn 'switch-time :warning "used-time=%s, major-mode=%s" used-time major-mode)

    ))

(defun mumamo-setup-local-fontification-vars ()
  "Set up buffer local variables for mumamo style fontification."
  (make-local-variable 'font-lock-fontify-region-function)
  (setq font-lock-fontify-region-function 'mumamo-fontify-region)

  ;; FIX-ME: This should really be defined similar to fontify, and
  ;; this should be used when finishing mumamo-mode.
  (make-local-variable 'font-lock-unfontify-region-function)
  (setq font-lock-unfontify-region-function 'mumamo-unfontify-region)

  ;; Like font-lock-turn-on-thing-lock:
  (make-local-variable 'font-lock-fontify-buffer-function)
  (setq font-lock-fontify-buffer-function 'mumamo-fontify-buffer)
  (setq font-lock-fontify-buffer-function 'jit-lock-refontify)

  (make-local-variable 'font-lock-unfontify-buffer-function)
  (setq font-lock-unfontify-buffer-function 'mumamo-unfontify-buffer)


  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'mumamo-indent-line-function)

  (set (make-local-variable 'fill-paragraph-function) 'mumamo-fill-paragraph-function)

  (make-local-variable 'indent-region-function)
  (setq indent-region-function 'mumamo-indent-region-function)

  (set (make-local-variable 'syntax-begin-function) 'mumamo-beginning-of-syntax)

  ;; FIX-ME: Not sure about this one, but it looks like it must be
  ;; set:
  (make-local-variable 'jit-lock-contextually)
  (setq jit-lock-contextually t)
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Turning on/off multi major modes

(defun mumamo-set-fontification-functions ()
  "Let mumamo take over fontification.
This is run after changing major mode so that jit-lock will get
the major mode specific values.  \(There are currently no such
values.)"
  ;; Give the jit machinery a starting point:
  (mumamo-jit-lock-register 'font-lock-fontify-region t)
  ;; Set the functions that font-lock should use:
  (mumamo-setup-local-fontification-vars)
  ;; Need some hook modifications to keep things together too:
  (add-hook 'change-major-mode-hook 'mumamo-change-major-function nil t)
  (add-hook 'post-command-hook 'mumamo-post-command nil t)
  (remove-hook 'change-major-mode-hook 'nxml-change-mode t)
  (remove-hook 'change-major-mode-hook 'nxhtml-change-mode t)
  )

(defun mumamo-initialize-state ()
  "Initialize some mumamo state variables."
  (setq mumamo-do-fontify-last-pos nil)
  (setq mumamo-do-fontify-last-major nil)
  (setq mumamo-done-first-set-major nil)
  (setq mumamo-just-changed-major nil))

(defun mumamo-turn-on-actions ()
  "Do what is necessary to turn on mumamo.
Turn on minor mode `font-lock-mode'.
Set up for mumamo style fontification.
Create a mumamo chunk at point.
Run `mumamo-turn-on-hook'."
  ;;(setq mumamo-main-major-mode major-mode)
  (unless font-lock-mode (font-lock-mode 1))
  (condition-case err
      (progn
        (mumamo-msgfntfy "mumamo-turn-on-actions")
        (unless mumamo-current-chunk-family
          ;;(mumamo-select-chunk-family)
          (error "Internal error: Chunk family is not set")
          )
        (if (not mumamo-current-chunk-family)
            (progn
              (lwarn '(mumamo) :warning
                     "Could not turn on mumamo because chunk family was not set\n\tin buffer %s."
                     (current-buffer))
              (with-current-buffer "*Warnings*"
                (insert "\tFor more information see `")
                (mumamo-insert-describe-button 'define-mumamo-multi-major-mode 'describe-function)
                (insert "'.\n")))
          (mumamo-initialize-state)
          (mumamo-set-fontification-functions)
          (mumamo-save-buffer-state nil
              (remove-list-of-text-properties (point-min) (point-max)
                                              (list 'fontified)))
          (setq mumamo-major-mode (nth 1 mumamo-current-chunk-family))
          ;; Load major mode:
          (let ((main-major-mode (mumamo-main-major-mode)))
            (with-temp-buffer
              (funcall main-major-mode)))
          ;; For validation header etc:
          (require 'rngalt nil t)
          (when (featurep 'rngalt)
            (setq rngalt-major-mode (mumamo-main-major-mode))
            (rngalt-update-validation-header-overlay))
          (mumamo-get-chunk-at (point))
          ;;(mumamo-set-major-post-command)
          ;;(add-hook 'change-major-mode-hook 'mumamo-change-major-function nil t)
          (when (boundp 'flyspell-generic-check-word-predicate)
            (setq flyspell-generic-check-word-predicate 'mumamo-flyspell-verify))
          (run-hooks 'mumamo-turn-on-hook)))
    (quit
     )
    (error
     (mumamo-display-error 'mumamo-turn-on-actions "%s"
                           (error-message-string err)))))

(defun mumamo-on-font-lock-off ()
  "The reverse of `mumamo-turn-on-actions'."
  (let ((mumamo-main-major-mode (mumamo-main-major-mode)))
    (mumamo-turn-off-actions)
    ;; Turning off `font-lock-mode' also turns off `mumamo-mode'.  It is
    ;; quite tricky to not turn on `font-lock-mode' again in case we got
    ;; here because it was turned off.  We must first remove the cmhh
    ;; function and then also run the internal font lock turn off.
    (let* ((flm  font-lock-mode)
           (flgm global-font-lock-mode)
           (remove-cmhh (and (not flm) flgm)))
      ;; If remove-cmhh is non-nil then we got here because
      ;; `font-lock-mode' was beeing turned off in the buffer, but
      ;; `global-font-lock-mode' is still on.
      (when remove-cmhh
        (remove-hook 'change-major-mode-hook 'global-font-lock-mode-cmhh))

      (if mumamo-main-major-mode
          (funcall mumamo-main-major-mode)
        (fundamental-mode))

      (unless flm
        (setq font-lock-mode nil)
        (font-lock-mode-internal nil))
      (when remove-cmhh
        (add-hook 'change-major-mode-hook 'global-font-lock-mode-cmhh)))))

(defun mumamo-unfontify-chunks ()
  "Unfontify all mumamo chunks in current buffer."
  (save-restriction
    (widen)
    (let ((ovls (overlays-in (point-min) (point-max))))
      (dolist (o ovls)
        (when (mumamo-chunk-major-mode o)
          (mumamo-unfontify-chunk o))))))

(defun mumamo-turn-off-actions ()
  "The reverse of `mumamo-turn-on-actions'."
  (mumamo-msgfntfy "mumamo-turn-off-actions")
  (when (mumamo-derived-from-mode (nth 1 mumamo-current-chunk-family) 'nxml-mode)
    (when (fboundp 'nxml-change-mode)
      (nxml-change-mode)))
  (when (and (boundp 'rng-validate-mode)
             rng-validate-mode)
    (rng-validate-mode 0))
  (when (boundp 'rngalt-major-mode)
    (setq rngalt-major-mode nil))
  (remove-hook 'change-major-mode-hook 'mumamo-change-major-function t)
  (mumamo-unfontify-chunks)
  (remove-hook 'after-change-functions 'mumamo-jit-lock-after-change t)
  (remove-hook 'post-command-hook 'mumamo-post-command t)
  (remove-hook 'c-special-indent-hook 'mumamo-c-special-indent t)
  (mumamo-remove-all-chunk-overlays)
  (setq mumamo-current-chunk-family nil)
  (set mumamo-multi-major-mode nil) ;; for minor-mode-map-alist
  (setq mumamo-multi-major-mode nil))

(defvar mumamo-turn-on-hook nil
  "Normal hook run after turning on `mumamo-mode'.")
(put 'mumamo-turn-on-hook 'permanent-local t)

(defvar mumamo-change-major-mode-hook nil
  "Normal hook run before internal change of major mode.")
(put 'mumamo-change-major-mode-hook 'permanent-local t)

(defvar mumamo-after-change-major-mode-hook nil
  "Normal hook run after internal change of major mode.")
(put 'mumamo-after-change-major-mode-hook 'permanent-local t)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Defining multi major modes

(defvar mumamo-defined-turn-on-functions nil
  "List of functions defined for turning on mumamo.
Those functions should be called instead of calling a major mode
function when you want to use multiple major modes in a buffer.
They may be added to for example `auto-mode-alist' to
automatically have the major mode support turned on when opening
a file.

Each of these functions defines how to mix certain major modes in
a buffer.

All functions defined by `define-mumamo-multi-major-mode' are added to
this list. See this function for a general description of how the
functions work.

If you want to quickly define a new mix of major mode you can use
`mumamo-easy-make-chunk-fun'.")

(defun mumamo-describe-chunks (chunks)
  (let* ((desc
          (concat "Main major mode: `" (symbol-name (nth 1 chunks)) "'\n"
                  "\nFunctions for dividing into submodes:\n")))
    (dolist (divider (nth 2 chunks))
      (setq desc
            (concat
             desc
             "\n`" (symbol-name divider)
             "'\n   "
             (let ((doc (documentation divider t)))
               (if (not doc)
                   "(Not documented)"
                 (substring doc 0 (string-match "\n" doc)))))))
    desc))

(defun mumamo-select-multiple-major-mode-division (mult-mode)
  (interactive (list
                (completing-read "Multiple major mode division setup: "
                                 mumamo-defined-turn-on-functions)))
  (message "mult-mode=%s" mult-mode))

(defun mumamo-add-multi-keymap (toggle keymap)
  "Add TOGGLE and KEYMAP to `minor-mode-map-alist'.
This is used to add a keymap to multi major modes since the local
keymap is occupied by the major modes.

It is also used to add the `mumamo-map' keymap to every buffer
with a multi major mode."
  ;; Copied from add-minor-mode
  ;; Add the map to the minor-mode-map-alist.
  (when keymap
    (let ((existing (assq toggle minor-mode-map-alist))
          (after t))
      (if existing
	  (setcdr existing keymap)
	(let ((tail minor-mode-map-alist) found)
	  (while (and tail (not found))
	    (if (eq after (caar tail))
		(setq found tail)
	      (setq tail (cdr tail))))
	  (if found
	      (let ((rest (cdr found)))
		(setcdr found nil)
		(nconc found (list (cons toggle keymap)) rest))
	    (setq minor-mode-map-alist (cons (cons toggle keymap)
					     minor-mode-map-alist))))))))

(defvar mumamo-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(control meta prior)] 'mumamo-backward-chunk)
    (define-key map [(control meta next)]  'mumamo-forward-chunk)
    ;; Use mumamo-indent-line-function:
    (define-key map [tab] 'indent-for-tab-command)
    map))

(mumamo-add-multi-keymap 'mumamo-multi-major-mode mumamo-map)

(defmacro define-mumamo-multi-major-mode (fun-sym spec-doc chunks)
  "Define a function that turns on support for multiple major modes.
Define a function FUN-SYM that set up to divide the current
buffer into chunks with different major modes using the
definitions in CHUNKS.

The documentation string for FUN-SYM should contain the special
documentation in the string SPEC-DOC, general documentation for
functions of this type and information about CHUNKS.

The function FUN-SYM can be used to setup a buffer instead of a
major mode function:

- The function FUN-SYM can be called instead of calling a major
  mode function when you want to use multiple major modes in a
  buffer.

- The defined function can be used instead of a major mode
  function in for example `auto-mode-alist'.

- As the very last thing FUN-SYM will run the hook FUN-SYM-hook,
  just as major modes do.

- There is also a general hook, `mumamo-turn-on-hook', which is
  run when turning on mumamo with any of these functions.  This
  is run right before the hook specific to any of the functions
  above that turns on the multiple major mode support.

- The multi major mode FUN-SYM has a keymap named FUN-SYM-map.
  This overrides the major modes' keymaps since it is handled as
  a minor mode keymap.

- There is also a special mumamo keymap, `mumamo-map' that is
  active in every buffer with a multi major mode.  This is also
  handled as a minor mode keymap and therefor overrides the major
  modes' keymaps.

- However when this support for multiple major mode is on the
  buffer is divided into chunks, each with its own major mode.

- The chunks are fontified according the major mode assigned to
  them for that.

- Indenting is also done according to the major mode assigned to
  them for that.

- The actual major mode used in the buffer is changed to the one
  in the chunk when moving point between these chunks.

- When major mode is changed the hooks for the new major mode,
  `after-change-major-mode-hook' and `change-major-mode-hook' are
  run.


** A little bit more technical description:

The dividing of a buffer into chunks is done during fontification
by `mumamo-get-chunk-at'.

The name of the function is saved in in the buffer local variable
`mumamo-multi-major-mode' when the function is called.

All functions defined by this macro is added to the list
`mumamo-defined-turn-on-functions'.

Basically Mumamo handles only major modes that uses jit-lock.
However as a special effort also `nxml-mode' and derivatives
thereof are handled.  Since it seems impossible to me to restrict
those major modes fontification to only a chunk without changing
`nxml-mode' the fontification is instead done by
`html-mode'/`sgml-mode' for chunks using `nxml-mode' and its
derivates.

CHUNKS is a list where each entry have the format

  \(CHUNK-DEF-NAME MAIN-MAJOR-MODE SUBMODE-CHUNK-FUNCTIONS)

CHUNK-DEF-NAME is the key name by which the entry is recognized.
MAIN-MAJOR-MODE is the major mode used when there is no chunks.
SUBMODE-CHUNK-FUNCTIONS is a list of the functions that does the
chunk division of the buffer.  They are tried in the order they
appear here during the chunk division process.

If you want to write new functions for chunk divisions then
please see `mumamo-find-possible-chunk'.  You can perhaps also
use `mumamo-easy-make-chunk-fun' or `mumamo-chunk-attr=' which
are more easy-to-use alternatives.

When you write those new functions you may want to use some of
the functions for testing chunks:

 `mumamo-test-create-chunk-at'  `mumamo-test-create-chunks-at-all'
 `mumamo-test-easy-make'        `mumamo-test-fontify-region'

These are in the file mumamo-test.el.
"
  ;;(let ((c (if (symbolp chunks) (symbol-value chunks) chunks))) (message "c=%S" c))
  (let* ((mumamo-describe-chunks (make-symbol "mumamo-describe-chunks"))
         (turn-on-fun (if (symbolp fun-sym)
                          (symbol-value
                           (intern
                            (symbol-name (quote fun-sym))))
                        (error "Parameter FUN-SYM must be a symbol")))
         (turn-on-hook (intern (concat (symbol-name turn-on-fun) "-hook")))
         (turn-on-map  (intern (concat (symbol-name turn-on-fun) "-map")))
         (turn-on-hook-doc (concat "Hook run at the very end of `" (symbol-name turn-on-fun) "'."))
         (chunks2 (if (symbolp chunks)
                      (symbol-value chunks)
                    chunks))
         (docstring
          (concat
           spec-doc
           "

The main use for this function is in `auto-mode-alist' to have
Emacs do this setup whenever you open a file named in a certain
way.  \(You can of course call this function directly yourself
too.)

It sets up for multiple mode in the following way:

"
           (funcall 'mumamo-describe-chunks chunks2)
           "

At the very end it runs first the hook `mumamo-turn-on-hook' and
then `" (symbol-name turn-on-hook) "'.

For more information see `define-mumamo-multi-major-mode'."
           )))
    ;;(message "chunks2=%s" chunks2)
    ;;(lwarn t :warning "turn-on-hook name=%s" (symbol-name turn-on-hook))
    (add-to-list 'mumamo-defined-turn-on-functions (cons (car chunks2) turn-on-fun))
    `(progn
       (defvar ,turn-on-hook nil ,turn-on-hook-doc)
       (defvar ,turn-on-map (make-sparse-keymap)) ;; fix-me: doc
       (defvar ,turn-on-fun nil)
       (make-variable-buffer-local ',turn-on-fun)
       (put ',turn-on-fun 'permanent-local t)
       (defun ,turn-on-fun nil ,docstring
         (interactive)
         (kill-all-local-variables)
         (run-hooks 'change-major-mode-hook)
         (setq mumamo-multi-major-mode ',turn-on-fun)
         (setq ,turn-on-fun t)
         (mumamo-add-multi-keymap ',turn-on-fun ,turn-on-map)
         (mumamo-define-chunks ',chunks2)
         (mumamo-turn-on-actions)
         (run-hooks ',turn-on-hook)
         )
       )
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Indenting, filling, moving etc

;; FIX-ME: Indentation in perl here doc indents the ending mark which
;; corrupts the perl here doc.

(defun mumamo-indent-line-function ()
  "Function to indent the current line.
This is the buffer local value of `indent-line-function' when
mumamo is used."
  (let ((here (point-marker))
        (before-text (<= (current-column) (current-indentation))))
    (mumamo-indent-line-function-1 nil nil)
    ;; If the marker was in the indentation part strange things happen
    ;; if we try to go back to the marker, at least in php-mode parts.
    (if before-text
        (back-to-indentation)
      (goto-char here))))

(defun mumamo-indent-line-major-modes ()
  "Return major modes for indenting current line.
A cons with major mode at beginning and dito at the end of line
is returned."
  (let* ((ovl1 (mumamo-get-chunk-at (line-beginning-position)))
         (major1 (mumamo-chunk-major-mode ovl1))
         (ovl2 (if (< (line-end-position) (overlay-end ovl1))
                   ovl1
                 (mumamo-get-chunk-at (line-end-position))))
         (major2 (mumamo-chunk-major-mode ovl2)))
    (cons major1 major2)))

;; FIX-ME: This is a terrible way to get the indentation the major
;; mode will do, but I can not find any better ...  -- The trouble is
;; that Emacs specifies that indent-line-function should indent.  It
;; would have been better here if it just returned the indentation to
;; use.
;; (defun mumamo-get-major-mode-indent-column ()
;;   "Return the indentation `mumamo-call-indent-line' should give.
;; Narrow to the current chunk, call `mumamo-call-indent-line',
;; record the indentation that results and then revert the changes
;; made.  Return the indentation that was recorded.
;;
;; The value returned from this function is used by
;; `mumamo-indent-line-function-1' when it tries to indent the
;; current line."
;;   (let ((got-indent 0)
;;         (this-line (line-beginning-position)))
;;     (save-restriction
;;       (let ((chunk (mumamo-get-chunk-at (point))))
;;         (narrow-to-region (overlay-start chunk)
;;                           (overlay-end chunk))
;;         (mumamo-save-buffer-state-with-undo nil
;;           (mumamo-call-indent-line)
;;           (setq got-indent (current-indentation))
;; ;;           (undo-start)
;; ;;           (while (and (not (eq t pending-undo-list))
;; ;;                       pending-undo-list)
;; ;;             (undo-more 1))
;;           )))
;;     (goto-char this-line)
;;     got-indent))

;; Fix-me: need to back up past comments in for example <style> /* comment */
;; fix-me: clean up
(defun mumamo-indent-line-function-1 (prev-line-majors last-main-major-indent)
  "Indent current line.
When doing that care must be taken if this line's major modes at
the start and end are different from previous line major modes.
The latter may be known through the parameter PREV-LINE-MAJORS.

Also the indentation of the last previous main major line may be
necessary to know.  This may be known through the parameter
LAST-MAIN-MAJOR-INDENT.

If the two parameters above are nil then this function will
search backwards in the buffer to try to determine their values.

The following rules are used when indenting:

- If the major modes are the same in this and the previous line
  then indentation is done using that mode.

- Otherwise if going into a submode indentation is increased by
  `mumamo-submode-indent-offset'.

- When going out of a submode indentation is reset to
  LAST-MAIN-MAJOR-INDENT.

- When going from one submode to another the new submode's
  indentation will be relative LAST-MAIN-MAJOR-INDENT."
  (let ((this-line-majors (mumamo-indent-line-major-modes))
        major-indent-line-function
        (main-major (mumamo-main-major-mode))
        (old-indent (current-indentation))
        (entering-submode nil)
        (leaving-submode nil)
        want-indent ;; The indentation we desire
        got-indent
        this-pending-undo-list)
    (mumamo-msgfntfy "mumamo-indent-line-function-1 %s %s, this-line-major=%s" prev-line-majors last-main-major-indent this-line-majors)
    (unless prev-line-majors
      (save-excursion
        (goto-char (line-beginning-position 0))
        (setq prev-line-majors (mumamo-indent-line-major-modes))))
    (setq entering-submode
          (or
           ;; Going from main to sub
           (and (eq (car prev-line-majors) main-major)
                   (not (eq (car this-line-majors) main-major)))
           ;; Going from sub to sub
           (and (not (eq (car prev-line-majors) main-major))
                (not (eq (car this-line-majors) main-major))
                (not (eq (car prev-line-majors)
                         (car this-line-majors))))))
    ;;(lwarn t :warning "line: %s, prev=%s, this=%s, \t%s" (line-number-at-pos) prev-line-majors this-line-majors (current-line-string))
    (setq leaving-submode
          (and (not (eq (cdr prev-line-majors) main-major))
               (eq (cdr this-line-majors) main-major)))
    (assert (not (and leaving-submode entering-submode)) t)
    (when (or leaving-submode entering-submode)
      (unless last-main-major-indent
        (save-excursion
          (while (not last-main-major-indent)
            (if (bobp)
                (setq last-main-major-indent 0)
              (goto-char (line-beginning-position 0))
              (when (eq main-major (car (mumamo-indent-line-major-modes)))
                (skip-chars-forward " \t")
                (if (eolp)
                    (setq last-main-major-indent 0)
                  (setq last-main-major-indent (current-column)))))))))
    (if leaving-submode
        (setq want-indent last-main-major-indent)
      (if entering-submode
          (setq want-indent (+ last-main-major-indent
                               (if (= 0 last-main-major-indent)
                                   mumamo-submode-indent-offset-0
                                 mumamo-submode-indent-offset)))
        (unless (eq (car this-line-majors) major-mode)
          ;; We have to change major mode, because we know nothing
          ;; about the requirements of the indent-line-function:
          (mumamo-set-major (car this-line-majors)))
        (if (eq (car this-line-majors) main-major)
            (progn
              (setq last-main-major-indent nil) ;; We can't use any old value more
              (mumamo-call-indent-line))
          ;; Get the indentation the major mode alone would use:
          ;;(setq got-indent (mumamo-get-major-mode-indent-column))
          ;; Since this line has another major mode than the
          ;; previous line we instead want to indent relative to
          ;; that line in a way decided in mumamo:
          (let ((chunk (mumamo-get-chunk-at (point)))
                (font-lock-dont-widen t))
            (save-restriction
              (narrow-to-region (overlay-start chunk)
                                (overlay-end chunk))
              (mumamo-call-indent-line)
              ;; Unfortunately the indentation can sometimes get 0
              ;; here even though it is clear it should not be 0. This
              ;; happens when there are only comments or empty lines
              ;; above.
;;; Is this needed any more?
;;;               (when (and nil (= 0 (current-indentation)))
;;;                 ;; Fix-me: Here we would like to undo the indentation
;;;                 ;; done, but how?
;;;                 (save-excursion
;;;                   (setq want-indent 0)
;;;                   (while (and (= 0 want-indent)
;;;                               (/= (point) (point-min)))
;;;                     (beginning-of-line 0)
;;;                     (setq want-indent (current-indentation)))
;;;                   ;; Now if want-indent is still 0 we need to look further above
;;;                   (when (= 0 want-indent)
;;;                     (widen)
;;;                     (while (and (= 0 want-indent)
;;;                                 (/= (point) (point-min)))
;;;                       (beginning-of-line 0)
;;;                       (setq want-indent (current-indentation)))
;;;                     ;; If we got to the main major mode we need to add
;;;                     ;; the special submode offset:
;;;                     (let* ((ovl (mumamo-get-chunk-at (point)))
;;;                            (major (mumamo-chunk-major-mode ovl)))
;;;                       (when (eq major main-major)
;;;                         (setq want-indent (+ want-indent
;;;                                              (if (= 0 want-indent)
;;;                                                  mumamo-submode-indent-offset-0
;;;                                                mumamo-submode-indent-offset))))))))
              )))))
    (when want-indent
      (indent-line-to want-indent))
    (list this-line-majors last-main-major-indent)))


;; Fix-me: I have some bug example which makes clear why this is
;; needed.  Where? It should be mentioned here!  I believe they were
;; seen in php-mode, but I still think it is better to have function
;; in mumamo since it is there the problem shows up.  It might be
;; needed for more file types than php.
;;
;; Fix-me: This is not called any more, why?
(defun mumamo-c-special-indent ()
  "Special indentations for modes derived from c.
Fix indentation if it has become 0.

This function corrects a problem that sometimes occur when using
a mode derived from C with mumamo.  For some reason the
indentation sometimes becomes 0 - even though it seems clear from
a visual inspection that it should not be 0.

This function is added to `c-special-indent-hook' by
`mumamo-set-major'."
  (when (= 0 (current-indentation))
    (let ((old-pos (point-marker))
          last-main-major-indent
          (main-major (mumamo-main-major-mode))
          want-indent)
      (while (not last-main-major-indent)
        (if (bobp)
            (setq last-main-major-indent 0)
          (goto-char (line-beginning-position 0))
          (when (eq main-major (car (mumamo-indent-line-major-modes)))
            (skip-chars-forward " \t")
            (if (eolp)
                (setq last-main-major-indent 0)
              (setq last-main-major-indent (current-column))))))
      (setq want-indent
            (if (= 0 last-main-major-indent)
                mumamo-submode-indent-offset-0
              (+ last-main-major-indent mumamo-submode-indent-offset )))
      (goto-char old-pos)
      (indent-line-to want-indent)
      (goto-char old-pos))))

(defvar mumamo-indent-line-alist nil
  "Alist of `major-mode' vs `indent-line-function'.")

(defun mumamo-call-indent-line ()
  "Call the relevant `indent-line-function'."
  (let ((indent-func (cadr (assq major-mode mumamo-indent-line-alist))))
    ;; We must get the correct syntax table etc
    (mumamo-with-major-mode-indentation major-mode (line-beginning-position)
      (funcall indent-func))))

(defun mumamo-indent-region-function (start end)
  "Indent the region between START and END."
  (save-excursion
    (setq end (copy-marker end))
    (goto-char start)
    (let ((old-point -1)
          prev-line-major
          last-main-major-indent)
      (while (and (< (point) end)
                  (/= old-point (point)))
        (or (and (bolp) (eolp))
            (let ((ret (mumamo-indent-line-function-1
                        prev-line-major
                        last-main-major-indent)))
              (setq prev-line-major        (nth 0 ret))
              (setq last-main-major-indent (nth 1 ret))))
        (setq old-point (point))
        (forward-line 1)))
    (message "Ready indenting region")))

(defun mumamo-fill-paragraph-function(&optional justify region)
  "Function to fill the current paragraph.
This is the buffer local value of `fill-paragraph-function' when
mumamo is used."
  (let* ((ovl (mumamo-get-chunk-at (point)))
         (major (mumamo-chunk-major-mode ovl))
         (main-major (mumamo-main-major-mode)))
    (mumamo-with-major-mode-fontification major (point)
      (let ((fill-paragraph-function mumamo-original-fill-paragraph-function))
        (fill-paragraph justify region)))))

(defun mumamo-forward-chunk ()
  "Move forward to next chunk."
  (interactive)
  (let* ((chunk (mumamo-get-chunk-at (point)))
         (end-pos (overlay-end chunk)))
    (goto-char (min end-pos
                    (point-max)))))

(defun mumamo-backward-chunk ()
  "Move backward to previous chunk."
  (interactive)
  (let* ((chunk (mumamo-get-chunk-at (point)))
         (start-pos (overlay-start chunk)))
    (goto-char (max (1- start-pos)
                    (point-min)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Spell checking

(defun mumamo-flyspell-verify ()
  "Function used for `flyspell-generic-check-word-predicate' when using mumamo."
  (let* ((chunk (when mumamo-multi-major-mode (mumamo-get-existing-chunk-at (point))))
         (chunk-major (when chunk (mumamo-chunk-major-mode chunk)))
         (mode-predicate (when chunk-major
                           (let ((predicate(get chunk-major 'flyspell-mode-predicate)))
                             (if predicate
                                 predicate
                               (if (mumamo-derived-from-mode chunk-major 'text-mode)
                                   nil
                                 'flyspell-generic-progmode-verify)))))
         )
    ;;(lwarn 'mumamo-flyspell-verify :warning "maor=%s, mode-predicate=%s" chunk-major mode-predicate)
    (if mode-predicate
        ;; Fix-me: (run-hooks 'flyspell-prog-mode-hook)
        (funcall mode-predicate)
      t)))

(eval-after-load 'flyspell
  (progn
    (put 'flyspell-mode 'permanent-local t)

    (put 'flyspell-generic-check-word-predicate 'permanent-local t)

    (put 'flyspell-casechars-cache 'permanent-local t)
    (put 'flyspell-ispell-casechars-cache 'permanent-local t)

    (put 'flyspell-not-casechars-cache 'permanent-local t)
    (put 'flyspell-ispell-not-casechars-cache 'permanent-local t)

    (put 'flyspell-auto-correct-pos 'permanent-local t)
    (put 'flyspell-auto-correct-region 'permanent-local t)
    (put 'flyspell-auto-correct-ring 'permanent-local t)
    (put 'flyspell-auto-correct-word 'permanent-local t)

    (put 'flyspell-consider-dash-as-word-delimiter-flag 'permanent-local t)

    (put 'flyspell-dash-dictionary 'permanent-local t)

    (put 'flyspell-dash-local-dictionary 'permanent-local t)

    (put 'flyspell-word-cache-start 'permanent-local t)
    (put 'flyspell-word-cache-end 'permanent-local t)
    (put 'flyspell-word-cache-word 'permanent-local t)
    (put 'flyspell-word-cache-result 'permanent-local t)

    (put 'flyspell-word-cache-start 'permanent-local t)


    (put 'flyspell-kill-ispell-hook 'permanent-local-hook t)
    (put 'flyspell-post-command-hook 'permanent-local-hook t)
    (put 'flyspell-pre-command-hook 'permanent-local-hook t)
    (put 'flyspell-after-change-function 'permanent-local-hook t)
    (put 'flyspell-hack-local-variables-hook 'permanent-local-hook t)
    (put 'flyspell-auto-correct-previous-hook 'permanent-local-hook t)

    (when mumamo-multi-major-mode
      (setq flyspell-generic-check-word-predicate 'mumamo-flyspell-verify))
    ))

(defun flyspell-mumamo-mode ()
  "Turn on `flyspell-mode' for multi major modes."
  (interactive)
  (setq flyspell-generic-check-word-predicate 'mumamo-flyspell-verify)
  (flyspell-mode 1)
  ;;(run-hooks 'flyspell-prog-mode-hook)
  )


(provide 'mumamo)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mumamo.el ends here
