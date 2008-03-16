;;; tabkey2.el --- Use second tab key pressed for what you want
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2008-03-15T14:40:28+0100 Sat
(defconst tabkey2:version "1.05")
;; Last-Updated: 2008-03-16T00:04:43+0100 Sat
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
;; The tab key is in Emacs often used for indentation.  However if you
;; press the tab key a second time and Emacs tries to do indentation
;; again, then usually nothing exciting will happen.  Then why not use
;; second tab key in a row for something else?
;;
;; Commonly used completion functions in Emacs is often bound to
;; something corresponding to Alt-Tab.  Unfortunately this is unusable
;; if you have a window manager that have an apetite for it (like that
;; on MS Windows for example, and several on GNU/Linux).
;;
;; Then using the second tab key press for what is bound to Alt-Tab
;; might be a good choice and perhaps also easy to remember.
;;
;; This little library tries to make it easy to do use the second tab
;; press for that.  See `tabkey2-mode' for more information.
;;
;;
;; This is a generalization of an idea Sebastien Rocca Serra once
;; presented on Emacs Wiki and called "Smart Tab".
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; Version 1.04:
;; - Added overlay to display state after first tab.
;;
;; Version 1.05:
;; - Fixed remove overlay problem.
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

(defgroup tabkey2 nil
  "Customization of second tab key press."
  :group 'convenience)

;; (setq tabkey2-in-minibuffer t)
(defcustom tabkey2-in-minibuffer nil
  "If non-nil use `tabkey2' also in minibuffer."
  :type 'boolean
  :group 'tabkey2)

(defcustom tabkey2-avoid-modes nil
  "Don't use `tabkey2' for major modes in this list."
  :type '(repeat command)
  :group 'tabkey2)

(defcustom tabkey2-mark-indentation-after-first t
  "Mark indentation after first tab to show state."
  :type 'boolean
  :group 'tabkey2)

(defvar tabkey2-last-pos nil)

(defvar tabkey2-preferred nil
  "Preferred function for second tab key press.")
(make-variable-buffer-local 'tabkey2-preferred)
(put 'tabkey2-preferred 'permanent-local t)

(defvar tabkey2-fallback nil
  "Fallback function for second tab key press.")
(make-variable-buffer-local 'tabkey2-fallback)
(put 'tabkey2-fallback 'permanent-local t)

(defvar tabkey2-overlay nil
  "Visible when tab key 2 action is to be done.")

(defun tabkey2 ()
  "Do something else when tab is pressed a second time.
The tab key is easy to type on your keyboard.  Then why not use
it often?  This was the idea of Smart Tabs and this is a
generalization of that idea.

Often the tab key is used for indentation. The idea here is that
if indentation has been done, then if tab is pressed a second
time something else can be done.

This function is bound to the tab key when `tabkey2-mode' is
on. It works like this:

1. The first time tab is pressed it does whatever tab would have
   done if `tabkey2-mode' was off.

2. The second time tab is pressed it does something else. When
   deciding what to do it look in this table and do whatever it
   found first that is not nil:

   - `tabkey2-preferred'
   - Key bindings:
     [(meta tab)]
     [(meta ?\t)]
     [(shift meta tab)]
     [(shift meta ?\t)]
     [(control meta tab)]
     [(control meta ?\t)]
     [(shift control meta tab)]
     [(shift control meta ?\t)]
   - `tabkey2-fallback'

   However if either
     - the minibuffer is active and `tabkey2-in-minibuffer' is nil
     - the major mode is in `tabkey2-avoid-modes'
   then do not do anything special the second time.

Note that it looks for bindings to M-Tab which might be hard to
type on systems where Emacs uses Alt as its META key.  This is
where completion by default often are bound in Emacs.

NOTE: This uses `emulation-mode-map-alists' and it supposes that
nothing else is bound to tab there."
  (interactive)
  (let* ((emulation-mode-map-alists
          (delq 'tabkey2--emul-keymap-alist (copy-sequence emulation-mode-map-alists)))
         (to-do-1 (key-binding [?\t] t))
         (to-do-2 (or
                   ;; Some preferred use for 2nd tab key in this buffer?
                   tabkey2-preferred
                   ;; tab bindings are a bit weird:
                   (key-binding [(meta tab)])
                   (key-binding [(meta ?\t)])
                   (key-binding [(shift meta tab)])
                   (key-binding [(shift meta ?\t)])
                   (key-binding [(control meta tab)])
                   (key-binding [(control meta ?\t)])
                   (key-binding [(shift control meta tab)])
                   (key-binding [(shift control meta ?\t)])
                   ;; fallback
                   tabkey2-fallback
                   to-do-1))
         (is-first (or (null tabkey2-last-pos)
                       (not (eq (marker-buffer tabkey2-last-pos)
                                (current-buffer)))
                       (/= tabkey2-last-pos (point))
                       (not (eq last-command 'tabkey2))))
         (to-do (if (or is-first
                        (unless tabkey2-in-minibuffer
                          (active-minibuffer-window))
                        (memq major-mode tabkey2-avoid-modes))
                    to-do-1
                  to-do-2)))
    (setq tabkey2-last-pos (point-marker))
    (set-marker-insertion-type tabkey2-last-pos t)
    (let ((last-command to-do))
      (call-interactively to-do))
    ;; Assume that the first command was an indentation command. Mark
    ;; if to-do-1 /= to-do-2:
    (when (and tabkey2-mark-indentation-after-first
               (eq to-do to-do-1)
               (not (eq to-do-1 to-do-2)))
      (let* ((beg (line-beginning-position))
             (ind (current-indentation))
             (end (+ beg ind)))
        (when (= ind 0)
          (setq end (1+ end)))
        (when (< beg end)
          (when (and tabkey2-overlay
                     (overlay-buffer tabkey2-overlay))
            (delete-overlay tabkey2-overlay))
          (setq tabkey2-overlay (make-overlay beg end))
          (overlay-put tabkey2-overlay 'face 'highlight)
          (add-hook 'post-command-hook 'tabkey2-post-command-once))))))

(defun tabkey2-post-command-once ()
  (unless (eq this-command 'tabkey2)
    (condition-case err
        (progn
          (remove-hook 'post-command-hook 'tabkey2-post-command-once)
          (delete-overlay tabkey2-overlay))
      (error (message "tabkey2: %s" (error-message-string err))))))

(defvar tabkey2-mode-emul-map
  (let ((map (make-sparse-keymap)))
    (define-key map [tab] 'tabkey2)
    map))

(defconst tabkey2--emul-keymap-alist (list (cons 'tabkey2-mode
                                                 tabkey2-mode-emul-map)))

(define-minor-mode tabkey2-mode
  "More fun with tab key number two (completion etc).
This minor mode binds TAB to `tabkey2' in a way that lets you use
it in all buffers.  See that function for more information."
  :keymap nil
  :global t
  :group 'tabkey2
  (if tabkey2-mode
      (add-to-list 'emulation-mode-map-alists 'tabkey2--emul-keymap-alist)
    (setq emulation-mode-map-alists (delq 'tabkey2--emul-keymap-alist
                                          emulation-mode-map-alists))))

(provide 'tabkey2)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tabkey2.el ends here
