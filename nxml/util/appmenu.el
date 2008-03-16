;;; appmenu.el --- A framework for [apps] popup menus.

;; Copyright (C) 2005 by Lennart Borgman

;; Author:  Lennart Borgman <lennart DOT borgman DOT 073 AT student DOT lu DOT se>
;; Created: Thu Jan 05 14:00:26 2006
(defconst appmenu:version "0.60") ;; Version:
;; Last-Updated: Tue Sep 11 03:04:55 2007 (7200 +0200)
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
;;  This is maybe a somewhat preliminary version!
;;
;;  appmenu.el is a framework for creating cooperative context
;;  sensitive popup menus with commands from different major and minor
;;  modes. For more information see `appmenu-mode'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; Version 0.60:
;; - Removed support for minor and major menus.
;; - Added support for text and overlay keymaps.
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

(defcustom appmenu-items-visible nil
  "Non-nil means show AppMenu help on AppMenu popup."
  :type 'boolean
  :group 'appmenu)

;; These are for other elisp modules to use:
;; (defvar appmenu-always-alist nil
;;   "List of menu keymaps to always show.")

(defvar appmenu-alist nil
  "Alist of additional menu keymaps.
The entries in this list are cons cells:

   (TEST . (TITLE . DEFINITION))

TEST should be a form to evaluate.  The entry is used if (eval
TEST) returns non-nil.

DEFINITION should be either a keymap or a function that returns a
keymap.

The function must take no argument and return a keymap.  If the
function returns nil then the entry is not shown in the popup
menu.  Using this you can make context sensitive popup menus.

For an example of use see mlinks.el.")



(defun appmenu-help ()
  (interactive)
  (describe-function 'appmenu-mode))

(defun appmenu-keymap-len (map)
  (let ((ml 0))
    (map-keymap (lambda (e f) (setq ml (1+ ml))) map)
    ml))

(defvar appmenu-mouse-only
  '(flyspell-correct-word))

(defun appmenu-make-menu-for-point ()
  "Constuct a menu based on point keymap."
  (let ((point-map (get-char-property (point) 'keymap))
        funs
        (map (make-sparse-keymap "At point"))
        (num 0)
        last-prefix
        this-prefix)
    (when point-map
      (map-keymap (lambda (key fun)
                    (when (and (symbolp fun)
                               (fboundp fun)
                               (not (memq fun appmenu-mouse-only))
                               )
                      (add-to-list 'funs fun)))
                  point-map)
      (dolist (fun funs)
        (let ((desc (when fun (documentation fun))))
          (when desc
            (setq desc (car (split-string desc "[\n]")))
            ;;(lwarn t :warning "pk: %s, %s" fun desc)
            (setq this-prefix
                  (car (split-string (symbol-name fun) "[-]")))
            (when (and last-prefix
                       (not (string= last-prefix this-prefix)))
              (define-key map
                (vector (intern (format "appmenu-point-div-%s" num)))
                (list 'menu-item "--")))
            (setq last-prefix this-prefix)
            (setq num (1+ num))
            (define-key map
              (vector (intern (format "appmenu-point-%s" num)))
              (list 'menu-item desc fun))))))
    (when (> num 0) map)))

(defun appmenu-map ()
  (let* ((map (make-sparse-keymap
               "AppMenu"
               ))
         (map-len (appmenu-keymap-len map))
         (map-init-len map-len)
         (num-minor 0)
         (id 0)
         (point-menu (appmenu-make-menu-for-point)))
    ;; AppMenu itself
    (when appmenu-items-visible
      (define-key map [appmenu-customize]
        (list 'menu-item "Customize AppMenu"
              (lambda () (interactive) (customize-group 'appmenu))
              :help "Customize AppMenu"
              :visible 'appmenu-items-visible))
      (define-key map [appmenu-help]
        (list 'menu-item "Help for AppMenu" 'appmenu-help
              :help "Help for how to use AppMenu"
              :visible 'appmenu-items-visible))
      (define-key map [appmenu-separator-1]
        (list 'menu-item "--")))
    (setq map-len (appmenu-keymap-len map))
    (dolist (rec appmenu-alist)
      (let* ((test (car rec))
             (titdef (cdr rec))
             (title (car titdef))
             (mapdef (cdr titdef))
             (usedef (if (symbolp mapdef)
                         (funcall mapdef)
                       mapdef)))
        (when (and usedef
                   (eval test))
          (setq id (1+ id))
          (define-key map
            (vector (intern (format "appmenu-%s" id)))
            (list 'menu-item title usedef)))
        ))
;;;     (let ((map-len-new (appmenu-keymap-len map)))
;;;       (when (> map-len-new map-len)
;;;         (define-key map [appmenu-header-always]
;;;           (list 'menu-item "--")))
;;;       (setq map-len map-len-new))
    (when point-menu
      (setq map-len (appmenu-keymap-len map))
      (when (> map-len map-init-len)
        (define-key map [appmenu-at-point-div]
          (list 'menu-item "--")))
      (define-key map [appmenu-at-point]
        (list 'menu-item "At current point"
              point-menu)))
    (setq map-len (appmenu-keymap-len map))
    (when (> map-len map-init-len)
      map)))

(eval-when-compile (require 'cl))
(defun appmenu-get-submenu (menu-command)
  (let (subtitle submenumap)
    (if (eq 'menu-item (car menu-command))
        (progn (setq subtitle   (cadr  menu-command))
               (setq submenumap (caddr menu-command)))
      (setq subtitle   (car menu-command))
      (setq submenumap (cdr menu-command)))
    (unless (keymapp submenumap) (error "submenu not a keymap=%s" submenumap))
    (cons subtitle submenumap)))

(defun appmenu-popup ()
  "Pops up the AppMenu menu."
  (interactive)
  (let* ((mod (event-modifiers last-input-event))
         (is-mouse (or (memq 'click mod)
                       (memq 'down  mod)
                       (memq 'drag  mod))))
    (when is-mouse
      (goto-char (posn-point (event-start last-input-event)))
      (sit-for 0.01))
    ;;(active-minibuffer-window)
    (condition-case err
        (let ((menu (appmenu-map)))
          (if menu
              (popup-menu-at-point menu)
            (message "Appmenu is empty")))
      (quit nil))))

(defvar appmenu-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [apps] 'appmenu-popup)
    (define-key map [mouse-3] 'appmenu-popup)
    map))

(define-minor-mode appmenu-mode
  "Use a context sensitive popup menu.
AppMenu (appmenu.el) is a framework for creating cooperative
context sensitive popup menus with commands from different major
and minor modes. Using this different modes may cooperate about
the use of popup menus.

By default the popup menu is on [apps] and [mouse-3].

The variable `appmenu-alist' is where the popup menu entries
comes from.

If there is a `keymap' property at point then relevant bindings
from this is also shown in the popup menu.

You can write functions that use whatever information you want in
Emacs to construct these entries. Since this information is only
collected when the popup menu is shown you do not have to care as
much about computation time as for entries in the menu bar."
  :global t
  :keymap appmenu-mode-map)
(when (and appmenu-mode
           (not (boundp 'define-globa-minor-mode-bug)))
  (appmenu-mode 1))

(provide 'appmenu)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; appmenu.el ends here

; LocalWords:  appmenu setq AppMenu
