;;; sepia-tree.el -- tree-widget-based calle[re] navigation

;; Copyright (C) 2004-2007 Sean O'Rourke.  All rights reserved, some
;; wrongs reversed.  This code is distributed under the same terms as
;; Perl itself.

;;; Commentary:

;; See the README file that comes with the distribution.

;;; Code:

(require 'tree-widget)

(defvar sepia-tree-use-image nil
  "*If non-nil, show tree-widget with icons.")

(defun sepia-tree-button-cb (widget &rest blah)
  (let* ((pw (widget-get widget :parent))
         (wid-name (widget-get pw :sepia-name))
         (location (and wid-name (car (xref-location wid-name)))))
    (cond
      ((not location) (error "Can't find %s." wid-name))
      (current-prefix-arg
       (find-file-other-window (car location))
       (sepia-set-found (list location) 'function)
       (sepia-next))
      ((widget-get widget :sepia-shown-p)
       (save-excursion
	 (end-of-line)
	 (let ((inhibit-read-only t))
	   (delete-region (point)
			  (+ 1 (point) (widget-get widget :sepia-shown-p))))
	 (widget-put widget :sepia-shown-p nil)))
      (t
       (let ((str (apply #'sepia-extract-def location)))
	 (if str
	     (save-excursion
	       (end-of-line)
	       (widget-put widget :sepia-shown-p (length str))
	       (widget-insert "\n" str))
	     (message "(not found)")))))))

(defun sepia-tree-node-cb (widget &rest blah)
  (let ((func (widget-get widget :sepia-func)))
    (or (widget-get widget :args)
	(let ((children (funcall func widget)))
	  (if children
	      (mapcar
	       (lambda (x) (sepia-tree-node func x))
	       children)
	      (widget-put widget :has-children nil))))))

(defun sepia-tree-node (func name)
  "Make a tree node for the object specified by FILE, LINE, OBJ,
and MOD.  The new node will have a property :sepia-X
corresponding to each of these values.  FUNC is a function that
will, given a widget, generate its children."
  `(tree-widget
    :node (push-button
	   :tag ,name
	   :format "%[%t%]\n"
	   :notify sepia-tree-button-cb)
    :dynargs sepia-tree-node-cb
    :has-children t
    :sepia-name ,name
    :sepia-func ,func))

(defun sepia-tree-tidy-buffer (name)
  "Get/create a new, tidy buffer for the tree widget."
  (switch-to-buffer name)
  (kill-all-local-variables)
  ;; because the widget images are ugly.
  (set (make-local-variable 'widget-image-enable) sepia-tree-use-image)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (let ((all (overlay-lists)))
    (mapcar #'delete-overlay (car all))
    (mapcar #'delete-overlay (cdr all)))
  (toggle-read-only 1)
  (view-mode -1))

(defun sepia-build-tree-buffer (func defs bufname)
  (if defs
      (lexical-let ((func func))
        (sepia-tree-tidy-buffer bufname)
        (with-current-buffer bufname
          (dolist (x defs)
            (widget-create
                   (sepia-tree-node
                    (lambda (widget)
                      (funcall func (widget-get widget :sepia-name)))
                    x)))
          (use-local-map (copy-keymap widget-keymap))
;;        (local-set-key "\M-." sepia-keymap)
;;        (sepia-install-keys)
          (let ((view-read-only nil))
            (toggle-read-only 1))
          (goto-char (point-min))
	  (message "Type C-h m for usage information")))
      (message "No items for %s" bufname)))

;;;###autoload
(defun sepia-callee-tree (name)
  "Create a tree view of a function's callees.

Pressing RET on a function's name displays its definition.  With
prefix argument, RET instead visits in another window."
  (interactive (let ((func (sepia-interactive-arg 'function))
                     (mod (sepia-interactive-module)))
                 (list (if mod (format "%s::%s" mod func)
                           func))))
  (let* ((defs (xref-apropos name)))
    (sepia-build-tree-buffer
     #'xref-callees
     defs
     (format "*%s callees*" name))))

(defun sepia-caller-tree (name)
  "Create a tree view of a function's callers.

Pressing RET on a function's name displays its definition.  With
prefix argument, RET instead visits in another window."
  (interactive (let ((func (sepia-interactive-arg 'function))
                     (mod (sepia-interactive-module)))
                 (list (if mod (format "%s::%s" mod func)
                           func))))
  (let* ((defs (xref-apropos name)))
    (sepia-build-tree-buffer
     #'xref-callers
     defs (format "*%s callers*" name))))

;;;###autoload
(defun sepia-module-callee-tree (mod)
  "Display a callee tree for each of MOD's subroutines.

Pressing RET on a function's name displays its definition.  With
prefix argument, RET instead visits in another window."
  (interactive (list (sepia-interactive-arg 'module)))
  (let ((defs (xref-mod-subs mod)))
    (sepia-build-tree-buffer #'xref-callees defs (format "*%s subs*" mod))))

(provide 'sepia-tree)
;;; sepia.el ends here
