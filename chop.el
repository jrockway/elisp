;;; chop.el -- Interactive binary search for a line within a window.
;;; Written by Luke Gorrie <luke@bluetail.com>. Version 1.0, May 2002.
;;; Probably only works in GNU Emacs 21.

;; Interactively-driven binary search to move the point to a
;; particular line on the screen, by successive choppings in half.

;; Setup:
;;
;; Bind move-chop-{up,down} to keys. Otherwise they probably won't
;; work right, since M-x mucks with `last-command'.

;; Instructions:
;;
;; Use the following algorithm to move to any visible line in O(log N)
;; steps for a window N lines high:
;;
;; 1. Choose a line, L.
;; 2. Press either UP or DOWN. This starts by moving the point to the center.
;; 3. Repeat until L is the current line (or close enough):
;;      If L is above the point, press UP. Otherwise press DOWN.
;;
;; In practice, a few chops usually gets you pretty close. Then you
;; can zero in with line/paragraph/defun-based motion.

(defvar chop-size nil
  "Number of lines that the next \"chop\" will contain, as floating-point.
Only meaningful for consecutive chops.")

(defvar chop-current-line nil
  "Current line number, as floating-point.
Only meaningful for consecutive chops.")

(defun chop-move-up ()
  "Move by one 'chop' into the upper half of the remaining space."
  (interactive)
  (chop-move -1))

(defun chop-move-down ()
  "Move by one 'chop' into the lower half of the remaining space."
  (interactive)
  (chop-move 1))

(defun chop-move (dir)
  "Move by one 'chop'. DIR is the direction: -1 for upwards, 1 for downwards."
  (setq this-command 'chop-move)
  (if (chop-new-p)
      (chop-first)
    (chop-next dir)))

(defun chop-new-p ()
  (or current-prefix-arg
      (not (eq last-command 'chop-move))))

(defun chop-first ()
  "Make a first chop, leaving the point in the middle of the window."
  (let ((half (/ (chop-last-line-number) 2.0)))
    (setq chop-size half)
    (setq chop-current-line half)
    (move-to-window-line (round half))))

(defun chop-next (dir)
  "Make the next chop."
  (setq chop-size (/ chop-size 2))
  (incf chop-current-line (* dir chop-size))
  (move-to-window-line (min (chop-last-line-number)
			    (round chop-current-line))))

(defun chop-last-line-number ()
  "Window height, minus 1 to index from 0, minus 1 to account for modeline."
  (- (window-height) 2))

(provide 'chop)

;;; chop.el ends here
