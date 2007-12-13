;; This was hacked together by Jorgen Schäfer
;; And hacked again by Yann Hodique
;; Donated to the public domain. Use at your own risk.

(defgroup visible-mark nil
  "Show the position of your mark."
  :group 'convenience
  :prefix "visible-mark-")

(defface visible-mark-face
  `((((type tty) (class color))
     (:background "blue" :foreground "white"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color) (background dark))
     (:background "blue"))
    (((class color) (background light))
     (:background "lightblue"))
    (t (:background "gray")))
  "Face for the mark."
  :group 'visible-mark)

(defvar visible-mark-overlay nil
  "The overlay used in this buffer.")
(make-variable-buffer-local 'visible-mark-overlay)

(defun visible-mark-move-overlay ()
  "Move the overlay in `visible-mark-overlay' to a new position."
  (let ((m (mark)))
    (when m (move-overlay visible-mark-overlay m (1+ m)))))

(require 'easy-mmode)

(defcustom global-visible-mark-mode-exclude-alist nil
  "A list of buffer names to be excluded"
  :group 'visible-mark
  :type '(repeat regexp))

(defun visible-mark-mode-maybe ()
  (when (cond
         ((minibufferp (current-buffer)) nil)
         ((flet ((fun (arg)
                      (if (null arg) nil
                        (or (string-match (car arg) (buffer-name))
                            (fun (cdr arg))))))
            (fun global-visible-mark-mode-exclude-alist)) nil)
         (t t))
    (visible-mark-mode)))

(define-minor-mode visible-mark-mode
  "A mode to make the mark visible."
  nil nil nil
  :group 'visible-mark
  (if visible-mark-mode
      (let ((m (or (mark) (set-mark (point-min)))))
        (unless visible-mark-overlay
          (setq visible-mark-overlay (make-overlay m (1+ m)))
          (overlay-put visible-mark-overlay 'face 'visible-mark-face)
          (add-hook 'post-command-hook 'visible-mark-move-overlay)))
      (when visible-mark-overlay
        (delete-overlay visible-mark-overlay)
        (setq visible-mark-overlay nil))))

(easy-mmode-define-global-mode
 global-visible-mark-mode visible-mark-mode visible-mark-mode-maybe :group 'visible-mark)

(provide 'visible-mark-mode)
