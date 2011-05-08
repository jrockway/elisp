;;; buffer-faces.el --- show all faces being used in a buffer

;; Copyright (C) 2011  Jonathan Rockway

;; Author: Jonathan Rockway <jon@jrock.us>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defun buffer-faces--build-faces (&optional buffer)
  (let (faces
        (inhibit-point-motion-hooks t))
    (save-excursion
      (with-current-buffer (or buffer (current-buffer))
        (goto-char (point-min))
        (while (/= (point) (point-max))
          (add-to-list 'faces (get-text-property (point) 'face))
          (goto-char (next-property-change (point) nil (point-max))))))
    (delete-if #'null faces)))

(defun buffer-faces--build-buffer (faces)
  (with-current-buffer (get-buffer-create "*faces*")
    (delete-region (point-min) (point-max))
    (loop for face in (sort faces 'eq) do
          (insert (propertize (format "%s\n" face) 'face face)))
    (current-buffer)))

(defun show-buffer-faces ()
  "Show a buffer containing each face used in the current buffer."
  (interactive)
  (pop-to-buffer
   (buffer-faces--build-buffer (buffer-faces--build-faces))))

(provide 'buffer-faces)
;;; buffer-faces.el ends here
)