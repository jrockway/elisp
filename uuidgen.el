;;; uuidgen.el --- generate and insert random UUIDs

;; Copyright (C) 2011  Jonathan Rockway

;; Author: Jonathan Rockway <jon@jrock.us>
;; Keywords: processes

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

(defun uuidgen (&optional time-p)
  "When called interactively, insert a UUID at the point.  When called from a program, return the UUID.
Optional argument TIME-P generates a time-based UUID instead of a random UUID."
  (interactive "P")
  (let ((uuid
         (with-temp-buffer
           (shell-command
            (format "uuidgen%s" (if time-p " -t" ""))
            (current-buffer))
           (buffer-substring-no-properties (point-min) (line-end-position)))))
    (if (called-interactively-p)
        (insert uuid)
      uuid)))

(provide 'uuidgen)
;;; uuidgen.el ends here
