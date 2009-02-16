;;; rcirc-extras.el --- my fixes for rcirc

;; Copyright (C) 2009  Jonathan Rockway

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


(defun rcirc-generate-new-buffer-name (process target)
  "Return a buffer name based on PROCESS and TARGET.
This is used for the initial name given to IRC buffers."
  (substring-no-properties
   (if target (generate-new-buffer-name target)
     (concat "*" (process-name process) "*"))))

(provide 'rcirc-extras)
;;; rcirc-extras.el ends here
