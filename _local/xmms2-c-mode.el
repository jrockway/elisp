;;; xmms2-c-mode.el --- c-mode for editing xmms2 files

;; Copyright (C) 2009  Jonathan Rockway

;; Author: Jonathan Rockway <jon@jrock.us>
;; Keywords: languages

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

;; from the wiki:

;;; Code:


(define-project-attribute '("xmms2-devel" . :project-name)
  '(:is-xmms t))

(add-hook 'c-mode-hook
          (lambda ()
            (ignore-errors
              (eproject-maybe-turn-on)
              (when (eproject-attribute :is-xmms)
                (c-set-style "K&R")
                (setq tab-width 4)
                (setq indent-tabs-mode t)
                (setq c-basic-offset 4)))))

(provide 'xmms2-c-mode)
;;; xmms2-c-mode.el ends here
