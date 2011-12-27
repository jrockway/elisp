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

(require 'rcirc)

(defun irc-start nil
  "Connect to all my networks."
  (interactive)
  (let ((password (password-read "IRC password: "))
        (host "itchy.internal"))
    (loop for port from 6667 to 6678 do
          (rcirc-connect host port "jrockway" "jrockway"
                         "Jonathan Rockway" nil password))))

(defun rcirc-generate-new-buffer-name (process target)
  "Return a buffer name based on PROCESS and TARGET.
This is used for the initial name given to IRC buffers."
  (substring-no-properties
   (if target (generate-new-buffer-name target)
     (concat "*" (process-name process) "*"))))

(defun irc-go-away ()
  (interactive)
  (loop for buf in (buffer-list)
        when (with-current-buffer buf (eq 'rcirc-mode major-mode))
        do (bury-buffer buf)))

(defun irc-kill-connections ()
  "Kill rcirc buffers, killing the server-buffer first."
  (interactive)
  (let ((kill-buffer-query-functions nil)
        (killed-channels 0)
        (killed-servers 0))
    (loop for buf in (buffer-list) do
          (with-current-buffer buf
            (when (eq major-mode 'rcirc-mode)
              (when (and (buffer-live-p rcirc-server-buffer))
                (incf killed-servers)
                (kill-buffer rcirc-server-buffer))
              (when (buffer-live-p buf)
                (incf killed-channels)
                (kill-buffer buf)))))
    (message "Killed %s channels and %s servers"
             killed-channels
             killed-servers)))


(add-hook 'rcirc-mode-hook (lambda () (flyspell-mode 1)))
(add-hook 'rcirc-mode-hook (lambda () (rcirc-omit-mode)))
(add-hook 'rcirc-mode-hook
             (lambda ()
               (when (or (= (aref (buffer-name) 0) ?#)
                         (= (aref (buffer-name) 0) ?&))
                 (setq rcirc-ignore-buffer-activity-flag t))))

(provide 'rcirc-extras)
;;; rcirc-extras.el ends here
