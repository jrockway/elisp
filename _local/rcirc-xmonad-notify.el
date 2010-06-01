;;; rcirc-xmonad-notify.el --- write rciric notifications to a pipe

;; Copyright (C) 2009  Jonathan Rockway

;; Author: Jonathan Rockway <jon@jrock.us>
;; Keywords: rcirc, xmobar

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
(require 'htmlize)

(defun rcirc-xmonad-write (data)
  (start-process "rcirc-xmonad-writer" nil "sh" "-c"
                 (format "echo ': %s' > /home/jon/tmp/emacs.fifo &" data)))

(defun rcirc-xmonad-notify () nil)
;;  (rcirc-xmonad-write (xmobarize-string rcirc-activity-string)))

(add-hook 'rcirc-update-activity-string-hook #'rcirc-xmonad-notify)

(defun htmlize-string (string)
  "Convert STRING to HTML."
  (let ((html (with-temp-buffer
                (insert string)
                (let ((htmlize-output-type 'font))
                  (with-current-buffer (htmlize-buffer)
                    (prog1 (buffer-substring-no-properties (point-min) (point-max))
                      (kill-buffer)))))))
    (with-temp-buffer
      (insert html)
      (let (start end)
        (goto-char (point-min))
        (re-search-forward "<pre>
?")
        (setq start (match-end 0))
        (re-search-forward "</pre>")
        (setq end (match-beginning 0))
        (buffer-substring-no-properties start end)))))

(defun xmobarize-string (string)
  (let ((html (htmlize-string string)))
    (with-string-buffer html
      (goto-char (point-min))
      (while (re-search-forward "<font color=\"#\\([^\"]+\\)\">" nil t)
        (replace-match (format "<fc=#%s,#000000>" (match-string 1))))

      (goto-char (point-min))
      (while (re-search-forward "</font>" nil t)
        (replace-match "</fc>"))

      (goto-char (point-min))
      (while (re-search-forward "\\(?:<b>\\|</b>\\)" nil t)
        (replace-match ""))

      ;; wow, this is some good programming here...
      (goto-char (point-min))
      (while (re-search-forward "&amp;" nil t)
        (replace-match "&"))

      (goto-char (point-min))
      (while (re-search-forward "&lt;" nil t)
        (replace-match "<"))

      (goto-char (point-min))
      (while (re-search-forward "&gt;" nil t)
        (replace-match ">")))))

(provide 'rcirc-xmonad-notify)
;;; rcirc-xmonad-notify.el ends here
