;;; haskell-extras.el --- haskell stuff

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

(require 'eproject)

(define-project-type haskell (generic)
  (look-for "*.cabal" :glob)
  :relevant-files ("\\.hs" "\\.cabal" "\\.lhs"))

(defun setup-haskell-project-build ()
  (set (make-local-variable 'compile-command)
       (format "cd %s; cabal configure; cabal build" (eproject-root))))

(add-hook 'haskell-project-file-visit-hook #'setup-haskell-project-build)

(provide 'haskell-extras)
;;; haskell-extras.el ends here
