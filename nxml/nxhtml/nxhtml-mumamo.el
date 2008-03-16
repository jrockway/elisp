;;; nxhtml-mumamo.el --- Multi major modes using nxhtml
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2008-03-10T19:04:20+0100 Mon
(defconst nxhtml-mumamo:version "0.5")
;; Last-Updated: x
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'mumamo)

;; (defgroup nxhtml-auto-val-head nil
;;   "Automatic turn on of XHTML validation headers."
;;   :group 'nxhtml)

;; (defmacro define-fictive-validation-header-toggle (fun-sym default-value)
;;   (let* ((fun-name (symbol-name fun-sym))
;;          (custom-sym (intern (concat fun-name "-auto-val-head")))
;;          (hook-sym (intern-soft (concat fun-name "-hook")))
;;          (docstring
;;           (concat "Automatic XHTML validation header for `" fun-name "'.
;; ´")))
;;     (assert hook-sym)
;;     `(defcustom ,custom-sym ,default-value
;;        ,docstring
;;        :type 'boolean
;;        :set (lambda (sym val)
;;               (set-default sym val)
;;               (if val
;;                   (add-hook ',hook-sym 'nxhtml-turn-on-validation-header-mode)
;;                 (remove-hook ',hook-sym 'nxhtml-turn-on-validation-header-mode)))
;;        :group 'nxhtml-auto-val-head)
;;     ))

;;;###autoload
(define-mumamo-multi-major-mode nxhtml-mumamo
  "Turn on multiple major modes for (X)HTML with main mode `nxhtml-mode'.
This covers inlined style and javascript and PHP."
  ("nXhtml Family" nxhtml-mode
   (mumamo-chunk-xml-pi
    mumamo-chunk-xml-pi2
    mumamo-chunk-inlined-style
    mumamo-chunk-inlined-script
    mumamo-chunk-style=
    mumamo-chunk-onjs=
    )))
;;(define-fictive-validation-header-toggle nxhtml-mumamo t)

;;;###autoload
(define-mumamo-multi-major-mode embperl-nxhtml-mumamo
  "Turn on multiple major modes for Embperl files with main mode `nxhtml-mode'.
This also covers inlined style and javascript."
  ("Embperl nXhtml Family" nxhtml-mode
   (mumamo-chunk-embperl-<-
    mumamo-chunk-embperl-<+
    mumamo-chunk-embperl-<!
    mumamo-chunk-embperl-<$
    mumamo-chunk-inlined-style
    mumamo-chunk-inlined-script
    mumamo-chunk-style=
    mumamo-chunk-onjs=
    )))

;;;###autoload
(define-mumamo-multi-major-mode jsp-nxhtml-mumamo
  "Turn on multiple major modes for JSP with main mode `nxhtml-mode'.
This also covers inlined style and javascript."
  ("JSP nXhtml Family" nxhtml-mode
   (mumamo-chunk-jsp
    mumamo-chunk-inlined-style
    mumamo-chunk-inlined-script
    mumamo-chunk-style=
    mumamo-chunk-onjs=
    )))

;;;###autoload
(define-mumamo-multi-major-mode asp-nxhtml-mumamo
  "Turn on multiple major modes for ASP with main mode `nxhtml-mode'.
This also covers inlined style and javascript."
  ("ASP nXhtml Family" nxhtml-mode
   (mumamo-chunk-asp
    mumamo-asp-chunk-inlined-script
    mumamo-chunk-inlined-script
    mumamo-chunk-style=
    mumamo-chunk-onjs=
    )))

;;;###autoload
(define-mumamo-multi-major-mode django-nxhtml-mumamo
  "Turn on multiple major modes for Django with main mode `nxhtml-mode'.
This also covers inlined style and javascript."
  ("Django nXhtml Family" nxhtml-mode
   (mumamo-chunk-django4
    mumamo-chunk-django
    mumamo-chunk-django2
    mumamo-chunk-django3
    mumamo-chunk-inlined-style
    mumamo-chunk-inlined-script
    mumamo-chunk-style=
    mumamo-chunk-onjs=
    )))

;;;###autoload
(define-mumamo-multi-major-mode eruby-nxhtml-mumamo
  "Turn on multiple major modes for eRuby with main mode `nxhtml-mode'.
This also covers inlined style and javascript."
  ("eRuby nXhtml Family" nxhtml-mode
   (mumamo-chunk-eruby
    mumamo-chunk-inlined-style
    mumamo-chunk-inlined-script
    mumamo-chunk-style=
    mumamo-chunk-onjs=
    )))

;;;###autoload
(define-mumamo-multi-major-mode smarty-nxhtml-mumamo
  "Turn on multiple major modes for Smarty with main mode `nxhtml-mode'.
This also covers inlined style and javascript."
  ("Smarty nXhtml Family" nxhtml-mode
   (mumamo-chunk-xml-pi
    mumamo-chunk-smarty
    mumamo-chunk-style=
    mumamo-chunk-onjs=
    )))

(eval-after-load 'php-mode '(fmode-replace-default-mode 'php-mode 'nxhtml-mumamo))



(provide 'nxhtml-mumamo)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nxhtml-mumamo.el ends here
