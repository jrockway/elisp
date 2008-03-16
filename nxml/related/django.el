;;; django.el ---
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: Sun Nov 18 18:29:41 2007
;; Version: 0.2
;; Last-Updated: Tue Nov 20 00:40:56 2007 (3600 +0100)
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
;; Simple highlighting for Django, mostly for use with
;; `mumamo-mode'. To use it with mumamo you can either give your files
;; the extension .djhtml - which should make mumamo-mode choose
;; "Django nXhtml Family" - or use
;;
;;   M-x mumamo-set-chunk-family
;;
;; to set the chunk family.
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

(defconst django-font-lock-keywords
  (list
   (cons (rx "{% comment %}" (submatch (0+ anything)) "{% endcomment %}") (list 1 font-lock-comment-face))
   '("{%\\|\\%}\\|{{\\|}}\\|{#\\|#}" . font-lock-preprocessor-face)
   '("{# ?\\(.*?\\) ?#}" . (1 font-lock-comment-face))
   '("{{ ?\\([^|]*?\\)\\(|.*?\\)? ?}}" . (1 font-lock-variable-name-face))
   (cons (rx
          word-start
          (or "as" "in"
              (seq
               (opt "end")
               (or "autoescape" "block" "comment" "cycle" "debug" "else"
                   "extends" "filter" "firstof" "for" "if" "ifchanged" "ifequal"
                   "ifnotequal" "include" "load" "now" "regroup"
                   "spaceless" "ssi" "templatetag" "url" "widthratio"
                   "with")))
          word-end)
         font-lock-builtin-face)
   ;; Built in filters:
   (cons (rx
          "|"
          (submatch
           (or "add" "addslashes" "capfirst" "center" "cut"
               "date" "default" "default_if_none"
               "dictsort" "dictsortreversed"
               "divisibleby"
               "escape"
               "filesizeformat"
               "first"
               "fixampersands"
               "floatformat"
               "force_escape"
               "iriencode"
               "join"
               "length" "length_is"
               "linebreaks" "linebreaksbr" "linenumbers"
               "ljust"
               "lower"
               "make_list"
               "phone2numeric"
               "pluralize"
               "pprint"
               "random"
               "removetags"
               "rjust"
               "safe" "slice" "slugify" "stringformat" "striptags"
               "time" "timesince" "timeuntil"
               "title" "truncatewords" "truncatewords_html"
               "unordered_list"
               "upper" "urlencode" "urlize" "urlizetrunc"
               "wordcount" "wordwrap" "yesno")))
         (list 1 font-lock-function-name-face))
   )
   "Minimal highlighting expressions for Django mode")

(define-derived-mode django-mode nil "Django"
  "Simple Django mode for use with `mumamo-mode'.
This mode only provides syntax highlighting."
  ;;(set (make-local-variable 'comment-start) "{#")
  ;;(set (make-local-variable 'comment-end)   "#}")
  (setq font-lock-defaults '(django-font-lock-keywords)))

(provide 'django)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; django.el ends here
