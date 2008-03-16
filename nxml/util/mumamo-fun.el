;;; mumamo-fun.el --- Multi major mode functions
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2008-03-09T01:35:21+0100 Sun
;; Version: 0.5
;; Last-Updated: 2008-03-09T03:09:31+0100 Sun
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   `cl', `mumamo', `sgml-mode'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Defines some "multi major modes" functions. See mumamo.el for more
;; information.
;;
;;;; Usage:
;;
;;  See mumamo.el for how to use the multi major mode functions
;;  defined here.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile (add-to-list 'load-path default-directory))
(require 'mumamo)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Chunk search routines for XHTML things

;;;; xml pi

(defvar mumamo-xml-pi-mode-alist
  '(("php" . php-mode))
  "Alist used by `mumamo-chunk-xml-pi' to get exception mode." )

(defun mumamo-search-bw-exc-start-xml-pi-1 (pos min lt-chars)
  (let ((exc-start (mumamo-chunk-start-bw-str pos min lt-chars))
        spec
        exc-mode
        hit)
    (when exc-start
      (goto-char exc-start)
      (when (and (not (looking-at "xml"))
                 (looking-at (rx (0+ (any "a-z")))))
        (setq exc-start (match-end 0))
        (setq spec (match-string-no-properties 0))
        (setq exc-mode (assoc spec mumamo-xml-pi-mode-alist))
        (when exc-mode (setq exc-mode (cdr exc-mode)))
        (setq hit t)
        )
      (when hit
        (unless exc-mode
          (setq exc-mode 'fundamental-mode))
        (when (<= exc-start pos)
          (cons exc-start exc-mode))))))

(defun mumamo-search-bw-exc-start-xml-pi (pos min)
  (mumamo-search-bw-exc-start-xml-pi-1 pos min "<?"))

(defun mumamo-search-bw-exc-start-xml-pi2 (pos min)
  (mumamo-search-bw-exc-start-xml-pi-1 pos min "«?"))

(defun mumamo-search-bw-exc-end-xml-pi (pos min)
  (mumamo-chunk-end-bw-str pos min "?>"))

(defun mumamo-search-bw-exc-end-xml-pi2 (pos min)
  (mumamo-chunk-end-bw-str pos min "?»"))

(defun mumamo-search-fw-exc-end-xml-pi (pos max)
  (mumamo-chunk-end-fw-str pos max "?>"))

(defun mumamo-search-fw-exc-end-xml-pi2 (pos max)
  (mumamo-chunk-end-fw-str pos max "?»"))

(defun mumamo-search-fw-exc-start-xml-pi-1 (pos max lt-chars)
  (goto-char pos)
  (skip-chars-backward "a-zA-Z")
  (let ((end-out (mumamo-chunk-start-fw-str (point) max lt-chars)))
    (when (looking-at "xml")
      (setq end-out nil))
    (when end-out
      ;; Get end-out:
      (when (looking-at (rx (0+ (any "a-z"))))
        (setq end-out (match-end 0))))
    end-out))

(defun mumamo-search-fw-exc-start-xml-pi (pos max)
  (mumamo-search-fw-exc-start-xml-pi-1 pos max "<?"))

(defun mumamo-search-fw-exc-start-xml-pi2 (pos max)
  (mumamo-search-fw-exc-start-xml-pi-1 pos max "«?"))

(defun mumamo-chunk-xml-pi (pos min max)
  "Find process instruction, <? ... ?>.  Return range and wanted mode."
  (mumamo-find-possible-chunk pos min max
                              'mumamo-search-bw-exc-start-xml-pi
                              'mumamo-search-bw-exc-end-xml-pi
                              'mumamo-search-fw-exc-start-xml-pi
                              'mumamo-search-fw-exc-end-xml-pi))

(defun mumamo-chunk-xml-pi2(pos min max)
  "Find faked process instruction, <? ... ?>.  Return range and wanted mode.
For use with nxthml-strval.el."
  (mumamo-find-possible-chunk pos min max
                              'mumamo-search-bw-exc-start-xml-pi2
                              'mumamo-search-bw-exc-end-xml-pi2
                              'mumamo-search-fw-exc-start-xml-pi2
                              'mumamo-search-fw-exc-end-xml-pi2))

;;;; <style ...>

(defconst mumamo-style-tag-start-regex
  (rx "<style"
      space
      (0+ (not (any ">")))
      "type"
      (0+ space)
      "="
      (0+ space)
      ?\"
      "text/css"
      ?\"
      (0+ (not (any ">")))
      ">"
      ;; FIX-ME: Commented out because of bug in Emacs
      ;;
      ;;(optional (0+ space) "<![CDATA[")
      ))

(defun mumamo-search-bw-exc-start-inlined-style (pos min)
  (goto-char (+ pos 6))
  (let ((marker-start (search-backward "<style" min t))
        exc-mode
        exc-start)
    (when marker-start
      (when (looking-at mumamo-style-tag-start-regex)
        (setq exc-start (match-end 0))
        (goto-char exc-start)
        (when (<= exc-start pos)
          (cons (point) 'css-mode))
        ))))

(defun mumamo-search-bw-exc-end-inlined-style (pos min)
  (mumamo-chunk-end-bw-str pos min "</style>"))

(defun mumamo-search-fw-exc-start-inlined-style (pos max)
  (goto-char (1+ pos))
  (skip-chars-backward "^<")
  ;; Handle <![CDATA[
  (when (and
         (eq ?< (char-before))
         (eq ?! (char-after))
         (not (bobp)))
    (backward-char)
    (skip-chars-backward "^<"))
  (unless (bobp)
    (backward-char 1))
  (let ((exc-start (search-forward "<style" max t))
        exc-mode)
    (when exc-start
      (goto-char (- exc-start 6))
      (when (looking-at mumamo-style-tag-start-regex)
        (goto-char (match-end 0))
        (point)
        ))))

(defun mumamo-search-fw-exc-end-inlined-style (pos max)
  (mumamo-chunk-end-fw-str pos max "</style>"))

(defun mumamo-chunk-inlined-style (pos min max)
  "Find <style>...</style>.  Return range and 'css-mode."
  (mumamo-find-possible-chunk pos min max
                              'mumamo-search-bw-exc-start-inlined-style
                              'mumamo-search-bw-exc-end-inlined-style
                              'mumamo-search-fw-exc-start-inlined-style
                              'mumamo-search-fw-exc-end-inlined-style))

;;;; <script ...>

(defconst mumamo-script-tag-start-regex
  (rx "<script"
      space
      (0+ (not (any ">")))
      "type"
      (0+ space)
      "="
      (0+ space)
      ?\"
      ;;(or "text" "application")
      ;;"/"
      ;;(or "javascript" "ecmascript")
      "text/javascript"
      ?\"
      (0+ (not (any ">")))
      ">"
      ;; FIX-ME: Commented out because of bug in Emacs
      ;;
      ;;(optional (0+ space) "<![CDATA[" )
      ))

(defun mumamo-search-bw-exc-start-inlined-script (pos min)
  (goto-char (+ pos 7))
  (let ((marker-start (search-backward "<script" min t))
        exc-mode
        exc-start)
    (when marker-start
      (when (looking-at mumamo-script-tag-start-regex)
        (setq exc-start (match-end 0))
        (goto-char exc-start)
        (when (<= exc-start pos)
          (cons (point) 'javascript-mode))
        ))))
(defun mumamo-search-bw-exc-end-inlined-script (pos min)
  (mumamo-chunk-end-bw-str pos min "</script>"))
(defun mumamo-search-fw-exc-start-inlined-script (pos max)
  (goto-char (1+ pos))
  (skip-chars-backward "^<")
  ;; Handle <![CDATA[
  (when (and
         (eq ?< (char-before))
         (eq ?! (char-after))
         (not (bobp)))
    (backward-char)
    (skip-chars-backward "^<"))
  (unless (bobp)
    (backward-char 1))
  (let ((exc-start (search-forward "<script" max t))
        exc-mode)
    (when exc-start
      (goto-char (- exc-start 7))
      (when (looking-at mumamo-script-tag-start-regex)
        (goto-char (match-end 0))
        (point)
        ))))
(defun mumamo-search-fw-exc-end-inlined-script (pos max)
  (mumamo-chunk-end-fw-str pos max "</script>"))

(defun mumamo-chunk-inlined-script (pos min max)
  "Find <script>...</script>.  Return range and 'javascript-mode."
  (mumamo-find-possible-chunk pos min max
                              'mumamo-search-bw-exc-start-inlined-script
                              'mumamo-search-bw-exc-end-inlined-script
                              'mumamo-search-fw-exc-start-inlined-script
                              'mumamo-search-fw-exc-end-inlined-script))

;;;; on[a-z]+=\"javascript:"

(defconst mumamo-onjs=start-regex
  (rx "<"
      (0+ (not (any ">")))
      space
      "on"
      (1+ (any "a-za-z"))
      "="
      (0+ space)
      ?\"
      (submatch
       "javascript:"
       (0+
        (not (any "\""))))
      ))

(defun mumamo-chunk-onjs=(pos min max)
  "Find javascript on...=\"...\".  Return range and 'javascript-mode."
  (mumamo-chunk-attr= pos min max "on[a-z]+=" t mumamo-onjs=start-regex
                      'javascript-mode))

;;;; style=

(defconst mumamo-style=start-regex
  (rx "<"
      (0+ (not (any ">")))
      space
      "style="
      (0+ space)
      ?\"
      (submatch
       (0+
        (not (any "\""))))
      ))

(defun mumamo-chunk-style=(pos min max)
  "Find style=\"...\".  Return range and 'css-mode."
  (mumamo-chunk-attr= pos min max "style=" nil mumamo-style=start-regex
                      'css-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; HTML w html-mode

;;;###autoload
(define-mumamo-multi-major-mode html-mumamo
  "Turn on multiple major modes for (X)HTML with main mode `html-mode'.
This covers inlined style and javascript and PHP."
  ("HTML Family" html-mode
   (mumamo-chunk-xml-pi
    mumamo-chunk-inlined-style
    mumamo-chunk-inlined-script
    mumamo-chunk-style=
    mumamo-chunk-onjs=
    )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; XHTML w nxml-mode

;;;###autoload
(define-mumamo-multi-major-mode nxml-mumamo
  "Turn on multiple major modes for (X)HTML with main mode `nxml-mode'.
This covers inlined style and javascript and PHP."
    ("nXml Family" nxml-mode
     (mumamo-chunk-xml-pi
      mumamo-chunk-inlined-style
      mumamo-chunk-inlined-script
      mumamo-chunk-style=
      mumamo-chunk-onjs=
      )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Embperl

(defun mumamo-chunk-embperl-<- (pos min max)
  "Find [- ... -], return range and perl-mode."
  (mumamo-quick-static-chunk pos min max "[-" "-]" nil 'perl-mode))

(defun mumamo-chunk-embperl-<+ (pos min max)
  "Find [+ ... +], return range and perl-mode."
  (mumamo-quick-static-chunk pos min max "[+" "+]" nil 'perl-mode))

(defun mumamo-chunk-embperl-<! (pos min max)
  "Find [! ... !], return range and perl-mode."
  (mumamo-quick-static-chunk pos min max "[!" "!]" nil 'perl-mode))

(defun mumamo-chunk-embperl-<$ (pos min max)
  "Find [$ ... $], return range and perl-mode."
  ;; This is a bit tricky since [$var] etc must be avoided.
  (let* ((begin-mark "[$")
         (end-mark "$]")
         (good-chars '(32 ;space
                       10 ;line feed
                       9  ;tab
                       ))
         (search-bw-exc-start (lambda (pos min)
                                (let ((not-found t)
                                      (next-char nil)
                                      (exc-start (mumamo-chunk-start-bw-str pos min begin-mark))
                                      (here (point)))
                                  (while (and not-found
                                              exc-start)
                                    (setq next-char (char-after (+ (point) 2)))
                                    (if (memq next-char good-chars)
                                        (setq not-found nil)
                                      (setq exc-start (search-backward begin-mark min t))))
                                  (when (and exc-start
                                             (<= exc-start pos))
                                    (cons exc-start 'perl-mode)))))
         (search-bw-exc-end (lambda (pos min)
                              (mumamo-chunk-end-bw-str pos min end-mark)))
         (search-fw-exc-start (lambda (pos max)
                                (let ((not-found t)
                                      (next-char nil)
                                      (exc-start (mumamo-chunk-start-fw-str pos max begin-mark))
                                      (here (point)))
                                  (while (and not-found
                                              exc-start)
                                    (setq next-char (char-after))
                                    (if (memq next-char good-chars)
                                        (setq not-found nil)
                                      (setq exc-start (search-forward begin-mark max t))))
                                  exc-start)))
         (search-fw-exc-end (lambda (pos max)
                              (mumamo-chunk-end-fw-str pos max end-mark)))
         )
    (mumamo-find-possible-chunk pos min max
                                search-bw-exc-start
                                search-bw-exc-end
                                search-fw-exc-start
                                search-fw-exc-end)))

;;;###autoload
(define-mumamo-multi-major-mode embperl-html-mumamo
  "Turn on multiple major modes for Embperl files with main mode `html-mode'.
This also covers inlined style and javascript."
    ("Embperl HTML Family" html-mode
     (mumamo-chunk-embperl-<-
      mumamo-chunk-embperl-<+
      mumamo-chunk-embperl-<!
      mumamo-chunk-embperl-<$
      mumamo-chunk-inlined-style
      mumamo-chunk-inlined-script
      mumamo-chunk-style=
      mumamo-chunk-onjs=
     )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; django

(defun mumamo-chunk-django4(pos min max)
  "Find {% comment %}.  Return range and 'django-mode."
  (mumamo-find-possible-chunk pos min max
                              'mumamo-search-bw-exc-start-django4
                              'mumamo-search-bw-exc-end-django4
                              'mumamo-search-fw-exc-start-django4
                              'mumamo-search-fw-exc-end-django4))

(defun mumamo-chunk-django3(pos min max)
  "Find {# ... #}.  Return range and 'django-mode."
  (mumamo-find-possible-chunk pos min max
                              'mumamo-search-bw-exc-start-django3
                              'mumamo-search-bw-exc-end-django3
                              'mumamo-search-fw-exc-start-django3
                              'mumamo-search-fw-exc-end-django3))

(defun mumamo-chunk-django2(pos min max)
  "Find {{ ... }}.  Return range and 'django-mode."
  (mumamo-find-possible-chunk pos min max
                              'mumamo-search-bw-exc-start-django2
                              'mumamo-search-bw-exc-end-django2
                              'mumamo-search-fw-exc-start-django2
                              'mumamo-search-fw-exc-end-django2))

(defun mumamo-chunk-django (pos min max)
  "Find {% ... %}.  Return range and 'django-mode."
  (mumamo-find-possible-chunk pos min max
                              'mumamo-search-bw-exc-start-django
                              'mumamo-search-bw-exc-end-django
                              'mumamo-search-fw-exc-start-django
                              'mumamo-search-fw-exc-end-django))

(defun mumamo-search-bw-exc-start-django (pos min)
  (let ((exc-start (mumamo-chunk-start-bw-str-inc pos min "{%")))
    (and exc-start
         (<= exc-start pos)
         (cons exc-start 'django-mode))))

(defun mumamo-search-bw-exc-start-django2(pos min)
  (let ((exc-start (mumamo-chunk-start-bw-str-inc pos min "{{")))
    (and exc-start
         (<= exc-start pos)
         (cons exc-start 'django-mode))))

(defun mumamo-search-bw-exc-start-django3(pos min)
  (let ((exc-start (mumamo-chunk-start-bw-str-inc pos min "{#")))
    (and exc-start
         (<= exc-start pos)
         (cons exc-start 'django-mode))))

(defun mumamo-search-bw-exc-start-django4(pos min)
  (let ((exc-start (mumamo-chunk-start-bw-str-inc pos min
                                                       "{% comment %}")))
    (and exc-start
         (<= exc-start pos)
         (cons exc-start 'django-mode))))

(defun mumamo-search-bw-exc-end-django (pos min)
  (mumamo-chunk-end-bw-str-inc pos min "%}"))

(defun mumamo-search-bw-exc-end-django2(pos min)
  (mumamo-chunk-end-bw-str-inc pos min "}}"))

(defun mumamo-search-bw-exc-end-django3(pos min)
  (mumamo-chunk-end-bw-str-inc pos min "#}"))

(defun mumamo-search-bw-exc-end-django4(pos min)
  (mumamo-chunk-end-bw-str-inc pos min "{% endcomment %}"))

(defun mumamo-search-fw-exc-start-django (pos max)
  (mumamo-chunk-start-fw-str-inc pos max "{%"))

(defun mumamo-search-fw-exc-start-django2(pos max)
  (mumamo-chunk-start-fw-str-inc pos max "{{"))

(defun mumamo-search-fw-exc-start-django3(pos max)
  (mumamo-chunk-start-fw-str-inc pos max "{#"))

(defun mumamo-search-fw-exc-start-django4(pos max)
  (mumamo-chunk-start-fw-str-inc pos max "{% comment %}"))

(defun mumamo-search-fw-exc-end-django (pos max)
  (mumamo-chunk-end-fw-str-inc pos max "%}"))

(defun mumamo-search-fw-exc-end-django2(pos max)
  (mumamo-chunk-end-fw-str-inc pos max "}}"))

(defun mumamo-search-fw-exc-end-django3(pos max)
  (mumamo-chunk-end-fw-str-inc pos max "#}"))

(defun mumamo-search-fw-exc-end-django4(pos max)
  (mumamo-chunk-end-fw-str-inc pos max "{% endcomment %}"))

;;;###autoload
(define-mumamo-multi-major-mode django-html-mumamo
  "Turn on multiple major modes for Django with main mode `html-mode'.
This also covers inlined style and javascript."
  ("Django HTML Family" html-mode
   (mumamo-chunk-django4
    mumamo-chunk-django
    mumamo-chunk-django2
    mumamo-chunk-django3
    mumamo-chunk-inlined-style
    mumamo-chunk-inlined-script
    mumamo-chunk-style=
    mumamo-chunk-onjs=
    )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; smarty

(defun mumamo-chunk-smarty (pos min max)
  "Find { ... }.  Return range and 'smarty-mode."
  (mumamo-find-possible-chunk pos min max
                              'mumamo-search-bw-exc-start-smarty
                              'mumamo-search-bw-exc-end-smarty
                              'mumamo-search-fw-exc-start-smarty
                              'mumamo-search-fw-exc-end-smarty))

(defun mumamo-search-bw-exc-start-smarty (pos min)
  (let ((exc-start (mumamo-chunk-start-bw-str-inc pos min "{")))
    (when (and exc-start
               (<= exc-start pos))
      (cons exc-start 'smarty-mode))))

(defun mumamo-search-bw-exc-end-smarty (pos min)
  (mumamo-chunk-end-bw-str-inc pos min "}"))

(defun mumamo-search-fw-exc-start-smarty (pos max)
  (let ((end-out (mumamo-chunk-start-fw-str-inc pos max "{")))
    end-out))

(defun mumamo-search-fw-exc-end-smarty (pos max)
  (mumamo-chunk-end-fw-str-inc pos max "}"))

;;;###autoload
(define-mumamo-multi-major-mode smarty-html-mumamo
  "Turn on multiple major modes for Smarty with main mode `html-mode'.
This also covers inlined style and javascript."
  ("Smarty HTML Family" html-mode
   (mumamo-chunk-xml-pi
    mumamo-chunk-smarty
    mumamo-chunk-style=
    mumamo-chunk-onjs=
    )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; jsp

(defun mumamo-chunk-jsp (pos min max)
  "Find <% ... %>.  Return range and 'java-mode."
  (mumamo-find-possible-chunk pos min max
                              'mumamo-search-bw-exc-start-jsp
                              'mumamo-search-bw-exc-end-jsp
                              'mumamo-search-fw-exc-start-jsp
                              'mumamo-search-fw-exc-end-jsp))

(defun mumamo-search-bw-exc-start-jsp (pos min)
  (let ((exc-start (mumamo-chunk-start-bw-str pos min "<%")))
    (when (and exc-start
               (<= exc-start pos))
      (cons exc-start 'java-mode))))

(defun mumamo-search-bw-exc-end-jsp (pos min)
  (mumamo-chunk-end-bw-str pos min "%>"))

(defun mumamo-search-fw-exc-start-jsp (pos max)
  (let ((end-out (mumamo-chunk-start-fw-str pos max "<%")))
    end-out))

(defun mumamo-search-fw-exc-end-jsp (pos max)
  (mumamo-chunk-end-fw-str pos max "%>"))

;;;###autoload
(define-mumamo-multi-major-mode jsp-html-mumamo
  "Turn on multiple major modes for JSP with main mode `html-mode'.
This also covers inlined style and javascript."
    ("JSP HTML Family" html-mode
     (mumamo-chunk-jsp
      mumamo-chunk-inlined-style
      mumamo-chunk-inlined-script
      mumamo-chunk-style=
      mumamo-chunk-onjs=
      )))

;;;###autoload
(define-mumamo-multi-major-mode asp-html-mumamo
  "Turn on multiple major modes for ASP with main mode `html-mode'.
This also covers inlined style and javascript."
  ("ASP Html Family" html-mode
   (mumamo-chunk-asp
    mumamo-asp-chunk-inlined-script
    mumamo-chunk-inlined-script
    mumamo-chunk-style=
    mumamo-chunk-onjs=
    )))

;;;###autoload
(define-mumamo-multi-major-mode sgml-mumamo
  "Turn on multiple major modes for ASP with main mode `sgml-mode'.
This also covers inlined style and javascript."
  ("SGML Family" sgml-mode
   (mumamo-chunk-xml-pi
    mumamo-chunk-inlined-style
    mumamo-chunk-inlined-script
    mumamo-chunk-style=
    mumamo-chunk-onjs=
    )))

;;;###autoload
(define-mumamo-multi-major-mode metapost-mumamo
  "Turn on multiple major modes for MetaPost."
  ("MetaPost TeX Family" metapost-mode
   (mumamo-chunk-textext
    mumamo-chunk-verbatimtex
    )))

;;;###autoload
(define-mumamo-multi-major-mode perl-mumamo
  "Turn on multiple major modes for Perl Here Document."
  ("Perl Here Doc" perl-mode
   (mumamo-chunk-perl-here-html
    )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; eruby

(defun mumamo-chunk-eruby (pos min max)
  "Find <% ... %>.  Return range and 'ruby-mode."
  (mumamo-find-possible-chunk pos min max
                              'mumamo-search-bw-exc-start-ruby
                              'mumamo-search-bw-exc-end-jsp
                              'mumamo-search-fw-exc-start-jsp
                              'mumamo-search-fw-exc-end-jsp))

(defun mumamo-search-bw-exc-start-ruby (pos min)
  (let ((exc-start (mumamo-chunk-start-bw-str pos min "<%")))
    (when (and exc-start
               (<= exc-start pos))
      (cons exc-start 'ruby-mode))))

;;;###autoload
(define-mumamo-multi-major-mode eruby-mumamo
  "Turn on multiple major mode for a general eRuby buffer."
  ("eRuby Family" nil
   (mumamo-chunk-eruby
    )))

;;;###autoload
(define-mumamo-multi-major-mode eruby-html-mumamo
  "Turn on multiple major modes for eRuby with main mode `html-mode'.
This also covers inlined style and javascript."
  ("eRuby Html Family" html-mode
   (mumamo-chunk-eruby
    mumamo-chunk-inlined-style
    mumamo-chunk-inlined-script
    mumamo-chunk-style=
    mumamo-chunk-onjs=
    )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; OpenLaszlo

(defconst mumamo-lzx-method-tag-start-regex
  (rx "<method"
      (optional
       space
       (0+ (not (any ">"))))
      ">"
      ;; FIX-ME: Commented out because of bug in Emacs
      ;;
      ;;(optional (0+ space) "<![CDATA[" )
      ))

(defun mumamo-search-bw-exc-start-inlined-lzx-method (pos min)
  (goto-char (+ pos 7))
  (let ((marker-start (search-backward "<method" min t))
        exc-mode
        exc-start)
    (when marker-start
      (when (looking-at mumamo-lzx-method-tag-start-regex)
        (setq exc-start (match-end 0))
        (goto-char exc-start)
        (when (<= exc-start pos)
          (cons (point) 'javascript-mode))
        ))))

(defun mumamo-search-bw-exc-end-inlined-lzx-method (pos min)
  (mumamo-chunk-end-bw-str pos min "</method>"))

(defun mumamo-search-fw-exc-start-inlined-lzx-method (pos max)
  (goto-char (1+ pos))
  (skip-chars-backward "^<")
  ;; Handle <![CDATA[
  (when (and
         (eq ?< (char-before))
         (eq ?! (char-after))
         (not (bobp)))
    (backward-char)
    (skip-chars-backward "^<"))
  (unless (bobp)
    (backward-char 1))
  (let ((exc-start (search-forward "<method" max t))
        exc-mode)
    (when exc-start
      (goto-char (- exc-start 7))
      (when (looking-at mumamo-lzx-method-tag-start-regex)
        (goto-char (match-end 0))
        (point)
        ))))

(defun mumamo-search-fw-exc-end-inlined-lzx-method (pos max)
  (mumamo-chunk-end-fw-str pos max "</method>"))

(defun mumamo-chunk-inlined-lzx-method (pos min max)
  "Find <method>...</method>.  Return range and 'javascript-mode."
  (mumamo-find-possible-chunk pos min max
                              'mumamo-search-bw-exc-start-inlined-lzx-method
                              'mumamo-search-bw-exc-end-inlined-lzx-method
                              'mumamo-search-fw-exc-start-inlined-lzx-method
                              'mumamo-search-fw-exc-end-inlined-lzx-method))

(defconst mumamo-lzx-handler-tag-start-regex
  (rx "<handler"
      (optional
       space
       (0+ (not (any ">"))))
      ">"
      ;; FIX-ME: Commented out because of bug in Emacs
      ;;
      ;;(optional (0+ space) "<![CDATA[" )
      ))

(defun mumamo-search-bw-exc-start-inlined-lzx-handler (pos min)
  (goto-char (+ pos 8))
  (let ((marker-start (search-backward "<handler" min t))
        exc-mode
        exc-start)
    (when marker-start
      (when (looking-at mumamo-lzx-handler-tag-start-regex)
        (setq exc-start (match-end 0))
        (goto-char exc-start)
        (when (<= exc-start pos)
          (cons (point) 'javascript-mode))
        ))))

(defun mumamo-search-bw-exc-end-inlined-lzx-handler (pos min)
  (mumamo-chunk-end-bw-str pos min "</handler>"))

(defun mumamo-search-fw-exc-start-inlined-lzx-handler (pos max)
  (goto-char (1+ pos))
  (skip-chars-backward "^<")
  ;; Handle <![CDATA[
  (when (and
         (eq ?< (char-before))
         (eq ?! (char-after))
         (not (bobp)))
    (backward-char)
    (skip-chars-backward "^<"))
  (unless (bobp)
    (backward-char 1))
  (let ((exc-start (search-forward "<handler" max t))
        exc-mode)
    (when exc-start
      (goto-char (- exc-start 8))
      (when (looking-at mumamo-lzx-handler-tag-start-regex)
        (goto-char (match-end 0))
        (point)
        ))))

(defun mumamo-search-fw-exc-end-inlined-lzx-handler (pos max)
  (mumamo-chunk-end-fw-str pos max "</handler>"))

(defun mumamo-chunk-inlined-lzx-handler (pos min max)
  "Find <handler>...</handler>.  Return range and 'javascript-mode."
  (mumamo-find-possible-chunk pos min max
                              'mumamo-search-bw-exc-start-inlined-lzx-handler
                              'mumamo-search-bw-exc-end-inlined-lzx-handler
                              'mumamo-search-fw-exc-start-inlined-lzx-handler
                              'mumamo-search-fw-exc-end-inlined-lzx-handler))


;;;###autoload
(define-mumamo-multi-major-mode laszlo-nxml-mumamo
  "Turn on multiple major modes for OpenLaszlo."
  ("OpenLaszlo Family" nxml-mode
   (mumamo-chunk-inlined-script
    mumamo-chunk-inlined-lzx-method
    mumamo-chunk-inlined-lzx-handler
    )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; csound

(defun mumamo-search-bw-exc-start-csound-orc (pos min)
  (let ((exc-start (mumamo-chunk-start-bw-str pos min "<csinstruments>")))
    (and exc-start
         (<= exc-start pos)
         (cons exc-start 'csound-orc-mode))))

(defun mumamo-search-bw-exc-end-csound-orc (pos min)
  (mumamo-chunk-end-bw-str pos min "</csinstruments>"))

(defun mumamo-search-fw-exc-start-csound-orc (pos max)
  (mumamo-chunk-start-fw-str pos max "<csinstruments>"))

(defun mumamo-search-fw-exc-end-csound-orc (pos max)
  (mumamo-chunk-end-fw-str pos max "</csinstruments>"))

(defun mumamo-chunk-csound-orc (pos min max)
  "Find <csinstruments>...</csinstruments>.  Return range and 'csound-orc-mode."
  (mumamo-find-possible-chunk pos min max
                              'mumamo-search-bw-exc-start-csound-orc
                              'mumamo-search-bw-exc-end-csound-orc
                              'mumamo-search-fw-exc-start-csound-orc
                              'mumamo-search-fw-exc-end-csound-orc))

(defun mumamo-search-bw-exc-start-csound-sco (pos min)
  (let ((exc-start (mumamo-chunk-start-bw-str pos min "<csscore>")))
    (and exc-start
         (<= exc-start pos)
         (cons exc-start 'csound-sco-mode))))

(defun mumamo-search-bw-exc-end-csound-sco (pos min)
  (mumamo-chunk-end-bw-str pos min "</csscore>"))

(defun mumamo-search-fw-exc-start-csound-sco (pos max)
  (mumamo-chunk-start-fw-str pos max "<csscore>"))

(defun mumamo-search-fw-exc-end-csound-sco (pos max)
  (mumamo-chunk-end-fw-str pos max "</csscore>"))

(defun mumamo-chunk-csound-sco (pos min max)
  "Found <csscore>...</csscore>.  Return range and 'csound-sco-mode."
  (mumamo-find-possible-chunk pos min max
                              'mumamo-search-bw-exc-start-csound-sco
                              'mumamo-search-bw-exc-end-csound-sco
                              'mumamo-search-fw-exc-start-csound-sco
                              'mumamo-search-fw-exc-end-csound-sco))

;;;###autoload
(define-mumamo-multi-major-mode csound-sgml-mumamo
  "Turn on mutiple major modes for CSound orc/sco Modes."
  ("CSound orc/sco Modes" sgml-mode
   (mumamo-chunk-csound-sco
    mumamo-chunk-csound-orc
    )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; noweb

(defcustom mumamo-noweb2-mode-from-ext
  '(
    ("php" . php-mode)
    ("c" . c-mode)
    )
  "File extension regexp to major mode mapping."
  :type '(repeat
          (cons regexp major-mode-function))
  :group 'mumamo)

(defvar mumamo-noweb2-found-mode-from-ext nil
  "Major modes determined from file names. Internal use.")

(defun mumamo-noweb2-chunk-start-fw (pos max)
  (mumamo-chunk-start-fw-re pos max "^<<\\(.*?\\)>>="))

(defun mumamo-noweb2-chunk-start-bw (pos min)
  (let ((exc-start (mumamo-chunk-start-bw-re pos min "^<<\\(.*?\\)>>="))
        (exc-mode 'text-mode))
    (when exc-start
      (let* ((file-name (match-string 1))
             (file-ext  (when file-name (file-name-extension file-name))))
        (when file-ext
          (setq exc-mode (catch 'major
                           (dolist (rec mumamo-noweb2-mode-from-ext)
                             (when (string-match (car rec) file-ext)
                               (throw 'major (cdr rec))))
                           nil))
          (unless exc-mode
            (setq exc-mode
                  (cdr (assoc file-ext mumamo-noweb2-found-mode-from-ext)))
            (unless exc-mode
              ;; Get the major mode from file name
              (with-temp-buffer
                (setq buffer-file-name file-name)
                (condition-case err
                    (normal-mode)
                  (error (message "error (normal-mode): %s"
                                  (error-message-string err))))
                (setq exc-mode (or major-mode
                                   'text-mode))
                (add-to-list 'mumamo-noweb2-found-mode-from-ext
                             (cons file-ext exc-mode)))
              ))))
      (cons exc-start exc-mode))))

(defun mumamo-noweb2-chunk-end-fw (pos max)
  (mumamo-chunk-end-fw-re pos max "^@"))

(defun mumamo-noweb2-chunk-end-bw (pos min)
  (mumamo-chunk-end-bw-re pos min "^@"))

(defun mumamo-noweb2-code-chunk (pos min max)
  (save-match-data
    (mumamo-find-possible-chunk pos min max
                                'mumamo-noweb2-chunk-start-bw
                                'mumamo-noweb2-chunk-end-bw
                                'mumamo-noweb2-chunk-start-fw
                                'mumamo-noweb2-chunk-end-fw)))


(define-mumamo-multi-major-mode noweb2-mumamo
  "test noweb"
  ("noweb Family" latex-mode
   (mumamo-noweb2-code-chunk)))

(provide 'mumamo-fun)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mumamo-fun.el ends here
