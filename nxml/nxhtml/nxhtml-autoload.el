;; nxhtml-autoload.el -- Autoloading of nxthml-mode

;; Copyright (C) 2005, 2006, 2007 by Lennart Borgman

;; Author: Lennart Borgman <lennartDOTborgmanDOT073ATstudentDOTluDOTse>
;; Created: Sat Feb 11 00:06:14 2006
;; Version: 0.51
;; Last-Updated: 2008-02-13T01:21:14+0100 Wed
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
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'ourcomments-util)

(unless (featurep 'nxml-enc)
  ;; This is for the case when nXml is included in Emacs
  (require 'nxml-mode))

(if (not (or (featurep 'nxml-enc) ;; nXml not in Emacs
             (featurep 'nxml-mode))) ;; nXml in Emacs
    (progn
      (lwarn
       '(nxhtml-autoload)
       :emergency
       (concat
        "\n\n\nERROR: nxml not loaded!\n\n"
        "    Please load nxml before loading nxhtml!\n"
        "    Load nxml by loading rng-auto.el in the nxml distribution.\n\n\n\n"))
      (sit-for 10))

  (add-to-list 'load-path
               (file-name-directory
                (if load-file-name load-file-name buffer-file-name)))

  (autoload 'nxhtml-report-bug "nxhtml-bug" "Report a bug in nXhtml." t)
  (autoload 'nxhtml-mode "nxhtml" "Major mode for editing XHTML documents." t)
  (autoload 'nxhtml-global-minor-mode "nxhtml-menu" "Toggle `nxhtml-minor-mode' in every buffer." t)
  (autoload 'gimp-edit-buffer "gimp" "Edit image file in current buffer with GIMP." t)
  (autoload 'gimp-edit-file "gimp" "Edit file with GIMP." t)

  (require 'fmode)
  (fmode-replace-default-mode 'html-mode 'nxhtml-mode)
  (fmode-replace-default-mode 'xml-mode 'nxml-mode)


  (require 'html-site)
  (require 'nxhtml-menu)

  ;;; Change below if you need to:
  (autoload 'css-mode "css-mode" "Mode for editing css files" t)
  (autoload 'javascript-mode "javascript" "Mode for JavaScript" t)
  (autoload 'php-mode "php-mode" "Mode for editing php files" t)
  (autoload 'smarty-mode "smarty-mode" "Mode for editing php smarty files" t)
  (autoload 'csharp-mode "csharp-mode" "Mode for editing C# code" t)
  (add-hook 'nxml-mode-hook
            (lambda ()
              (define-key nxml-mode-map [M-left]  'nxml-backward-element)
              (define-key nxml-mode-map [M-right] 'nxml-forward-element)
              (define-key nxml-mode-map [M-up]    'nxml-backward-up-element)
              (define-key nxml-mode-map [M-down]  'nxml-down-element)))

  (require 'mumamo-fun)
  (require 'nxhtml-mumamo)
  (require 'as-external)
  )

(defcustom nxhtml-auto-mode-alist
  '(
    ("\\.x?html?\\'"  . nxhtml-mumamo)
    ("\\.x?htmlf?\\'" . nxhtml-mumamo)
    ("\\.php\\'"      . nxhtml-mumamo)
    ("\\.phtml\\'"    . nxhtml-mumamo)
    ("\\.jsp\\'"      . jsp-nxhtml-mumamo)
    ("\\.asp\\'"      . asp-nxhtml-mumamo)
    ("\\.djhtml\\'"   . django-nxhtml-mumamo)
    ("\\.rhtml\\'"    . eruby-nxhtml-mumamo)
    ("\\.phps\\'"     . smarty-nxhtml-mumamo)
    ("\\.epl\\'"      . embperl-nxhtml-mumamo)
    ("\.lzx\\'"       . laszlo-nxml-mumamo)

    ("\\.js\\'"       . javascript-mode)
    ("\\.css\\'"      . css-mode)
    )
  "List to add to `auto-mode-alist'.
This list is added to `auto-mode-alist' when loading
nxhtml-autostart.el and will therefore help Emacs to determine
which major mode a file will be opened in.

Please notice that `mumamo-mode' may override this choice of
major mode when setting the chunk family.  The chunk family then
determines the major mode.  The chunk family is set from
`mumamo-filenames-list'.  You may want to synch the two list, but
it is not necessary.  However not synching may perhaps lead to
surpricing results.  To synch the lists means that the mode in
this list should correspond to the main major mode in the mumamo
chunk family.

* Note: This variable (nxhtml-auto-mode-alist) is just for your
  convenience.  Probably most users normally just adds to
  `auto-mode-alist' in their .emacs with lines like

    \(add-to-list 'auto-mode-alist '(\"\\.x?html?\\'\"  . nxhtml-mode))

  but doing something like that here would make it impossible to
  customize that easily for you."
  ;;:type '(alist :key-type regexp :tag "hej" :value-type major-mode-function)
  :type '(repeat (cons :tag "Enter file name pattern and major mode"
                       (regexp :tag "Regexp for file name")
                       (major-mode-function :tag "Major mode")))
  :set (lambda (sym val)
         (set-default sym val)
         (dolist (v val)
           (add-to-list 'auto-mode-alist v)))
  :group 'nxhtml)

(provide `nxhtml-autoload)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nxhtml-autoload.el ends here
