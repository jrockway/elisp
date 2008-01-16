
(defun insert-self-shift (noshift)
  (interactive)
  (if noshift
      (insert "my ($self) = @_;\n")
    (insert "my $self = shift;\n"))
  (cperl-indent-command))

(defun cpan-install ()
  (interactive)
  (let ((module (read-with-default "Module" (thing-at-point 'perl-module)
                                   "You must specify a module to install!")))

    (split-window)
    (ansi-term "/home/jon/perl/install/bin/cpanp" "
cpan-install")
    (term-send-raw-string (format "install %s\n" module))))

(global-set-key "\C-c\C-i" 'cpan-install)

(provide 'cperl-misc)
