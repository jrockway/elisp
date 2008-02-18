use ExtUtils::MakeMaker qw(WriteMakefile prompt);
use 5.006;                      # for "no warnings" -- sorry!
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.

print <<EOS;
NOTE:
    To actually use this package in a useful way, you probably need to
    move the Emacs Lisp files somewhere.  Where will depend on your
    installation.

    You will also need to install the HTML or Texinfo documentation
    somewhere appropriate to your system.
EOS

my %prereq = (
    'Data::Dumper'    => 0,
    'Scalar::Util'    => 0,
);

## Poor man's optional deps.
sub test_for
{
    my $mod = shift;
    eval "require $mod";
    if ($@) {
        my $resp = prompt("@_.  Install $mod [yN]? ", 'n');
        $prereq{$mod} = 0 if $resp =~ /^y/i;
    }
}

test_for 'PadWalker', 'Stack/lexical inspection requires PadWalker >= 1.0';
test_for 'Lexical::Persistence', 'Strict mode requires Lexical::Persistence';
test_for 'Module::CoreList', 'sepia-core-version requires Module::CoreList';
test_for 'Devel::Size', 'Printing variable sizes requires Devel::Size';
test_for 'Module::Info', 'Module::Info required for some Emacs functions';

WriteMakefile(
    'NAME'		=> 'Sepia',
    'VERSION_FROM'	=> 'lib/Sepia.pm', # finds $VERSION
    'PREREQ_PM'         => \%prereq,
    AUTHOR     => "Sean O'Rourke <seano\@cpan.org>",
    ABSTRACT => 'Simple Emacs-Perl InterAction',
);
