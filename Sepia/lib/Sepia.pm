package Sepia;

=head1 NAME

Sepia - Simple Emacs-Perl Interface

=head1 SYNOPSIS

From inside Emacs:

   M-x load-library RET sepia RET
   M-x sepia-repl RET

At the prompt in the C<*sepia-repl*> buffer:

   main @> ,help

For more information, please see F<Sepia.html> or F<sepia.info>, which
come with the distribution.

=cut

$VERSION = '0.96_01';
use strict;
use B;
use Sepia::Debug;               # THIS TURNS ON DEBUGGING INFORMATION!
use Cwd 'abs_path';
use Scalar::Util 'looks_like_number';
use Text::Abbrev;

use vars qw($PS1 %REPL %RK %REPL_DOC %REPL_SHORT %PRINTER
            @REPL_RESULT @res
            $REPL_LEVEL $PACKAGE $WANTARRAY $PRINTER $STRICT $PRINT_PRETTY
            $ISEVAL);

sub repl_strict
{
    eval { require Lexical::Persistence; import Lexical::Persistence };
    if ($@) {
        print "Strict mode requires Lexical::Persistence.\n";
    } else {
        *repl_strict = sub {
            my $x = as_boolean(shift, $STRICT);
            if ($x && !$STRICT) {
                $STRICT = new Lexical::Persistence;
            } elsif (!$x) {
                undef $STRICT;
            }
        };
        goto &repl_strict;
    }
}

sub core_version
{
    eval { require Module::CoreList };
    if ($@) {
        '???';
    } else {
        *core_version = sub { Module::CoreList->first_release(@_) };
        goto &core_version;
    }
}

BEGIN {
    eval { use List::Util 'max' };
    if ($@) {
        *Sepia::max = sub {
            my $ret = shift;
            for (@_) {
                $ret = $_ if $_ > $ret;
            }
            $ret;
        };
    }
}

sub repl_size
{
    eval { require Devel::Size };
    if ($@) {
        print "Size requires Devel::Size.\n";
    } else {
        *Sepia::repl_size = sub {
            ## XXX: C&P from repl_who:
            my ($pkg, $re) = split ' ', shift || '';
            if ($pkg =~ /^\/(.*)\/?$/) {
                $pkg = $PACKAGE;
                $re = $1;
            } elsif (!$re && !defined %{$pkg.'::'}) {
                $re = $pkg;
                $pkg = $PACKAGE;
            } else {
                $re = '';
                $pkg = $PACKAGE;
            }
            my @who = who($pkg, $re);
            my $len = max(map { length } @who) + 4;
            my $fmt = '%-'.$len."s%10d\n";
            print 'Var', ' ' x ($len + 2), "Bytes\n";
            print '-' x ($len-4), ' ' x 9, '-' x 5, "\n";
            local $SIG{__WARN__} = sub {};
            for (@who) {
                next unless /^[\$\@\%\&]/; # skip subs.
                # print STDERR "package $pkg; Devel::Size::total_size \\$_;";
                my $res = eval "package $pkg; Devel::Size::total_size \\$_;";
                # next if $res == 0;
                printf $fmt, $_, $res;
            }
        };
        goto &repl_size;
    }
}

=head1 DESCRIPTION

Sepia is a set of features to make Emacs a better tool for Perl
development.  This package contains the Perl side of the
implementation, including all user-serviceable parts (for the
cross-referencing facility see L<Sepia::Xref>).  This document is
aimed as Sepia developers; for user documentation, see
L<sepia/index.html>.

Though not intended to be used independent of the Emacs interface, the
Sepia module's functionality can be used through a rough procedural
interface.

=head2 C<@compls = completions($string [, $type])>

Find a list of completions for C<$string> with glob type C<$type>,
which may be "SCALAR", "HASH", "ARRAY", "CODE", "IO", or the special
value "VARIABLE", which means either scalar, hash, or array.
Completion operates on word subparts separated by [:_], so
e.g. "S:m_w" completes to "Sepia::my_walksymtable".

=head2 C<@compls = method_completions($expr, $string [,$eval])>

Complete among methods on the object returned by C<$expr>.  The
C<$eval> argument, if present, is a function used to do the
evaluation; the default is C<eval>, but for example the Sepia REPL
uses C<Sepia::repl_eval>.  B<Warning>: Since it has to evaluate
C<$expr>, method completion can be extremely problematic.  Use with
care.

=cut

sub _apropos_re($)
{
    # Do that crazy multi-word identifier completion thing:
    my $re = shift;
    return qr/.*/ if $re eq '';
    if (wantarray) {
        map {
            s/(?:^|(?<=[A-Za-z\d]))(([^A-Za-z\d])\2*)/[A-Za-z\\d]*$2+/g;
            qr/^$_/
        } split /:+/, $re, -1;
    } else {
        if ($re !~ /[^\w\d_^:]/) {
            $re =~ s/(?<=[A-Za-z\d])(([^A-Za-z\d])\2*)/[A-Za-z\\d]*$2+/g;
        }
        qr/$re/;
    }
}

my %sigil;
BEGIN {
    %sigil = qw(ARRAY @ SCALAR $ HASH %);
}

sub filter_untyped
{
    no strict;
    local $_ = /^::/ ? $_ : "::$_";
    defined *{$_}{CODE} || defined *{$_}{IO} || (/::$/ && defined *{$_}{HASH});
}

## XXX: Careful about autovivification here!  Specifically:
##     defined *FOO{HASH} # => ''
##     defined %FOO       # => ''
##     defined *FOO{HASH} # => 1
sub filter_typed
{
    no strict;
    my $type = shift;
    local $_ = /^::/ ? $_ : "::$_";
    if ($type eq 'SCALAR') {
        defined ${$_};
    } elsif ($type eq 'VARIABLE') {
        defined ${$_} || defined *{$_}{HASH} || defined *{$_}{ARRAY};
    } else {
        defined *{$_}{$type}
    }
}

sub maybe_icase
{
    my $ch = shift;
    $ch =~ /[A-Z]/ ? $ch : '['.uc($ch).$ch.']';
}

sub all_abbrev_completions
{
    use vars '&_completions';
    local *_completions = sub {
        no strict;
        my ($stash, @e) = @_;
        my $ch = '[A-Za-z0-9]*';
        my $re1 = "^".maybe_icase($e[0]).$ch.join('', map {
            '_'.maybe_icase($_).$ch
        } @e[1..$#e]);
        $re1 = qr/$re1/;
        my $re2 = maybe_icase $e[0];
        $re2 = qr/^$re2.*::$/;
        my @ret = grep !/::$/ && /$re1/, keys %{$stash};
        my @pkgs = grep /$re2/, keys %{$stash};
        (map("$stash$_", @ret),
         @e > 1 ? map { _completions "$stash$_", @e[1..$#e] } @pkgs :
             map { "$stash$_" } @pkgs)
    };
    map { s/^:://; $_ } _completions('::', split //, shift);
}

sub apropos_re
{
    my ($icase, $re) = @_;
    $re =~ s/_/[^_]*_/g;
    $icase ? qr/^$re.*$/i : qr/^$re.*$/;
}

sub all_completions
{
    my $icase = $_[0] !~ /[A-Z]/;
    my @parts = split /:+/, shift, -1;
    my $re = apropos_re $icase, pop @parts;
    use vars '&_completions';
    local *_completions = sub {
        no strict;
        my $stash = shift;
        if (@_ == 0) {
            map { "$stash$_" } grep /$re/, keys %{$stash};
        } else {
            my $re2 = $icase ? qr/^$_[0].*::$/i : qr/^$_[0].*::$/;
            my @pkgs = grep /$re2/, keys %{$stash};
            map { _completions "$stash$_", @_[1..$#_] } @pkgs
        }
    };
    map { s/^:://; $_ } _completions('::', @parts);
}

sub completions
{
    my ($type, $str, $t);
    my %h = qw(@ ARRAY % HASH & CODE * IO $ SCALAR);
    my %rh;
    @rh{values %h} = keys %h;
    if (@_ == 1) {
        ($type, $str) = $_[0] =~ /^([\%\$\@\&]?)(.*)/;
        $t = $type || '';
    $type = $h{$type} if $type;
    } else {
        ($str, $type) = @_;
        $type ||= '';
        $t = $rh{$type} if $type;
    }
    my @ret = grep {
        $type ? filter_typed $type : filter_untyped
    } all_completions $str;
    if (!@ret && $str !~ /:/) {
        @ret = grep {
            $type ? filter_typed $type : filter_untyped
        } all_abbrev_completions $str;
    }
    @ret = map { s/^:://; "$t$_" } @ret;
#     ## XXX: Control characters, $", and $1, etc. confuse Emacs, so
#     ## remove them.
    grep {
        length > 0 && !looks_like_number $_ && !/^[^\w\d_]$/ && !/^_</ && !/^[[:cntrl:]]/
    } @ret;
}

sub method_completions
{
    my ($x, $fn, $eval) = @_;
    $x =~ s/^\s+//;
    $x =~ s/\s+$//;
    $eval ||= 'CORE::eval';
    no strict;
    return unless ($x =~ /^\$/ && ($x = $eval->("ref($x)")))
        || $eval->('defined(%{'.$x.'::})');
    unless ($@) {
        my $re = _apropos_re $fn;
        ## Filter out overload methods "(..."
        return sort { $a cmp $b } map { s/.*:://; $_ }
            grep { defined *{$_}{CODE} && /::$re/ && !/\(/ }
                methods($x, 1);
    }
}

=head2 C<@locs = location(@names)>

Return a list of [file, line, name] triples, one for each function
name in C<@names>.

=cut

sub location
{
    no strict;
    my @x= map {
        my $str = $_;
        if (my ($pfx, $name) = $str =~ /^([\%\$\@]?)(.+)/) {
            if ($pfx) {
                warn "Sorry -- can't lookup variables.";
                [];
            } else {
                # XXX: svref_2object only seems to work with a package
                # tacked on, but that should probably be done
                # elsewhere...
                $name = 'main::'.$name unless $name =~ /::/;
                my $cv = B::svref_2object(\&{$name});
                if ($cv && defined($cv = $cv->START) && !$cv->isa('B::NULL')) {
                    my ($file, $line) = ($cv->file, $cv->line);
                    if ($file !~ /^\//) {
                        for (@INC) {
                            if (-f "$_/$file") {
                                $file = "$_/$file";
                                last;
                            }
                        }
                    }
                    my ($shortname) = $name =~ /^(?:.*::)([^:]+)$/;
                    [Cwd::abs_path($file), $line, $shortname || $name]
                } else {
#                    warn "Bad CV for $name: $cv";
                    [];
                }
            }
        } else {
            []
        }
    } @_;
    return @x;
}

=head2 C<@matches = apropos($name [, $is_regex])>

Search for function C<$name>, either in all packages or, if C<$name>
is qualified, only in one package.  If C<$is_regex> is true, the
non-package part of C<$name> is a regular expression.

=cut

sub my_walksymtable(&*)
{
    no strict;
    my ($f, $st) = @_;
    local *_walk = sub {
        local ($stash) = @_;
        &$f for keys %$stash;
        _walk("$stash$_") for grep /(?<!main)::$/, keys %$stash;
    };
    _walk($st);
}

sub apropos
{
    my ($it, $re, @types) = @_;
    my $stashp;
    if (@types) {
        $stashp = grep /STASH/, @types;
        @types = grep !/STASH/, @types;
    } else {
        @types = qw(CODE);
    }
    no strict;
    if ($it =~ /^(.*::)([^:]+)$/) {
        my ($stash, $name) = ($1, $2);
        if (!defined %$stash) {
            return;
        }
        if ($re) {
            my $name = qr/^$name/;
            map {
                "$stash$_"
            }
            grep {
                my $stashnm = "$stash$_";
                /$name/ &&
                    (($stashp && /::$/)
                     || scalar grep { defined *{$stashnm}{$_} } @types)
            } keys %$stash;
        } else {
            defined &$it ? $it : ();
        }
    } else {
        my @ret;
        my $findre = $re ? qr/$it/ : qr/^\Q$it\E$/;
        my_walksymtable {
            push @ret, "$stash$_" if /$findre/;
        } '::';
        map { s/^:*(?:main:+)*//;$_ } @ret;
    }
}

=head2 C<@names = mod_subs($pack)>

Find subs in package C<$pack>.

=cut

sub mod_subs
{
    no strict;
    my $p = shift;
    my $stash = \%{"$p\::"};
    if (defined $stash) {
        grep { defined &{"$p\::$_"} } keys %$stash;
    }
}

=head2 C<@decls = mod_decls($pack)>

Generate a list of declarations for all subroutines in package
C<$pack>.

=cut

sub mod_decls
{
    my $pack = shift;
    no strict 'refs';
    my @ret = map {
	my $sn = $_;
	my $proto = prototype(\&{"$pack\::$sn"});
	$proto = defined($proto) ? "($proto)" : '';
	"sub $sn $proto;";
    } mod_subs($pack);
    return wantarray ? @ret : join '', @ret;
}

=head2 C<$info = module_info($module, $type)>

Emacs-called function to get module information.

=cut

sub module_info
{
    eval { require Module::Info; import Module::Info };
    if ($@) {
        undef;
    } else {
        *module_info = sub {
            my ($m, $func) = @_;
            my $info;
            if (-f $m) {
                $info = Module::Info->new_from_file($m);
            } else {
                (my $file = $m) =~ s|::|/|g;
                $file .= '.pm';
                if (exists $INC{$file}) {
                    $info = Module::Info->new_from_loaded($m);
                } else {
                    $info = Module::Info->new_from_module($m);
                }
            }
            if ($info) {
                return $info->$func;
            }
        };
        goto &module_info;
    }
}

=head2 C<$file = mod_file($mod)>

Find the likely file owner for module C<$mod>.

=cut

sub mod_file
{
    my $m = shift;
    $m =~ s/::/\//g;
    while ($m && !exists $INC{"$m.pm"}) {
        $m =~ s#(?:^|/)[^/]+$##;
    }
    $m ? $INC{"$m.pm"} : undef;
}

=head2 C<@mods = package_list>

Gather a list of all distributions on the system. XXX UNUSED

=cut

our $INST;
sub inst()
{
    unless ($INST) {
        eval 'require ExtUtils::Installed';
        $INST = new ExtUtils::Installed;
    }
    $INST;
}

sub package_list
{
    sort { $a cmp $b } inst()->modules;
}

=head2 C<@mods = module_list>

Gather a list of all packages (.pm files, really) installed on the
system, grouped by distribution. XXX UNUSED

=cut

sub module_list
{
    @_ = package_list unless @_;
    my $incre = join '|', map quotemeta, @INC;
    $incre = qr|(?:$incre)/|;
    my $inst = inst;
    map {
        [$_, sort map {
            s/$incre//; s|/|::|g;$_
        } grep /\.pm$/, $inst->files($_)]
    } @_;
}

=head2 C<@mods = doc_list>

Gather a list of all documented packages (.?pm files, really)
installed on the system, grouped by distribution. XXX UNUSED

=cut

sub doc_list
{
    @_ = package_list unless @_;
    my $inst = inst;
    map {
        [$_, sort map {
            s/.*man.\///; s|/|::|g;s/\..?pm//; $_
        } grep /\..pm$/, $inst->files($_)]
    } @_;
}

=head2 C<lexicals($subname)>

Return a list of C<$subname>'s lexical variables.  Note that this
includes all nested scopes -- I don't know if or how Perl
distinguishes inner blocks.

=cut

sub lexicals
{
    my $cv = B::svref_2object(\&{+shift});
    return unless $cv && ($cv = $cv->PADLIST);
    my ($names, $vals) = $cv->ARRAY;
    map {
        my $name = $_->PV; $name =~ s/\0.*$//; $name
    } grep B::class($_) ne 'SPECIAL', $names->ARRAY;
}

=head2 C<$lisp = tolisp($perl)>

Convert a Perl scalar to some ELisp equivalent.

=cut

sub tolisp($)
{
    my $thing = @_ == 1 ? shift : \@_;
    my $t = ref $thing;
    if (!$t) {
        if (!defined $thing) {
            'nil'
        } elsif (looks_like_number $thing) {
            ''.(0+$thing);
        } else {
            ## XXX Elisp and perl have slightly different
            ## escaping conventions, so we do this crap instead.
            $thing =~ s/["\\]/\\$1/g;
            qq{"$thing"};
        }
    } elsif ($t eq 'GLOB') {
        (my $name = $$thing) =~ s/\*main:://;
        $name;
    } elsif ($t eq 'ARRAY') {
        '(' . join(' ', map { tolisp($_) } @$thing).')'
    } elsif ($t eq 'HASH') {
        '(' . join(' ', map {
            '(' . tolisp($_) . " . " . tolisp($thing->{$_}) . ')'
        } keys %$thing).')'
    } elsif ($t eq 'Regexp') {
        "'(regexp . \"" . quotemeta($thing) . '")';
#     } elsif ($t eq 'IO') {
    } else {
        qq{"$thing"};
    }
}

=head2 C<printer(\@res, $wantarray)>

Print C<@res> appropriately on the current filehandle.  If C<$ISEVAL>
is true, use terse format.  Otherwise, use human-readable format,
which can use either L<Data::Dumper>, L<YAML>, or L<Data::Dump>.

=cut

%PRINTER = (
    dumper => sub {
        eval { require Data::Dumper };
        local $Data::Dumper::Deparse = 1;
        local $Data::Dumper::Indent = 0;
        local $_;
        my $thing = @res > 1 ? \@res : $res[0];
        eval {
            $_ = Data::Dumper::Dumper($thing);
            s/^\$VAR1 = //;
            s/;$//;
        };
        if (length $_ > ($ENV{COLUMNS} || 80)) {
            $Data::Dumper::Indent = 1;
            eval {
                $_ = Data::Dumper::Dumper($thing);
                s/\A\$VAR1 = //;
                s/;\Z//;
            };
            s/\A\$VAR1 = //;
            s/;\Z//;
        }
        $_;
    },
    plain => sub {
        "@res";
    },
    yaml => sub {
        eval { require YAML };
        if ($@) {
            $PRINTER{dumper}->();
        } else {
            YAML::Dump(\@res);
        }
    },
    dump => sub {
        eval { require Data::Dump };
        if ($@) {
            $PRINTER{dumper}->();
        } else {
            Data::Dump::dump(\@res);
        }
    }
);

sub printer
{
    local *res = shift;
    my ($wantarray) = @_;
    my $res;
    @::__ = @res;
    $::__ = @res == 1 ? $res[0] : [@res];
    my $str;
    if ($ISEVAL) {
        $res = "@res";
    } elsif (@res == 1 && UNIVERSAL::can($res[0], '()')) {
        # overloaded?
        $res = $res[0];
    } elsif (!$ISEVAL && $PRINT_PRETTY && @res > 1 && !grep ref, @res) {
        $res = columnate(@res);
        print $res;
        return;
    } else {
        $res = $PRINTER{$PRINTER}->();
    }
    if ($ISEVAL) {
        print ';;;', length $res, "\n$res\n";
    } else {
        print "$res\n";
    }
}

BEGIN {
    $PS1 = "> ";
    $PACKAGE = 'main';
    $WANTARRAY = 1;
    $PRINTER = 'dumper';
    $PRINT_PRETTY = 1;
}

sub prompt()
{
    "$PACKAGE ".($WANTARRAY ? '@' : '$').$PS1
}

sub Dump
{
    eval {
        Data::Dumper->Dump([$_[0]], [$_[1]]);
    };
}

sub flow
{
    my $n = shift;
    my $n1 = int(2*$n/3);
    local $_ = shift;
    s/(.{$n1,$n}) /$1\n/g;
    $_
}

=head2 C<define_shortcut $name, $sub [, $doc [, $shortdoc]]>

Define $name as a shortcut for function $sub.

=cut

    sub define_shortcut
{
    my ($name, $doc, $short, $fn);
    if (@_ == 2) {
        ($name, $fn) = @_;
        $short = $name;
        $doc = '';
    } elsif (@_ == 3) {
        ($name, $fn, $doc) = @_;
        $short = $name;
    } else {
        ($name, $fn, $short, $doc) = @_;
    }
    $REPL{$name} = $fn;
    $REPL_DOC{$name} = $doc;
    $REPL_SHORT{$name} = $short;
}

sub define_shortcuts
{
    define_shortcut 'help', \&Sepia::repl_help,
        'help [CMD]',
            'Display help on all commands, or just CMD.';
    define_shortcut 'cd', \&Sepia::repl_chdir,
        'cd DIR', 'Change directory to DIR';
    define_shortcut 'pwd', \&Sepia::repl_pwd,
        'Show current working directory';
    define_shortcut 'methods', \&Sepia::repl_methods,
        'methods X [RE]',
            'List methods for reference or package X, matching optional pattern RE';
    define_shortcut 'package', \&Sepia::repl_package,
        'package PKG', 'Set evaluation package to PKG';
    define_shortcut 'who', \&Sepia::repl_who,
        'who PKG [RE]',
            'List variables and subs in PKG matching optional pattern RE.';
    define_shortcut 'wantarray', \&Sepia::repl_wantarray,
        'wantarray [0|1]', 'Set or toggle evaluation context';
    define_shortcut 'format', \&Sepia::repl_format,
        'format [TYPE]', "Set output formatter to TYPE (one of 'dumper', 'dump', 'yaml', 'plain'; default: 'dumper'), or show current type.";
    define_shortcut 'strict', \&Sepia::repl_strict,
        'strict [0|1]', 'Turn \'use strict\' mode on or off';
    define_shortcut 'quit', \&Sepia::repl_quit,
        'Quit the REPL';
    define_shortcut 'reload', \&Sepia::repl_reload,
        'Reload Sepia.pm and relaunch the REPL.';
    define_shortcut 'shell', \&Sepia::repl_shell,
        'shell CMD ...', 'Run CMD in the shell';
    define_shortcut 'eval', \&Sepia::repl_eval,
        'eval EXP', '(internal)';
    define_shortcut 'size', \&Sepia::repl_size,
        'size PKG [RE]',
            'List total sizes of objects in PKG matching optional pattern RE.';
    define_shortcut define => \&Sepia::repl_define,
        'define NAME [\'doc\'] BODY',
            'Define NAME as a shortcut executing BODY';
    define_shortcut undef => \&Sepia::repl_undef,
        'undef NAME', 'Undefine shortcut NAME';
}

sub repl_help
{
    my $width = $ENV{COLUMNS} || 80;
    my $args = shift;
    if ($args =~ /\S/) {
        $args =~ s/^\s+//;
        $args =~ s/\s+$//;
        my $full = $RK{$args};
        if ($full) {
            print "$RK{$full}    ",
                flow($width - length $RK{$full} - 4, $REPL_DOC{$full}), "\n";
        } else {
            print "$args: no such command\n";
        }
    } else {
        my $left = 1 + max map length, values %REPL_SHORT;
        print "REPL commands (prefixed with ','):\n";

        for (sort keys %REPL) {
            my $flow = flow($width - $left, $REPL_DOC{$_});
            $flow =~ s/(.)\n/"$1\n".(' ' x $left)/eg;
            printf "%-${left}s%s\n", $REPL_SHORT{$_}, $flow;
        }
    }
}

sub repl_define
{
    local $_ = shift;
    my ($name, $doc, $body);
    if (/^\s*(\S+)\s+'((?:[^'\\]|\\.)*)'\s+(.+)/) {
        ($name, $doc, $body) = ($1, $2, $3);
    } elsif (/^\s*(\S+)\s+(\S.*)/) {
        ($name, $doc, $body) = ($1, $2, $2);
    } else {
        print "usage: define NAME ['doc'] BODY...\n";
        return;
    }
    my $sub = eval "sub { do { $body } }";
    if ($@) {
        print "usage: define NAME ['doc'] BODY...\n\t$@\n";
        return;
    }
    define_shortcut $name, $sub, $doc;
    %RK = abbrev keys %REPL;
}

sub repl_undef
{
    my $name = shift;
    $name =~ s/^\s*//;
    $name =~ s/\s*$//;
    my $full = $RK{$name};
    if ($full) {
        delete $REPL{$full};
        delete $REPL_SHORT{$full};
        delete $REPL_DOC{$full};
        %RK = abbrev keys %REPL;
    } else {
        print "$name: no such shortcut.\n";
    }
}

sub repl_format
{
    my $t = shift;
    chomp $t;
    if ($t eq '') {
        print "printer = $PRINTER, pretty = @{[$PRINT_PRETTY ? 1 : 0]}\n";
    } else {
        my %formats = abbrev keys %PRINTER;
        if (exists $formats{$t}) {
            $PRINTER = $formats{$t};
        } else {
            warn "No such format '$t' (dumper, dump, yaml, plain).\n";
        }
    }
}

sub repl_chdir
{
    chomp(my $dir = shift);
    $dir =~ s/^~\//$ENV{HOME}\//;
    $dir =~ s/\$HOME/$ENV{HOME}/;
    if (-d $dir) {
        chdir $dir;
        my $ecmd = '(cd "'.Cwd::getcwd().'")';
        print ";;;###".length($ecmd)."\n$ecmd\n";
    } else {
        warn "Can't chdir\n";
    }
}

sub repl_pwd
{
    print Cwd::getcwd(), "\n";
}

sub who
{
    my ($pack, $re_str) = @_;
    $re_str ||= '.?';
    my $re = qr/$re_str/;
    no strict;
    if ($re_str =~ /^[\$\@\%\&]/) {
        ## sigil given -- match it
    sort grep /$re/, map {
        (defined %{$pack.'::'.$_} ? '%'.$_ : (),
         defined ${$pack.'::'.$_} ? '$'.$_ : (), # ?
         defined @{$pack.'::'.$_} ? '@'.$_ : (),
             defined &{$pack.'::'.$_} ? '&'.$_ : (),
            )
        } grep !/::$/ && !/^(?:_<|[^\w])/ && /$re/, keys %{$pack.'::'};
    } else {
        ## no sigil -- don't match it
        sort map {
            (defined %{$pack.'::'.$_} ? '%'.$_ : (),
             defined ${$pack.'::'.$_} ? '$'.$_ : (), # ?
             defined @{$pack.'::'.$_} ? '@'.$_ : (),
         defined &{$pack.'::'.$_} ? $_ : (),
     )
        } grep !/::$/ && !/^(?:_<|[^\w])/ && /$re/, keys %{$pack.'::'};
    }
}


sub columnate
{
    my $len = 0;
    my $width = $ENV{COLUMNS} || 80;
    for (@_) {
        $len = length if $len < length;
    }
    my $nc = int($width / ($len+1)) || 1;
    my $nr = int(@_ / $nc) + (@_ % $nc ? 1 : 0);
    my $fmt = ('%-'.($len+1).'s') x ($nc-1) . "%s\n";
    my @incs = map { $_ * $nr } 0..$nc-1;
    my $str = '';
    for my $r (0..$nr-1) {
        $str .= sprintf $fmt, map { $_ || '' } @_[map { $r + $_ } @incs];
    }
    $str =~ s/ +$//m;
    $str
}

sub repl_who
{
    my ($pkg, $re) = split ' ', shift;
    if ($pkg =~ /^\/(.*)\/?$/) {
        $pkg = $PACKAGE;
        $re = $1;
    } elsif (!$re && !defined %{$pkg.'::'}) {
        $re = $pkg;
        $pkg = $PACKAGE;
    }
    print columnate who($pkg || $PACKAGE, $re);
}

sub methods
{
    my ($pack, $qualified) = @_;
    no strict;
    my @own = $qualified ? grep {
        defined *{$_}{CODE}
    } map { "$pack\::$_" } keys %{$pack.'::'}
        : grep {
            defined *{"$pack\::$_"}{CODE}
        } keys %{$pack.'::'};
    (@own, defined @{$pack.'::ISA'}
         ? (map methods($_, $qualified), @{$pack.'::ISA'}) : ());
}

sub repl_methods
{
    my ($x, $re) = split ' ', shift;
    $x =~ s/^\s+//;
    $x =~ s/\s+$//;
    if ($x =~ /^\$/) {
        $x = $REPL{eval}->("ref $x");
        return 0 if $@;
    }
    $re ||= '.?';
    $re = qr/$re/;
    print columnate sort { $a cmp $b } grep /$re/, methods $x;
}

sub as_boolean
{
    my ($val, $cur) = @_;
    $val =~ s/\s+//g;
    length($val) ? $val : !$cur;
}

sub repl_wantarray
{
    $WANTARRAY = as_boolean shift, $WANTARRAY;
}

sub repl_package
{
    chomp(my $p = shift);
    no strict;
    if (defined %{$p.'::'}) {
        $PACKAGE = $p;
#         my $ecmd = '(setq sepia-eval-package "'.$p.'")';
#         print ";;;###".length($ecmd)."\n$ecmd\n";
    } else {
        warn "Can't go to package $p -- doesn't exist!\n";
    }
}

sub repl_quit
{
    last repl;
}

sub repl_reload
{
    do $INC{'Sepia.pm'};
    if ($@) {
        print "Reload failed:\n$@\n";
    } else {
        $REPL_LEVEL = 0;        # ok?
        goto &Sepia::repl;
    }
}

sub repl_shell
{
    my $cmd = shift;
    print `$cmd 2>& 1`;
}

sub repl_eval
{
    my ($buf) = @_;
    no strict;
    # local $PACKAGE = $pkg || $PACKAGE;
    if ($STRICT) {
        if (!$WANTARRAY) {
            $buf = 'scalar($buf)';
        }
        my $ctx = join(',', keys %{$STRICT->get_context('_')});
        $ctx = $ctx ? "my ($ctx);" : '';
        $buf = eval "sub { package $PACKAGE; use strict; $ctx $buf }";
        if ($@) {
            print "ERROR\n$@\n";
            return;
        }
        $STRICT->call($buf);
    } else {
        $buf = "do { package $PACKAGE; no strict; $buf }";
        if ($WANTARRAY) {
            eval $buf;
        } else {
            scalar eval $buf;
        }
    }
}

## Collects warnings for REPL
my @warn;

sub sig_warn
{
    push @warn, shift
}

sub print_warnings
{
    if (@warn) {
        if ($ISEVAL) {
            my $tmp = "@warn";
            print ';;;'.length($tmp)."\n$tmp\n";
        } else {
            for (@warn) {
                # s/(.*) at .*/$1/;
                print "warning: $_\n";
            }
        }
    }
}

sub repl_banner
{
    print <<EOS;
I need user feedback!  Please send questions or comments to seano\@cpan.org.
Sepia version $Sepia::VERSION.
Type ",h" for help, or ",q" to quit.
EOS
}

=head2 C<repl()>

Execute a command interpreter on standard input and standard output.
If you want to use different descriptors, localize them before
calling C<repl()>.  The prompt has a few bells and whistles, including:

=over 4

=item Obviously-incomplete lines are treated as multiline input (press
'return' twice or 'C-c' to discard).

=item C<die> is overridden to enter a debugging repl at the point
C<die> is called.

=back

Behavior is controlled in part through the following package-globals:

=over 4

=item C<$PACKAGE> -- evaluation package

=item C<$PRINTER> -- result printer (default: dumper)

=item C<$PS1> -- the default prompt

=item C<$STRICT> -- whether 'use strict' is applied to input

=item C<$WANTARRAY> -- evaluation context

=item C<$PRINT_PRETTY> -- format some output nicely (default = 1)

Format some values nicely, independent of $PRINTER.  Currently, this
displays arrays of scalars as columns.

=item C<$REPL_LEVEL> -- level of recursive repl() calls

If zero, then initialization takes place.

=item C<%REPL> -- maps shortcut names to handlers

=item C<%REPL_DOC> -- maps shortcut names to documentation

=item C<%REPL_SHORT> -- maps shortcut names to brief usage

=back

=cut

sub repl
{
    $| = 1;
    if ($REPL_LEVEL == 0) {
        define_shortcuts;
        -f "$ENV{HOME}/.sepiarc" and do "$ENV{HOME}/.sepiarc";
        warn ".sepiarc: $@\n" if $@;
    }
    local $REPL_LEVEL = $REPL_LEVEL + 1;

    my $in;
    my $buf = '';
    my $sigged = 0;

    my $nextrepl = sub { $sigged = 1; };

    local *__;
    local *CORE::GLOBAL::die = \&Sepia::Debug::die;
    local *CORE::GLOBAL::warn = \&Sepia::Debug::warn;
    local @REPL_RESULT;
    Sepia::Debug::add_repl_commands;
    repl_banner if $REPL_LEVEL == 1;
    print prompt;
    my @sigs = qw(INT TERM PIPE ALRM);
    local @SIG{@sigs};
    $SIG{$_} = $nextrepl for @sigs;
 repl: while (defined(my $in = <STDIN>)) {
            if ($sigged) {
                $buf = '';
                $sigged = 0;
                print "\n", prompt;
                next repl;
            }
            $buf .= $in;
            $buf =~ s/^\s*//;
            local $ISEVAL;
            if ($buf =~ /^<<(\d+)\n(.*)/) {
                $ISEVAL = 1;
                my $len = $1;
                my $tmp;
                $buf = $2;
                while ($len && defined($tmp = read STDIN, $buf, $len, length $buf)) {
                    $len -= $tmp;
                }
            }
            my (@res);
            ## Only install a magic handler if no one else is playing.
            local $SIG{__WARN__} = $SIG{__WARN__};
            @warn = ();
            unless ($SIG{__WARN__}) {
                $SIG{__WARN__} = 'Sepia::sig_warn';
            }
            if ($buf =~ /^,(\S+)\s*(.*)/s) {
                ## Inspector shortcuts
                my $short = $1;
                if (exists $Sepia::RK{$short}) {
                    my $ret;
                    my $arg = $2;
                    chomp $arg;
                    $Sepia::REPL{$Sepia::RK{$short}}->($arg, wantarray);
                } else {
                    if (grep /^$short/, keys %Sepia::REPL) {
                        print "Ambiguous shortcut '$short': ",
                            join(', ', sort grep /^$short/, keys %Sepia::REPL),
                                "\n";
                    } else {
                        print "Unrecognized shortcut '$short'\n";
                    }
                    $buf = '';
                    print prompt;
                    next repl;
                }
            } else {
                ## Ordinary eval
                @res = $REPL{eval}->($buf);
                if ($@) {
                    if ($ISEVAL) {
                        ## Always return results for an eval request
                        Sepia::printer \@res, wantarray;
                        Sepia::printer [$@], wantarray;
                        # print_warnings $ISEVAL;
                        $buf = '';
                        print prompt;
                    } elsif ($@ =~ /(?:at|before) EOF(?:$| at)/m) {
                        ## Possibly-incomplete line
                        if ($in eq "\n") {
                            print "Error:\n$@\n*** cancel ***\n", prompt;
                            $buf = '';
                        } else {
                            print ">> ";
                        }
                    } else {
                        print_warnings;
                        # $@ =~ s/(.*) at eval .*/$1/;
                        print "error: $@\n";
                        print prompt;
                        $buf = '';
                    }
                    next repl;
                }
            }
            if ($buf !~ /;\s*$/ && $buf !~ /^,/) {
                ## Be quiet if it ends with a semicolon, or if we
                ## executed a shortcut.
                Sepia::printer \@res, wantarray;
            }
            $buf = '';
            print_warnings;
            print prompt;
        }
    wantarray ? @REPL_RESULT : $REPL_RESULT[0]
}

sub perl_eval
{
    tolisp($REPL{eval}->(shift));
}

=head2 C<$status = html_module_list([$file [, $prefix]])>

Generate an HTML list of installed modules, looking inside of
packages.  If C<$prefix> is missing, uses "about://perldoc/".  If
$file is given, write the result to $file; otherwise, return it as a
string.

=head2 C<$status = html_package_list([$file [, $prefix]])>

Generate an HTML list of installed top-level modules, without looking
inside of packages.  If C<$prefix> is missing, uses
"about://perldoc/".  $file is the same as for C<html_module_list>.

=cut

sub html_module_list
{
    my ($file, $base) = @_;
    $base ||= 'about://perldoc/';
    my $inst = inst();
    return unless $inst;
    my $out;
    open OUT, ">", $file || \$out or return;
    print OUT "<html><body><ul>";
    my $pfx = '';
    my %ns;
    for (package_list) {
        push @{$ns{$1}}, $_ if /^([^:]+)/;
    }
    for (sort keys %ns) {
        print OUT qq{<li><b>$_</b><ul>} if @{$ns{$_}} > 1;
        for (sort @{$ns{$_}}) {
            my @fs = map {
                s/.*man.\///; s|/|::|g; s/\.\d(?:pm)?$//; $_
            } grep /\.\d(?:pm)?$/, sort $inst->files($_);
            if (@fs == 1) {
                print OUT qq{<li><a href="$base$fs[0]">$fs[0]</a>};
            } else {
                print OUT qq{<li>$_<ul>};
                for (@fs) {
                    print OUT qq{<li><a href="$base$_">$_</a>};
                }
                print OUT '</ul>';
            }
        }
        print OUT qq{</ul>} if @{$ns{$_}} > 1;
    }
    print OUT "</ul></body></html>\n";
    close OUT;
    $file ? 1 : $out;
}

sub html_package_list
{
    my ($file, $base) = @_;
    return unless inst();
    $base ||= 'about://perldoc/';
    my $out;
    open OUT, ">", $file || \$out or return;
    print OUT "<html><body><ul>";
    my $pfx = '';
    my %ns;
    for (package_list) {
        push @{$ns{$1}}, $_ if /^([^:]+)/;
    }
    for (sort keys %ns) {
        if (@{$ns{$_}} == 1) {
            print OUT
                qq{<li><a href="$base$ns{$_}[0]">$ns{$_}[0]</a>};
        } else {
            print OUT qq{<li><b>$_</b><ul>};
            print OUT qq{<li><a href="$base$_">$_</a>}
                for sort @{$ns{$_}};
            print OUT qq{</ul>};
        }
    }
    print OUT "</ul></body></html>\n";
    close OUT;
    $file ? 1 : $out;
}

1;
__END__

=head1 TODO

See the README file included with the distribution.

=head1 SEE ALSO

Sepia's public GIT repository is located at L<http://repo.or.cz/w/sepia.git>.

There are several modules for Perl development in Emacs on CPAN,
including L<Devel::PerlySense> and L<PDE>.  For a complete list, see
L<http://emacswiki.org/cgi-bin/wiki/PerlLanguage>.

=head1 AUTHOR

Sean O'Rourke, E<lt>seano@cpan.orgE<gt>

Bug reports welcome, patches even more welcome.

=head1 COPYRIGHT

Copyright (C) 2005-2008 Sean O'Rourke.  All rights reserved, some
wrongs reversed.  This module is distributed under the same terms as
Perl itself.

=cut
