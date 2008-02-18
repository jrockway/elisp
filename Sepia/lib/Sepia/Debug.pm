package Sepia::Debug;
# use Sepia;
use Carp ();                    # old Carp doesn't export shortmess.
use Text::Abbrev;
use strict;
use vars qw($pack $file $line $sub $level
            $STOPDIE $STOPWARN);

sub define_shortcut;
*define_shortcut = *Sepia::define_shortcut;

BEGIN {
    ## Just leave it on -- with $DB::trace = 0, there doesn't seem
    ## to be a perforamnce penalty!
    $^P = 0x303;
    $STOPDIE = 1;
    $STOPWARN = 0;
}

sub peek_my
{
    eval { require PadWalker };
    if ($@) {
        +{ }
    } else {
        *peek_my = \&PadWalker::peek_my;
        goto &peek_my;
    }
}

# set debugging level
sub repl_debug
{
    debug(@_);
}

sub repl_backtrace
{
    for (my $i = 0; ; ++$i) {
        my ($pack, $file, $line, $sub) = caller($i);
        last unless $pack;
        print($i == $level+3 ? "*" : ' ', " [$i]\t$sub ($file:$line)\n");
    }
}

# return value from die
sub repl_return
{
    if ($Sepia::WANTARRAY) {
        @Sepia::REPL_RESULT = $Sepia::REPL{eval}->(@_);
    } else {
        $Sepia::REPL_RESULT[0] = $Sepia::REPL{eval}->(@_);
    }
    last repl;
}

use vars qw($DIE_TO @DIE_RETURN $DIE_LEVEL);
$DIE_LEVEL = 0;

sub repl_xreturn
{
    ($DB::DIE_TO, $DB::DIE_RETURN[0]) = split ' ', $_[0], 2;
    $DB::DIE_RETURN[0] = $Sepia::REPL{eval}->($DB::DIE_RETURN[0]);
    last SEPIA_DB_SUB;
}

# { package DB;
#  no strict;
sub sub
{
    no strict;
    local $DIE_LEVEL = $DIE_LEVEL + 1;
    ## Set up a dynamic catch target
 SEPIA_DB_SUB: {
        return &$DB::sub;
    };
    # we're dying!
    last SEPIA_DB_SUB
        if $DIE_LEVEL > 1 && defined $DIE_TO
            && $DB::sub !~ /(?:^|::)\Q$DIE_TO\E$/;
    undef $DIE_TO;
    wantarray ? @DIE_RETURN : $DIE_RETURN[0]
}
# }

sub repl_dbsub
{
    my $arg = shift;
    if ($arg) {
        *DB::sub = \&sub;
    } else {
        undef &DB::sub;
    }
}

sub repl_lsbreak
{
    no strict 'refs';
    for my $file (sort grep /^_</ && defined %{"::$_"}, keys %::) {
        my ($name) = $file =~ /^_<(.*)/;
        my @pts = keys %{"::$file"};
        next unless @pts;
        print "$name:\n";
        for (sort { $a <=> $b } @pts) {
            print "\t$_\t${$file}{$_}\n"
        }
    }
}

# evaluate EXPR in environment ENV
sub eval_in_env
{
    my ($expr, $env) = @_;
    local $Sepia::ENV = $env;
    my $str = '';
    for (keys %$env) {
        next unless /^([\$\@%])(.+)/;
        $str .= "local *$2 = \$Sepia::ENV->{'$_'}; ";
    }
    eval "do { no strict; $str $expr }";
}

sub tie_class
{
    my $sig = substr shift, 0, 1;
    return $sig eq '$' ? 'Tie::StdScalar'
        : $sig eq '@' ? 'Tie::StdArray'
            : $sig eq '%' ? 'Tie::StdHash'
                : die "Sorry, can't tie $sig\n";
}

## XXX: this is a better approach (the local/tie business is vile),
## but it segfaults and I'm not sure why.
sub eval_in_env2
{
    my ($expr, $env, $fn) = @_;
    local $Sepia::ENV = $env;
    my @vars = grep /^([\$\@%])(.+)/, keys %$env;
    my $body = 'sub { my ('.join(',', @vars).');';
    for (@vars) {
        $body .= "Devel::LexAlias::lexalias(\$Sepia::ENV, '$_', \\$_);"
    }
    $body .= "$expr }";
    print STDERR "---\n$body\n---\n";
    $body = eval $body;
    $@ || $body->();
}

# evaluate EXP LEV levels up the stack
sub repl_upeval
{
    eval_in_env(shift, peek_my(4+$level));
}

# inspect lexicals at level N, or current level
sub repl_inspect
{
    my $i = shift;
    if ($i =~ /\d/) {
        $i = 0+$i;
    } else {
        $i = $level + 3;
    }
    my $sub = (caller $i)[3];
    if ($sub) {
        my $h = peek_my($i+1);
        print "[$i] $sub:\n";
        for (sort keys %$h) {
            local @Sepia::res = $h->{$_};
            print "\t$_ = ", $Sepia::PRINTER{$Sepia::PRINTER}->(), "\n";
        }
    }
}

sub debug
{
    my $new = Sepia::as_boolean(shift, $DB::trace);
    return if $new == $DB::trace;
    if ($new) {
        # $^P = 0x2 | 0x10 | 0x100 | 0x200;
        # *DB::DB = \&repl;
        $DB::trace = 1;
        print "debug ON\n";
    } else {
        $DB::trace = 0;
        print "debug OFF\n";
    }
}

sub breakpoint_file
{
    my ($file) = @_;
    return \%{$main::{"_<$file"}} if exists $main::{"_<$file"};
    if ($file !~ /^\//) {
        ($file) = grep /^_<.*\/\Q$file\E$/, keys %main::;
        return \%{$main::{$file}} if $file;
    }
    return undef;
}

sub breakpoint
{
    my ($file, $line, $cond) = @_;
    my $h = breakpoint_file $file;
    if (defined $h) {
        $h->{$line} = $cond || 1;
        return $cond ? "$file\:$line if $cond" : "$file\:$line";
    }
    return undef;
}

sub repl_break
{
    my $arg = shift;
    $arg =~ s/^\s+//;
    $arg =~ s/\s+$//;
    my ($f, $l, $cond) = $arg =~ /^(.+?):(\d+)\s*(.*)/;
    $cond = 1 unless $cond =~ /\S/;
    $f ||= $file;
    $l ||= $line;
    return unless defined $f && defined $l;
    my $bp = breakpoint($f, $l, $cond);
    print "break $bp\n" if $bp;
}

sub update_location
{
    # XXX: magic numberage.
    ($pack, $file, $line, $sub) = caller($level + shift);
}

sub show_location
{
    print "_<$file:$line>\n" if defined $file && defined $line;
}

sub repl_list
{
    my @lines = eval shift;
    @lines = $line - 5 .. $line + 5 unless @lines;
    printf '%-6d%s', $_, ${"::_<$file"}[$_-1] for @lines;
}

sub repl_delete
{
    my ($f, $l) = split /:/, shift;
    $f ||= $file;
    $l ||= $line;
    my $h = breakpoint_file $f;
    delete $h->{$l} if defined $h;
}

sub add_repl_commands
{
    define_shortcut 'delete', \&repl_delete,
        'Delete current breakpoint.';
    define_shortcut 'debug', \&repl_debug,
        'debug [0|1]', 'Enable or disable debugging.';
    define_shortcut 'break', \&repl_break,
        'break [F:N [E]]',
        'Set a breakpoint in F at line N (or at current position), enabled if E evalutes to true.';
    define_shortcut 'lsbreak', \&repl_lsbreak,
        'List breakpoints.';
    define_shortcut 'dbsub', \&repl_dbsub, '(Un)install DB::sub.';
    %Sepia::RK = abbrev keys %Sepia::REPL;
}

sub add_debug_repl_commands
{

    define_shortcut up => sub {
        $level += shift || 1;
        update_location(4);
        show_location;
    }, 'up [N]', 'Move up N stack frames.';
    define_shortcut down => sub {
        $level -= shift || 1;
        $level = 0 if $level < 0;
        update_location(4);
        show_location;
    }, 'down [N]', 'Move down N stack frames.';
    define_shortcut continue => sub {
        $level = 0;
        $DB::single = 0;
        last repl;
    }, 'Yep.';

    define_shortcut next => sub {
        my $n = shift || 1;
        $DB::single = 0;
        breakpoint $file, $line + $n, 'next';
        last repl;
    }, 'next [N]', 'Advance N lines, skipping subroutines.';

    define_shortcut step => sub {
        $DB::single = shift || 1;
        last repl;
    }, 'step [N]', 'Step N lines forward, entering subroutines.';

    define_shortcut list => \&repl_list,
        'list EXPR', 'List source lines of current file.';
    define_shortcut backtrace => \&repl_backtrace, 'show backtrace';
    define_shortcut inspect => \&repl_inspect,
        'inspect [N]', 'inspect lexicals in frame N (or current)';
    define_shortcut return => \&repl_return, 'return EXPR', 'return EXPR';
    define_shortcut xreturn => \&repl_xreturn, 'xreturn NAME EXPR',
        'xreturn NAME EXPR';
    define_shortcut eval => \&repl_upeval,
        'eval EXPR', 'evaluate EXPR in current frame';      # DANGER!
}

sub repl
{
    show_location;
    local %Sepia::REPL = %Sepia::REPL;
    local %Sepia::REPL_DOC = %Sepia::REPL_DOC;
    add_debug_repl_commands;
    map { define_shortcut @$_ } @_;
    local %Sepia::RK = abbrev keys %Sepia::REPL;
    # local $Sepia::REPL_LEVEL = $Sepia::REPL_LEVEL + 1;
    local $Sepia::PS1 = "*$Sepia::REPL_LEVEL*> ";
    Sepia::repl();
}

sub DB::DB
{
    return if $Sepia::ISEVAL;
    local $level = 0;
    local ($pack, $file, $line, $sub) = caller($level);
    ## Don't do anything if we're inside an eval request, even if in
    ## single-step mode.
    return unless $DB::single || exists $main::{"_<$file"}{$line};
    if ($DB::single) {
        return unless --$DB::single == 0;
    } else {
        my $cond = $main::{"_<$file"}{$line};
        if ($cond eq 'next') {
            delete $main::{"_<$file"}{$line};
        } else {
            return unless $Sepia::REPL{eval}->($cond);
        }
    }
    repl();
}

my $MSG = "('\\C-c' to exit, ',h' for help)";

sub die
{
    ## Protect us against people doing weird things.
    if ($STOPDIE && !$SIG{__DIE__}) {
        my @dieargs = @_;
        local $level = 0;
        local ($pack, $file, $line, $sub) = caller($level);
        my $tmp = "@_";
        $tmp .= "\n" unless $tmp =~ /\n\z/;
        print "$tmp\tin $sub\nDied $MSG\n";
        my $trace = $DB::trace;
        $DB::trace = 1;
        repl(
            [die => sub { local $STOPDIE=0; CORE::die @dieargs },
             'Continue dying.'],
            [quit => sub { local $STOPDIE=0; CORE::die @dieargs },
             'Continue dying.']);
        $DB::trace = $trace;
    } else {
        CORE::die(Carp::shortmess @_);
    }
    1;
}

sub warn
{
    ## Again, this is above our pay grade:
    if ($STOPWARN && $SIG{__WARN__} eq 'Sepia::sig_warn') {
        my @dieargs = @_;
        my $trace = $DB::trace;
        $DB::trace = 1;
        local $level = 0;
        local ($pack, $file, $line, $sub) = caller($level);
        print "@_\n\tin $sub\nWarned $MSG\n";
        repl(
            [warn => sub { local $STOPWARN=0; CORE::warn @dieargs },
             'Continue warning.'],
            [quit => sub { local $STOPWARN=0; CORE::warn @dieargs },
             'Continue warning.']);
        $DB::trace = $trace;
    } else {
        ## Avoid showing up in location information.
        CORE::warn(Carp::shortmess @_);
    }
}

sub oops
{
    my $sig = shift;
    if ($STOPDIE) {
        my $trace = $DB::trace;
        $DB::trace = 1;
        local $level = 0;
        local ($pack, $file, $line, $sub) = caller($level);
        print "@_\n\tin $sub\nCaught signal $sig\n";
        repl(
        [die => sub { local $STOPDIE=0; CORE::die "Caught signal $sig; exiting." },
         'Just die.'],
        [quit => sub { local $STOPWARN=0; CORE::die "Caught signal $sig; exiting." },
         'Just die.']);
        $DB::trace = $trace;
    } else {
        Carp::confess "Caught signal $sig: continue at your own risk.";
    }
}

1;
