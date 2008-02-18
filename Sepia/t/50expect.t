#!/usr/bin/env perl

BEGIN {
    eval 'use Test::Expect';
    if ($@) {
        print "# requires Test::Expect\n1..1\nok 1\n";
        exit 0;
    } else {
        eval 'use Test::Simple tests => 36';
    }
}

use FindBin '$Bin';
use Sepia;
use Sepia::Xref;

expect_run
    command => "$^X -Mblib -MSepia -MSepia::Xref -e Sepia::repl",
    prompt => [-re => 'main @[^>]*> '],
    quit => ',quit';
expect_handle()->log_file('/tmp/b') if $ENV{USER} eq 'seano';

expect ",help",
q!REPL commands (prefixed with ','):
    break [F:N [E]]    Set a breakpoint in F at line N (or at current
                       position), enabled if E evalutes to true.
    cd DIR             Change directory to DIR
    debug [0|1]        Enable or disable debugging.
    delete             Delete current breakpoint.
    format [dumper|dump|yaml|plain]
                       Set output formatter (default: dumper)
    help               Display this message
    lsbreak            List breakpoints.
    methods X [RE]     List methods for reference or package X,
                       matching optional pattern RE.

    package PACKAGE    Set evaluation package to PACKAGE
    quit               Quit the REPL
    reload             Reload Sepia.pm and relaunch the REPL.
    shell CMD ...      Run CMD in the shell.
    strict [0|1]       Turn 'use strict' mode on or off
    wantarray [0|1]    Set or toggle evaluation context
    who PACKAGE [RE]   List variables and subs in PACKAGE matching optional
                       pattern RE.!
    if 0;

expect_send ",wh Sepia::Xref xref";
expect_like qr/xref \s+ xref_definitions \s+ xref_main \s+ xref_cv \s+ xref_exclude \s+ xref_object \s* /x;

expect_send '{ package A; sub a {}; package X; @ISA = qw(A); sub x {} };';
expect ",wh X", '@ISA x', 'package list';
expect ",me X", 'a x', 'methods 1';

expect '$x = bless {}, X;', '$x = bless {}, X;'; # XXX: stupid expect.
expect ',me $x', ",me \$x\na x", 'methods 2';    # XXX: stupid expect.

######################################################################
## Debugger
expect ',lsb', '';
expect_send ',debug 1';
expect_send "do '$Bin/testy.pl';", 'get testy';

expect 'fib1 10', '55', 'plain fib';
expect ',br testy.pl:6', "break testy.pl:6 if 1", 'break?';
expect_send 'fib1 10';
# sleep 1;
expect_like qr|_<$Bin/testy.pl:6>|, 'break in fib';
# XXX AGAIN STUPID EXPECT!
expect '$n = 3', "\$n = 3\n3", 'munge lexicals';
expect '$n = 3', "\$n = 3\n3", 'munge lexicals';
expect ',in',
'[3] DB::DB:
	$n = \3', 'munged';
expect ',del', '';
expect ',con', '2', 'return from fib';
expect_send 'fib2 10', 'bad fib';
expect_like qr/_<$Bin\/testy.pl:12>/;
expect_send ',q', 'quit';
# expect_like qr/_<$Bin\/testy.pl:12>/;
expect_like qr/error: asdf/, 'saw die message';

print <<'EOS' if 0;             # for debugging
,help
,wh Sepia::Xref xref
{ package A; sub a {}; package X; @ISA = qw(A); sub x {} };
,wh X
,me X
$x = bless {}, X;
,me $x
,lsb
,debug 1
do 'testy.pl';
fib1 10
,br testy.pl:6
fib1 10
$n = 3
,in
,del
,con
fib2 10
,q
EOS
