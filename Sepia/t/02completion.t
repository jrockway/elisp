#!/usr/bin/env perl

use Test::Simple tests => 11;
use Data::Dumper;
require Sepia;
no warnings;

## Set up some symbols to complete on:
package Z::A;
sub a_function { }
sub a_nother_function { }
$a_var = 0;
@a_var2 = ();
%a_var3 = ();
package Z::Another;
sub a_function { }
sub a_nother_function { }
$a_var = 0;
@a_var2 = ();
%a_var3 = ();
package Z::A::Nother;
sub a_function { }
sub a_nother_function { }
$a_var = 0;
@a_var2 = ();
%a_var3 = ();
package Z::Blah;
sub a_function { }
sub a_nother_function { }
$a_var = 0;
@a_var2 = ();
%a_var3 = ();
## Whew!
package main;

sub ok_comp
{
    my $str = shift;
    my $res = Dumper([sort(Sepia::completions($str))]);
    my $expect = Dumper([sort @_]);
    my $ok = $res eq $expect;
    ok($ok, $ok ? $str : "$str\n$res\n$expect\n");
}

ok_comp('$Z:A:a_v', qw($Z::A::a_var $Z::Another::a_var));
ok_comp('@Z:A:a_v', qw(@Z::A::a_var2 @Z::Another::a_var2));
ok_comp('%Z:A:a_v', qw(%Z::A::a_var3 %Z::Another::a_var3));
ok_comp('%z:a:a_v', qw(%Z::A::a_var3 %Z::Another::a_var3));
ok_comp('%z:a:a_', qw(%Z::A::a_var3 %Z::Another::a_var3));
ok_comp('%z:a:a', qw(%Z::A::a_var3 %Z::Another::a_var3));
ok_comp('Z:A:a_v');
ok_comp('Z:A:a', qw(Z::A::a_nother_function Z::Another::a_nother_function
                    Z::A::a_function Z::Another::a_function));
ok_comp('z:a:a', qw(Z::A::a_nother_function Z::Another::a_nother_function
                    Z::A::a_function Z::Another::a_function));
ok_comp('zaa', qw(Z::A::a_nother_function Z::Another::a_nother_function
                    Z::A::a_function Z::Another::a_function));
ok_comp('za', qw(Z::A:: Z::Another::));

