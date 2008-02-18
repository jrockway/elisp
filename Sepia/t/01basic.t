#!/usr/bin/env perl
use Test::Simple tests => 15;

require Data::Dumper;
require Sepia;
require Sepia::Xref;
require Sepia::Debug;
ok(1, 'loaded');

Sepia::Xref::rebuild();
ok(1, 'rebuild');

sub all
{
    my $ok = 1;
    $ok &&= $_ for @_;
    $ok;
}

my @loc1 = Sepia::location('Sepia::location');
ok($loc1[0][0] =~ /Sepia\.pm$/, 'location');
ok((grep { $_ eq 'Sepia::location' } Sepia::apropos('location')), 'apropos');
# 4 to here
sub apply_to_loc                # 3 tests per call.
{
    my $f = shift;
    my $loc1 = $f->('location');
    ok($loc1, 'location 1');
    my $loc2 = $f->('Sepia::location');
    ok($loc2, 'fullname location');
    my $ok = 1;
    ok(all(map { $loc1->[$_] eq $loc2->[$_] } 0..$#{$loc1}), 'sameness');
    $loc1;
}

apply_to_loc(\&Sepia::Xref::callers);
apply_to_loc(\&Sepia::Xref::callees);
# 10 tests to here.

my @subs = Sepia::mod_subs('Sepia');
ok(all(map { defined &{"Sepia::$_"} } @subs), 'mod_subs');
if (exists $INC{'Module/Info.pm'}) {
    ok(Sepia::module_info('Sepia', 'name') eq 'Sepia');
    ok(Sepia::module_info('Sepia', 'version') eq $Sepia::VERSION);
    ok(Sepia::module_info('Sepia', 'file') =~ /Sepia\.pm$/);
    ok(Sepia::module_info('Sepia', 'is_core') == 0);
} else {
    ok(1, 'skipped -- no Module::Info') for 1..4;
}

exit;
