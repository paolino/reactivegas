#!/run/current-system/sw/bin/perl
use strict;
use warnings;
use open qw(:std :utf8);
(@ARGV == 3) or die "usage: mutate.pl FILE OLD NEW";
my ($f, $old, $new) = @ARGV;
open my $in, '<:encoding(UTF-8)', $f or die "open $f: $!";
local $/;
my $s = <$in>;
close $in;
my $c = () = $s =~ /\Q$old\E/g;
die "MATCH-COUNT=$c (need exactly 1) for [$old]" unless $c == 1;
$s =~ s/\Q$old\E/$new/;
open my $out, '>:encoding(UTF-8)', $f or die "write $f: $!";
print $out $s;
close $out;
print "MUTATED-OK $f\n";
