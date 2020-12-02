#! /usr/bin/perl
use strict;
use warnings;
eval "use diagnostics" unless defined $ENV{'NDEBUG'}; # because perl; makes things SLOWER.

sub countreps($$) {
    my ($ch, $string) = @_;
    die $ch unless length($ch) == 1;
    my @letters = split(//, $string);
    my $n = 0;
    foreach my $x(@letters) {
        $n++ if $ch eq $x;
    }
    return $n;
}

while (<>) {
    chomp;
    my $line = $_;
    if ($line =~ /([0-9]+)\-([0-9]+) *(.): *(.*)/) {
        my $lo = $1;
        my $hi = $2;
        my $ch = $3;
        my $pass = $4;
        die $line unless length($ch) == 1;
        my $n = countreps($ch, $pass);
        next unless $n >= $lo && $n <= $hi;
        print $line,"\n";
    } else {
        warn "skip($line)";
    }
}
