#! /usr/bin/perl
use strict;
use warnings;
eval "use diagnostics" unless defined $ENV{'NDEBUG'}; # because perl; makes things SLOWER.

# 1-3 a: abcde is valid: position 1 contains a and position 3 does not.
# 1-3 b: cdefg is invalid: neither position 1 nor position 3 contains b.
# 2-9 c: ccccccccc is invalid: both position 2 and position 9 contain c.

while (<>) {
    chomp;
    my $line = $_;
    if ($line =~ /([0-9]+)\-([0-9]+) *(.): *(.*)/) {
        my $pos1 = $1;
        my $pos2 = $2;
        my $ch = $3;
        my $pass = $4;
        die $line unless length($ch) == 1;
        #my @letters = split(//, $line); this idiocy took me a stupid amount of time...
        my @letters = split(//, $pass);
        my $ch1 = $letters[$pos1 - 1];
        my $ch2 = $letters[$pos2 - 1];
        #if (($ch eq $indexfrom1[$pos1]) != ($ch eq $indexfrom1[$pos2])) {
        if ($ch eq $ch1) {
            if ($ch ne $ch2) {
                print $line,"\n";
            }
        } else {
            if ($ch eq $ch2) {
                print $line,"\n";
                #warn "ch=($ch) ch2=($ch2) pos1=$pos1 pos2=$pos2" if $pass eq "dnntb";
            }
        }
        if ($pass eq "dnntb") {
            #warn "ch1" if $ch eq $ch1;
            #warn "ch2" if $ch eq $ch2;
            #warn join(' ', $pos1, $pos2, $ch, $pass);
        }
    } else {
        die "skip($line)";
    }
}
