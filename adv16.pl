#! /usr/bin/perl
# language picked by /dev/urandom; TODO maybe zig? (not forth!)
use strict;
use warnings;
use diagnostics;

die "usage: $0 ticketfile" unless $#ARGV == 0;
open(my $fh, '<', $ARGV[0]) or die "open ticket: $!";

my $dbg = \&CORE::warn;
$dbg = sub {} if defined $ENV{'NDEBUG'};

# comma separated numbers => array reference with those numbers.
sub parse_ticket($) {
    my ($line) = @_;
    chomp $line;
    my @fields;
    while (length($line) > 0) {
        die $line unless $line =~ /([0-9]+),?(.*)/;
        # "It doesn't matter which position corresponds to which field"
        #$dbg->("$1 in line $line");
        push @fields, $1;
        $line = $2;
    }
    $dbg->("ticket = ", join(' ', @fields));
    return \@fields;
}

# 1. rules, eg "class: 1-3 or 5-7"
# rule{'class'} = (1,3) means class is 1,2,or 3
# except we need an array of arrays (inclusive or) => array of references
my %rule;
while (<$fh>) {
    chomp;
    my $line = $_;
    $dbg->("line($line)");
    # "departure station:" => can't use eg (\w+)
    last unless $line =~ /^(.+): *(.*)/;
    my $field = $1;
    $dbg->("field($field)");
    die "$field appears twice in line($line)" if defined $rule{$field};
    my @all;
    my $todo = $2;
    while (length($todo) > 0) {
        die "$todo($line)" unless $todo =~ /([0-9]+)\-([0-9]+) *(.*)/;
        my ($min,$max) = ($1,$2);
        die "min($min),max($max) in line $line" unless $max >= $min;
        $dbg->("min=$min max=$max");
        push @all, [$min,$max];
        $todo = $3;
        $todo =~ s/^or +//;
    }
    die "empty rules for $field?" unless $#all >= 0;
    $rule{$field} = \@all;
}

# 2.: "your ticket:" (empty line eaten in the previous loop)
my $ticket;
while (<$fh>) {
    die $_ unless $_ =~ /your ticket:/;
    my $line = <$fh>;
    die "??" unless defined $line;
    $ticket = parse_ticket($line);
    last;
}
die "no ticket?" unless defined $ticket;

# 3: empty line, then "nearby tickets:", then list of tickets.
my $line_empty = <$fh>;
my $line_nearby = <$fh>;
die "wanted nearby tickets: $line_nearby" unless $line_nearby eq "nearby tickets:\n";
my @tickets;
while (<$fh>) {
    push @tickets, parse_ticket($_);
}

# part1: find tickets that contain ONE fields that CANNOT be any valid value
# (ie, out of range for all rules) => sum those invalid fields

sub valid_field($) {
    my ($value) = (@_);
    foreach my $field(keys %rule) {
        my $ref = $rule{$field};
        foreach my $minmaxref(@$ref) {
            my @minmax = @$minmaxref;
            my ($min,$max) = @minmax;
            #$dbg->("$field: $min <= $value <= $max?");
            return 1 if ($min <= $value && $value <= $max);
        }
    }
    $dbg->("part1 BAD ticket field ($value)");
    return 0;
}
# return field value, undef if ok
sub invalid_field_in_ticket($) {
    my ($ticket) = @_;
    my $retval;
    foreach my $field(@$ticket) {
        next if valid_field($field);
        die "invalid $retval and $field" if defined $retval;
        $dbg->("bad field: $field");
        $retval = $field;
    }
    return $retval;
}

sub part1() {
    my $total = 0;
    foreach my $nearby(@tickets) {
        my $bad = invalid_field_in_ticket($nearby);
        next unless defined $bad;
        $dbg->("$total += $bad");
        $total += $bad;
    }
    print "total = $total\n";
}
part1() if 0; # getopt or whatever

# --- Part Two ---
# Now that you've identified which tickets contain invalid values, discard those tickets entirely.
# Use the remaining valid tickets to determine which field is which...

sub only_valid_tickets {
    my @validtickets;
    foreach my $nearby(@tickets) {
        next if invalid_field_in_ticket($nearby);
        push @validtickets, $nearby;
    }
    return @validtickets;
}
@tickets = only_valid_tickets(@tickets);

sub arraylen {
    return 1 + $#_; # I always make this mistake
}

# eg valid_positions[row] = (0,1,2) => test until only ONE valid position remains.
my %valid_positions; # INDEX into array!
my $nfields = arraylen(keys %rule);
foreach my $field(keys %rule) {
    my @a = 0..$nfields;
    pop @a; # 0 .. n-1
    $valid_positions{$field} = \@a;
}

sub mark_invalid($$) {
    my ($arrayref, $i) = @_;
    die "??" unless defined $arrayref->[$i];
    $arrayref->[$i] = -666;
}

sub marked_invalid($$) {
    my ($arrayref, $i) = @_;
    die "??" unless defined $arrayref->[$i];
    return $arrayref->[$i] < 0;
}

sub value_follows_rule($$) {
    my ($value, $ruleref) = @_;
    #$dbg->("follow? value($value) ruleref($ruleref)");
    foreach my $minmaxref(@$ruleref) {
        #$dbg->("minmaxref($minmaxref)");
        my @minmax = @$minmaxref;
        my ($min,$max) = @minmax;
        #$dbg->("$min <= $value <= $max?");
        return 1 if ($min <= $value && $value <= $max);
    }
    return 0;
}

# pruning out invalid fields is not enough for obvious reasons,
# even in the test input => try all permutations!
# XXX no, it turns out there are 30 fields => 30 factorial is > 2^100...
# I'm preserving it for posterity, just because.
# The above is correct if we look for ANY invalid field.

sub any_ticket_has_invalid_nth_value($$$) {
    my ($i, $ruleref, $ticketsref) = @_;
    foreach my $ticket(@$ticketsref) {
        my $value = $ticket->[$i];
        next if (value_follows_rule($value, $ruleref));
        return 1;
    }
    return 0;
}

# foreach field: if ANY ticket has that field invalid, mark it invalid.
for (my $i = 0; $i < $nfields; $i++) {
    foreach my $field(keys %rule) {
        if (any_ticket_has_invalid_nth_value($i, $rule{$field}, \@tickets)) {
            $dbg->("$field cannot be $i");
            mark_invalid($valid_positions{$field}, $i);
        }
    }
}

my %fieldidx;
foreach my $field(keys %valid_positions) {
    my @nonnegative = grep { $_ >= 0 } @{$valid_positions{$field}};
    $valid_positions{$field} = \@nonnegative;
    my $validref = $valid_positions{$field};
    print "$field: ", join(' ', @$validref), "\n\n";
    next;
    foreach my $i(@$validref) {
        die $field if defined $fieldidx{$field};
        $fieldidx{$field} = $i;
    }
    die "undefd $field" unless defined $fieldidx{$field};
}

# urgh... messy!
my @todo = keys %valid_positions;
while ($#todo >= 0) {
    my $found = 0;
    foreach my $field(@todo) {
        my @p = @{$valid_positions{$field}};
        die "no positions left for $field!" if $#p < 0;
        next unless $#p == 0;
        my $fixed = $p[0];
        $dbg->("$field must be ${p[0]}");
        # this position is fixed => eliminate from all other candidates
        foreach my $other(keys %valid_positions) {
            next if $field eq $other;
            my @o = @{$valid_positions{$other}};
            @o = grep { $_ != $fixed } @o;
            $valid_positions{$other} = \@o;
        }
        @todo = grep { $_ ne $field } @todo;
        $found = 1;
        last;
    }
    die "stuck at ", join(' ',@todo) unless $found;
}

...;



# Once you work out which field is which, look for the six fields on your ticket that start with the word departure.
# What do you get if you multiply those six values together?
# my ticket is simply called "ticket" (because "my $ticket")
my $myticket = $ticket;
my $mul = 1;
foreach my $field(keys %rule) {
    next unless $field =~ /^departure/;
    $dbg->("field($field)");
    my $i = $fieldidx{$field};
    my $value = $myticket->[$i];
    $dbg->("mult by $value");
    $mul *= $value;
}
print "we obtain $mul";




...;

