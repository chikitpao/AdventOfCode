#!/bin/env perl

#   AoC2020, Day 11: Seating System
#   Author: Chi-Kit Pao
#
#   Output:
#   Question 1: How many seats end up occupied?
#   Answer: 2238
#   Question 2: How many seats end up occupied?
#   Answer: 2013
#
#   Time usage shown via command "time":
#   real	0m6,067s
#   user	0m6,059s
#   sys	0m0,009s


use strict;
use warnings;

use constant {
    INVALID   => "",
    FLOOR   => ".",
    EMPTY   => "L",
    OCCUPIED   => "#"
};

sub parse_file {
    my $file_name = $_[0];

    my @seats = ();

    open(my $fh, "<", $file_name) or die "Can't open input.txt: $!";
    while (my $line = <$fh>) {
    chomp($line); # Get rid of newline
    push(@seats, $line);
    }
    close($fh) || warn "close failed: $!";

    print "Row count: ", scalar(@seats), "\n";
    print "Column count: ", length($seats[0]), "\n";

    return @seats;
}

sub get_seat {
    my $seat_string = $_[0];
    my $row_count = $_[1];
    my $column_count = $_[2];
    my $row = $_[3];
    my $column = $_[4];

    if ($row < 0 || $row >= $row_count) {
        return INVALID;
    }
    if ($column < 0 || $column >= $column_count) {
        return INVALID;
    }

    return substr($seat_string, $row * $column_count + $column, 1)
}

sub change_state {
    my $seat_string = $_[0];
    my $row_count = $_[1];
    my $column_count = $_[2];
    my $row = $_[3];
    my $column = $_[4];
    my $part = $_[5];

    my $my_seat = get_seat($seat_string, $row_count, $column_count, $row, $column);
    if ($my_seat eq INVALID || $my_seat eq FLOOR) {
        return $my_seat;
    }

    my $occupied_count = 0;
    my $is_empty = $my_seat eq EMPTY;
    for my $i (-1 .. 1) {
        for my $j (-1 .. 1) {
            if ($i != 0 || $j != 0) {
                if ($part != 2) {
                    if (get_seat($seat_string, $row_count, $column_count, $row + $i,
                        $column + $j) eq OCCUPIED) {
                        $occupied_count += 1;
                    }
                } else {
                    my $row_offset = $i;
                    my $column_offset = $j;
                    while (1) {
                        my $seat = get_seat($seat_string, $row_count, $column_count,
                            $row + $row_offset, $column + $column_offset);
                        if ($seat ne FLOOR) {
                            if ($seat eq OCCUPIED)  {
                                $occupied_count += 1;
                            }
                            last;
                        }
                        $row_offset += $i;
                        $column_offset += $j;
                    }
                }
            }
        }
    }

    if ($is_empty && $occupied_count == 0) {
        return OCCUPIED;
    }

    my $occupation_tolerance = 4;
    if ($part == 2) {
        $occupation_tolerance = 5;
    }
    if (!$is_empty && $occupied_count >= $occupation_tolerance) {
        return EMPTY;
    }
    return $my_seat;
}

sub answer {
    my $seat_string = $_[0];
    my $row_count = $_[1];
    my $column_count = $_[2];
    my $part = $_[3];

    my $old_seat_string = "";
    my $new_seat_string = $seat_string;

    while (1) {
        $old_seat_string = $new_seat_string;

        my @new_seats = ();
        for my $row (0 .. ($row_count-1)) {
            for my $column (0 .. ($column_count-1)) {
                push(@new_seats, change_state($old_seat_string, $row_count,
                    $column_count, $row, $column, $part));
            }
        }
        $new_seat_string = join('', @new_seats);

        if ($old_seat_string eq $new_seat_string) {
            last;
        }
    }

    # Count occurence of '#' in string
    my $count = ($new_seat_string =~ tr/#//);
    return $count;
}

sub main {
    my @seats = parse_file("input.txt");
    my $row_count = scalar(@seats);
    my $column_count = length($seats[0]);
    my $seat_string = join('', @seats);

    print "Question 1: How many seats end up occupied?\n";
    my $answer1 = answer($seat_string, $row_count, $column_count, 1);
    print "Answer: $answer1\n";
    print "Question 2: How many seats end up occupied?\n";
    my $answer2 = answer($seat_string, $row_count, $column_count, 2);
    print "Answer: $answer2\n";
}

main();
