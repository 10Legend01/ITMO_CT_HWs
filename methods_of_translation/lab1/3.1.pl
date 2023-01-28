#!/usr/bin/perl
# use strict;
# use warnings FATAL => 'all';

$haveSpace = 0;
$start = 1;
while (<>) {
    if (/^\s*$/) {
        if ($start == 0) {
            $haveSpace = 1;
        }
    } else {
        $start = 0;
        if ($haveSpace == 1) {
            $haveSpace = 0;
            print "\n";
        }
        s/^\s+//g;
        s/\s+$//g;
        s/\s+/ /g;
        print;
        print "\n";
    }
}