#!/usr/bin/perl
use strict;
use warnings FATAL => 'all';
while (<>) {
    s/(a.*?aa.*?aa.*?a)/bad/g;
    print;
}