#!/usr/bin/perl
use strict;
use warnings FATAL => 'all';
while (<>) {
    print if /.*[xyz].{5}(.{1})?(.{2})?(.{3})?(.{4})?(.{2})?[xyz].*/;
}