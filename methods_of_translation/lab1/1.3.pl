#!/usr/bin/perl
use strict;
use warnings FATAL => 'all';
while (<>) {
    print if /.*[cC][aA][tT].*/;
}