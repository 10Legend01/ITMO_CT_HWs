#!/usr/bin/perl

use strict;
use warnings FATAL => 'all';
while (<>) {
    # print if /^(0|1)*(00|11)(0|1)*$/ ;
    # print "Yes\n" if /^(0|1)*(00|11)(0|1)*$/ ;
    print if /^.*cat.*cat.*$/ ;

}