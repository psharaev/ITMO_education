#!/usr/bin/perl
use strict;
use warnings FATAL => 'all';

while (<>) {
    s/(\b\w+\b)(\W+)(\b\w+\b)/$3$2$1/;
    print;
}