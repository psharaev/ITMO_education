#!/usr/bin/perl
use strict;
use warnings FATAL => 'all';

while (<>) {
    s/(\b)(\w)(\w)(\w*\b)/$1$3$2$4/g;
    print;
}