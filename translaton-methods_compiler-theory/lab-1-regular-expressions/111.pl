#!/usr/bin/perl
use strict;
use warnings FATAL => 'all';

while (<>) {
    print if /^(0+|(1(01*0)*1)+)+$/;
}