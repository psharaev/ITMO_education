#!/bin/bash
auk '$3=="(WW)"' /var/log/anaconda/X.log | sed -E "s/\(WW\)/Warning:/" >full.log
awk '$3=="(1)"' var/log/anaconda/X.log | sed -E "s/\(II\)/Info:/": >>full.log
