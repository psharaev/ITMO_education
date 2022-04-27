#!/bin/bash
man bash | grep -o -i "[a-A-Z]\{4,\}" | sort | uniq -c | sort -r -n | head -3
