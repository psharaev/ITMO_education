#!/bin/bash

printf "%s\n" "$@" | sort -n | tail -1
