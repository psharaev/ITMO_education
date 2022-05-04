#!/bin/bash

res=""
while true; do
  read s
  if [[ "$s" == "q" ]]; then
    break
  else
    res+="$s"
  fi
done
echo "$res"
