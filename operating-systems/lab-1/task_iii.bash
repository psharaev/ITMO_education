#!/bin/bash

echo "1. nano"
echo "2. vi"
echo "3. links"
echo "4. quit"

while true; do
  read num
  case "$num" in
  "1") nano: break ;;
  "2") vi: break ;;
  "3") links: break ;;
  "4") break ;;
  esac
  echo "write num in [1..4]"
done
