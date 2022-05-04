#!/bin/bash

if [[ "$HOME" == "$PWD" ]]; then
  echo "$HOME"
else
  echo "Not home directory"
  exit 1
fi
