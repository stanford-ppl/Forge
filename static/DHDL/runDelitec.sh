#!/bin/bash

if [ $# -ne 1 ]; then
  echo "Usage: $0 <args>"
  exit -1
fi

bin/delitec --cpp --maxj  $1
