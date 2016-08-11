#!/bin/bash

if [ $# -ne 1 ]; then
  echo "Usage: $0 <args>"
  exit -1
fi

bin/delitec --cpp --maxj --dump_exception --noDSE --noPIR $1
