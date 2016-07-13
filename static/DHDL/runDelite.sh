#!/bin/bash
if [ $# -ne 1 ]; then
  echo "Usage: $0 <args>"
  exit -1
fi

bin/delite --no-jvm --no-compile --cpp=1 --maxj=1 --codecache=`pwd`/out $1
