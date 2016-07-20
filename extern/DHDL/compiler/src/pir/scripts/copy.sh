#!/bin/bash

if [ $# -ne 1 ]; then
  echo "Usage: $0 <dir>"
  exit -1
fi

DIRNAME=$1
FILENAME=$(basename "$1")
EXT="${FILENAME##*.}"
NAME="${FILENAME%.*}"

## Compress the Top file before copying
tar -cvzf $DIRNAME/RunRules/DFE/maxfiles/Top.tar.gz -C $DIRNAME/RunRules/DFE/maxfiles Top.max

## Copy files over
rsync -avz \
  -e "ssh -p 3033" \
  --include CPUCode \
  --include EngineCode \
  --exclude Top_MAIA_DFE/ \
  --exclude binaries/ \
  --exclude objects/ \
  --exclude Top.max \
  $DIRNAME raghup17@portal.maxeler.com:~/isca16/tilest


