#!/bin/bash

TILELD="1 4 8 12"
ROWS="100 500 1000"

BURST_SIZE_INTS=96
COL_BURSTS="10 20 50"

PAR="8 24 48"

## Generate per-benchmark results CSV
resultFiles=""
for numReader in $TILELD; do
  for numRows in $ROWS; do
    for numColBursts in $COL_BURSTS; do
      numCols=$(($numColBursts*$BURST_SIZE_INTS))
      for par in $PAR; do
        OUT_DIR=${numReader}_${numRows}_${numCols}_${par}
        echo "Processing $OUT_DIR"
        ./res $OUT_DIR
        resultFiles+=" summaries/$OUT_DIR/results.csv"
      done
    done
  done
done

## Combine all CSVs into one
./multi_join.sh $resultFiles > results.csv
