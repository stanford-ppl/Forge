#!/bin/bash

CSV_FILE=time_results.csv

echo "Name, Avg time" >> $CSV_FILE

TILELD="1 4 8 12"
ROWS="100 500 1000"

BURST_SIZE_INTS=96
COL_BURSTS="10 20 50"

PAR="8 24 48"

for numReader in $TILELD; do
  for numRows in $ROWS; do
    for numColBursts in $COL_BURSTS; do
      numCols=$(($numColBursts*$BURST_SIZE_INTS))
      for par in $PAR; do
        OUT_DIR=${numReader}_${numRows}_${numCols}_${par}
	echo $OUT_DIR
        bash runBench.sh $OUT_DIR >> $CSV_FILE
      done
    done
  done
done
