#!/bin/bash
if [ $# -ne 2 ]; then
  echo "Usage: $0 <summary_file> <output_csv_file>"
  exit -1
fi

SUMMARY_FILE=$1
CSV_FILE=$2

## Courtesy stackoverflow: http://stackoverflow.com/questions/965053/extract-filename-and-extension-in-bash
FILENAME=$(basename "$SUMMARY_FILE")
EXT="${FILENAME##*.}"
NAME="${FILENAME%.*}"

READERS=`echo $NAME | cut -f1 -d'_'`
ROWS=`echo $NAME | cut -f2 -d'_'`
COLS=`echo $NAME | cut -f3 -d'_'`
PAR=`echo $NAME | cut -f4 -d'_'`
echo "\"Readers\", $READERS" >> $CSV_FILE
echo "\"Rows\", $ROWS" >> $CSV_FILE
echo "\"Cols\", $COLS" >> $CSV_FILE
echo "\"Par\", $PAR" >> $CSV_FILE

## ALMs
echo "ALMs," >> $CSV_FILE
v=`grep "\[a\] ALMs used for LUT logic and registers" $SUMMARY_FILE  | cut -f3 -d';' | sed s/,//g | tr -d '[[:space:]]'`
echo "\"Register & Logic\", $v" >> $CSV_FILE
v=`grep "\[b\] ALMs used for LUT logic" $SUMMARY_FILE | cut -f3 -d';' | sed s/,//g | tr -d '[[:space:]]'`
echo "\"Logic Only\", $v" >> $CSV_FILE
v=`grep "\[c\] ALMs used for registers" $SUMMARY_FILE | cut -f3 -d';' | sed s/,//g | tr -d '[[:space:]]'`
echo "\"Register Only\", $v" >> $CSV_FILE
v=`grep "\[d\] ALMs used for memory (up to half of total ALMs)" $SUMMARY_FILE | cut -f3 -d';' | sed s/,//g | tr -d '[[:space:]]'`
echo "\"Mem\", $v" >> $CSV_FILE
echo "\"Logic Subtotal\"," >> $CSV_FILE
echo "\"Register Subtotal\"," >> $CSV_FILE
echo "\"Placement Total\"," >> $CSV_FILE
v=`grep  "\[B\] Estimate of ALMs recoverable by dense packing" $SUMMARY_FILE | cut -f3 -d';' | cut -f1 -d'/' | sed s/,//g | tr -d '[[:space:]]'`
echo "Recoverable, $v" >> $CSV_FILE
v=`grep  "\[C\] Estimate of ALMs unavailable \[=a+b+c+d\]" $SUMMARY_FILE | cut -f3 -d';' | cut -f1 -d'/' | sed s/,//g | tr -d '[[:space:]]'`
echo "Unavailable, $v" >> $CSV_FILE
echo "\"Needed\"," >> $CSV_FILE

## LUTs
echo "ALMs," >> $CSV_FILE
v=`grep  " 7 input functions" $SUMMARY_FILE | cut -f3 -d';' | sed s/,//g | tr -d '[[:space:]]'`
echo "\"7 inputs\", $v" >> $CSV_FILE
v=`grep  " 6 input functions" $SUMMARY_FILE | cut -f3 -d';' | sed s/,//g | tr -d '[[:space:]]'`
echo "\"6 inputs\", $v" >> $CSV_FILE
v=`grep  " 5 input functions" $SUMMARY_FILE | cut -f3 -d';' | sed s/,//g | tr -d '[[:space:]]'`
echo "\"5 inputs\", $v" >> $CSV_FILE
v=`grep  " 4 input functions" $SUMMARY_FILE | cut -f3 -d';' | sed s/,//g | tr -d '[[:space:]]'`
echo "\"4 inputs\", $v" >> $CSV_FILE
v=`grep  " <=3 input functions" $SUMMARY_FILE | cut -f3 -d';' | sed s/,//g | tr -d '[[:space:]]'`
echo "\"<= 3 inputs\", $v" >> $CSV_FILE
echo "\"Logic Subtotal\"," >> $CSV_FILE
v=`grep  "Combinational ALUT usage for route-throughs" $SUMMARY_FILE | cut -f3 -d';' | sed s/,//g | tr -d '[[:space:]]'`
echo "\"Route-Thru\", $v" >> $CSV_FILE
v=`grep  " 64-address deep" $SUMMARY_FILE | cut -f3 -d';' | sed s/,//g | tr -d '[[:space:]]'`
echo "\"64 Address Mems\", $v" >> $CSV_FILE
v=`grep  " 32-address deep" $SUMMARY_FILE | cut -f3 -d';' | sed s/,//g | tr -d '[[:space:]]'`
echo "\"32 Address Mems\", $v" >> $CSV_FILE
v=`grep  " 16-address deep" $SUMMARY_FILE | cut -f3 -d';' | sed s/,//g | tr -d '[[:space:]]'`
echo "\"16 Address Mems\", $v" >> $CSV_FILE
echo "\"Mem Subtotal\"," >> $CSV_FILE
echo "\"Total\"," >> $CSV_FILE

## Registers
echo "\"Registers\"," >> $CSV_FILE
v=`grep  " Design implementation registers" $SUMMARY_FILE | cut -f3 -d';' | sed s/,//g | tr -d '[[:space:]]'`
echo "\"Implementation\", $v" >> $CSV_FILE
v=`grep  " Routing optimization registers" $SUMMARY_FILE | cut -f3 -d';' | sed s/,//g | tr -d '[[:space:]]'`
echo "\"Routing\", $v" >> $CSV_FILE
echo "\"Total\"," >> $CSV_FILE

## DSPs
v=`grep  "Total DSP Blocks" $SUMMARY_FILE | cut -f3 -d';' | cut -f1 -d'/' | sed s/,//g | tr -d '[[:space:]]'`
echo "\"DSPs\", $v" >> $CSV_FILE

## BRAM
v=`grep  "M20K blocks" $SUMMARY_FILE | cut -f3 -d';' | cut -f1 -d'/' | sed s/,//g | tr -d '[[:space:]]'`
echo "\"BRAM\", $v" >> $CSV_FILE
