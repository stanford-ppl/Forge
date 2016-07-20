#!/bin/bash

if [ $# -ne 1 ]; then
  echo "Usage: $0 <dir>"
  exit -1
fi

DIR=$1
NUMTRIES=5

pushd $DIR >> log.log
maxforceidle -a >> log.log 2>&1
./unpack.sh >> log.log 2>&1

sum=0
for i in `seq 1 $NUMTRIES`; do
  t=`bash run_fpga.sh 384000 2>&1 | grep "Kernel done, elapsed time" | cut -f2 -d'='`
  sum=$(echo "scale=8; $sum+$t" | bc)
done

avg=$(echo "scale=8; $sum/$NUMTRIES" | bc) 
echo "$DIR, $avg"

f=`find $d -iname Top.max`
rm -f $f >> log.log 2>&1
make fpga_clean >> log.log 2>&1
popd >> log.log
 
