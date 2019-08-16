#!/bin/bash
set -e

base=`basename $2 ".f90"`
FC=gfortran
$FC $2 -o $2.out
echo "Generating fortran output"
$2.out < ./input/$base.in > $2.fort.out
rm $2.out
echo "Generating f19 output"
$1 $2 -o $2.out
$2.out < ./input/$base.in > $2.fc.out
$4 $2.fort.out $2.fc.out
OUT=$?
if [ "$OUT" != "0" ]; then
echo "output miss match $OUT"
exit 1
fi
rm $2.fort.out $2.fc.out
rm  $2.out
