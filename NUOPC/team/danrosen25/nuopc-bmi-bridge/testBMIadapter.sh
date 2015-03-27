#!/bin/bash

count=0

function TestProto {
((count++))
testList[count]=$1
echo ---------------------------------------------------------------------------
echo STARTING: $1
cd $1
gmake distclean
gmake
mpirun -np 4 ./$2
if [ $? -eq 0 ]
then
testResult[count]="PASS"
else
testResult[count]="FAIL"
fi
echo FINISHED: $1
echo ---------------------------------------------------------------------------
echo
}

TestProto . mainApp.exe

i=1
while [[ $i -le $count ]]
do
echo ${testResult[i]}: ${testList[i]}
((i++))
done
