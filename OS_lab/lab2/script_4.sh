#!/bin/bash

File=ans_scr4.txt
All=$(ps -aN --no-header -o pid)
for i in $All
do
	CPPid=$(cat /proc/$i/status | grep "^PPid:" | awk '{ print $2 }')
	Sum=$(cat /proc/$i/sched | grep -E "^.*sum_exec_runtime" | awk '{ print $3 }')
	Nr=$(cat /proc/$i/sched | grep -E "^nr_switches" | awk '{ print $3 }')
	CArt=$(echo "$Sum / $Nr" | bc -l)
	echo "$i $CPPid $CArt"
done | sort -nk2 -nk1 | awk '{ print "ProcessID="$1" : Parent_ProcessID="$2" : Average_Running_Time="$3 }' > $File
less $File
