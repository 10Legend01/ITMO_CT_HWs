#!/bin/bash

File=ans_scr2.txt
Reg="^[[:space:]]*[0-9]+ /sbin/"
ps -x -o pid,args | grep -E "$Reg" | awk '{ print $1 }' > $File
cat $File
