#!/bin/bash

File=ans_scr1.txt
Reg="^[[:space:]]*user"
Ans=$(ps -u | grep -E "$Reg")
if (( ${#Ans} != 0 )); then
	echo "$Ans" | wc -l > $File
	echo "$Ans" | awk '{ print $2":"$11 }' >> $File
else
	echo 0 > $File
fi
