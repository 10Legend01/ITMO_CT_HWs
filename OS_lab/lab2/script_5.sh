#!/bin/bash

File=ans_scr5.txt
Read=ans_scr4.txt
sed -n 1p $Read > $File
CurrPar=$(sed -n 1p $Read | awk '{ print $3 }')
ART=$(sed -n 1p $Read | awk -F'=' '{ print $4 }')
Sz=1
sed 1d $Read | sed '$G' | while read i
do
	if [[ -z "$i" ]]; then
		NumPPid=$(echo "$CurrPar" | awk -F"=" '{ print $2 }')
		NewART=$(echo "$ART / $Sz" | bc -l)
		echo "Average_running_Children_of_ParentID=$NumPPid is $NewART" >> $File
		break
	fi
	PPid=$(echo $i | awk '{ print $3 }')
	if [[ $PPid != $CurrPar ]]; then
		NumPPid=$(echo "$CurrPar" | awk -F"=" '{ print $2 }')
		CurrPar="$PPid"
		NewART=$(echo "$ART / $Sz" | bc -l)
		echo "Average_running_Children_of_ParentID=$NumPPid is $NewART" >> $File
		ART=0
		Sz=0
	fi
	(( Sz=Sz+1 ))
	ART=$(echo "$ART + $(echo $i | awk -F'=' '{ print $4 }')" | bc -l)
	echo $i >> $File
done
less $File
