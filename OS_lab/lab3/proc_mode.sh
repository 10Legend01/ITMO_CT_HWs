#!/bin/bash

Run="on"
Mode=$1
Size=$2
if (( $# == 3 )); then
	PartnerPID=$3
fi

off(){ Run="off"; }
plus(){
	if [[ $Run == "on" ]]; then
		File=/dev/shm/$(( $RANDOM % $Size + 1 ))
		Num=$(cat $File)
		echo $(( $Num + 1 )) > $File
		kill -USR1 $PartnerPID
	else
		kill -SIGTERM $PartnerPID
		exit
	fi
}
subtruct(){
	File=/dev/shm/$(( $RANDOM % $Size + 1))
	Num=$(cat $File)
	echo $(( $Num - 1 )) > $File
	kill -USR1 $PartnerPID
}
partner(){
	bash proc_mode.sh subtr $Size $$ &
	PartnerPID=$!
}

trap "if [[ '$Mode' == 'plus' ]]; then plus; else subtruct; fi" USR1
trap "partner" USR2
trap "off" SIGTERM

while true; do
	if [[ $Run == "off" ]]; then
		exit
	fi
	sleep 0.0001
done
