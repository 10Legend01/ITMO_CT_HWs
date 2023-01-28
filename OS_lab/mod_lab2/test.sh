#!/bin/bash

checkPID(){
	if [[ -z "$(echo $(bash whitebox.sh) | grep -E "(^|[^0-9])+${1}([^0-9]|$)+")" ]]; then
		return 0
	else
		return 1
	fi
}

checkBoxes(){
	Black="$(bash blackbox.sh)"
	for i in $(bash whitebox.sh); do
		if [[ -z "$(echo $Black | grep -E "(^|[^0-9])+${i}([^0-9]|$)+")" ]]; then
			return 0
		fi
	done
	return 1
}

if [[ ! $(bash blackbox.sh) =~ ^[^0-9]*([0-9]+[^0-9]+){2}[0-9]+[^0-9]*$ ]]; then
	echo "Error: blackbox outputs the wrong format."
	exit
fi

bash memory.sh &
MemPid=$!
Old=$(cat /proc/${MemPid}/stat | awk '{ print $24 }')
while true; do
	sleep 0.2
	New=$(cat /proc/${MemPid}/stat | awk '{ print $24 }')
	if (( $Old == $New )); then
		break
	fi
	Old=$New
done

checkPID $MemPid
if (( $? == 0 )); then
	kill $MemPID
	echo "Error: script \"memory.sh\" doesn't work properly."
	exit
fi

checkBoxes
if (( $? == 0 )); then
	kill $MemPid
	echo "Error: blackbox and whitebox do not match."
	exit
fi
kill $MemPid

echo "Blackbox and whitebox are the same."
