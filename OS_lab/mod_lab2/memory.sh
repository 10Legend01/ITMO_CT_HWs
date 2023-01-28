#!/bin/bash

check(){
	if [[ -z "$(echo $(bash whitebox.sh) | grep -E "(^|[^0-9])+${1}([^0-9]|$)+")" ]]; then
		return 0
	else
		return 1
	fi
}

Ma=()
while true; do
	check $$
	if (( $? == 1 )); then
		break
	fi
	for (( i = 0; i < 10000; i++ )); do
		Ma+=(1 2 3 4 5 6 7 8 9 10)
	done
done

while true; do
	sleep 10
done
