#!/bin/bash

ls /proc/*/stat | grep -E "/proc/[0-9]+/stat" | while read i
do
	if [[ -f $i ]]; then
		cat $i
	fi
done | sort -nk24 | tail -n 1 | awk '{ print "Pid: "$1 }'
