#!/bin/bash

mkfifo pipe 2>/dev/null
bash handler_5.sh $$ &
while true; do
	read Line
	echo "$Line" > pipe
done
