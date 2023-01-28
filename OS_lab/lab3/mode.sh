#!/bin/bash

if (( $# != 2 )); then
	echo "Usage: mode.sh time size"
	exit
fi
if (( $1 <= 0 )); then
	echo "Error. Please, enter time number greater than 0."
	exit
fi
if (( $2 <= 0 || $2 >= 32768 )); then
	echo "Error. Please, enter size number greater than 0 and less then 32768."
	exit
fi
Size=$2

bash proc_mode.sh plus $Size &
sleep 0.01
kill -SIGUSR2 $!
sleep 0.01

for File in $(seq $Size); do
	echo 0 > /dev/shm/$File
done

kill -SIGUSR1 $!
sleep $1

kill -SIGTERM $!
sleep 0.1

for File in $(seq $Size); do
	echo $(cat /dev/shm/$File)
	rm /dev/shm/$File
done | less
