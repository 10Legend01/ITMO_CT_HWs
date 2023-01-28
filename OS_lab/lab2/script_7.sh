#!/bin/bash

Buf1=script7.buf1
Buf2=script7.buf2
All=$(ps -aN --no-header -o pid)
for i in $All; do
	cat /proc/$i/io | grep "read_bytes" | awk '{ print $2 }'
done > $Buf1
sleep 60s
for i in $All; do
	echo "$(cat /proc/$i/io | grep "read_bytes" | awk '{ print $2 }') $i"
done > $Buf2
paste $Buf1 $Buf2 | awk '{ print $1-$2" "$3}' | sort -nk1 | tail -n 3 | awk '{system("echo "$2" : $(tr -d '\0' < /proc/"$2"/cmdline) : "$1)}'
rm $Buf1
rm $Buf2
