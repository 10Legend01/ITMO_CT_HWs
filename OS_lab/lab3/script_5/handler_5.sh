#!/bin/bash

Ans=1
Mode="+"
echo "Current answer = $Ans"
(tail -f pipe) |
while true
do
	read Line
	if [[ "$Line" == "QUIT" ]]; then
		kill $(pgrep -P $$ tail)
		exit
	elif [[ "$Line" == "+" || "$Line" == "*" ]]; then
		Mode="$Line"
	elif [[ "$Line" =~ ^-?[0-9]+$ ]]; then
		Ans=$(($Ans $Mode $Line))
		echo "Current answer = $Ans"
	else
		echo "Error: unknown input: \"$Line\""
		kill $(pgrep -P $$ tail)
		exit
	fi
done
kill $1
rm pipe
