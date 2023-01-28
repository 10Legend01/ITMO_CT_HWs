#!/bin/bash

bash handler_6.sh &
pidHand=$!
while true
do
	read Line
	case $Line in
	'+') kill -USR1 $pidHand ;;
	'*') kill -USR2 $pidHand ;;
	TERM)
		kill -SIGTERM $pidHand
		exit
		;;
	esac
done
