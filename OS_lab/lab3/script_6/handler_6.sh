#!/bin/bash

Ans=1
Mode="null"

usr1(){ Mode="+"; }
usr2(){ Mode="*"; }
sigterm(){ Mode="sigterm"; }

trap "usr1" USR1
trap "usr2" USR2
trap "sigterm" SIGTERM

while true
do
	case $Mode in
	'+' | '*')
		Ans=$(( $Ans $Mode 2 ))
		echo "Current answer = $Ans"
		;;
	sigterm)
		echo "Handler terminated"
		exit
		;;
	esac
	sleep 1
done
