#!/bin/bash

if (( $(echo "$1 >= $2" | bc -l) ))
then
	if (( $(echo "$1 >= $3" | bc -l) ))
	#if (( $1 >= $3 ))
	then
		echo "$1"
	else
		echo "$3"
	fi
else
	if (( $(echo "$2 >= $3" | bc -l) ))
	#if (( $2 >= $3 ))
	then
		echo "$2"
	else
		echo "$3"
	fi
fi
