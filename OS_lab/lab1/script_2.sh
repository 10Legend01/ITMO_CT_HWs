#!/bin/bash

STR=""
while true
do
read INP
if [[ $INP == "q" ]]
then
	break
fi
STR=$STR$INP
done
echo $STR
