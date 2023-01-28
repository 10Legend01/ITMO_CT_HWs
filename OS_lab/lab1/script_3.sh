#!/bin/bash

#STR=""
while true
do
clear
echo -e "Program menu\nPlease, enter your choice:\n1 - \"nano\";\n2 - \"vi\";\n3 - \"links\";\n4 - exit."
read -n 1 INP
case $INP in
1 )
nano
;;
2 )
vi
;;
3 )
links
;;
4 )
break
;;
esac
done
clear
