#!/bin/bash

Reg="[a-zA-Z0-9_\-\.]+@[a-zA-Z0-9_\-]+\.[a-z]+"
#grep -E -o -r "$Reg" /etc/ | grep -E -o "$Reg" | tr '\n' ',' | sed -E "s/,$//" | sed -E "s/,/, /g" > emails.lst
grep -E -o -r -h -a "$Reg" /etc/ | sort | uniq | tr '\n' ',' | sed -E "s/,/, /g" > emails.lst
#less emails.lst
