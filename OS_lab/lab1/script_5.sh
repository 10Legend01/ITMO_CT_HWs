#!/bin/bash

#cat /var/log/anaconda/syslog | grep -E "[0-2][0-4]:[0-9][0-9]:[0-9][0-9],[0-9][0-9][0-9] INFO " > info.log
cat /var/log/anaconda/syslog | grep -E "^.* INFO " > info.log
#less info.log
