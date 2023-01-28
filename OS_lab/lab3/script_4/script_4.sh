#!/bin/bash

Task=infinity.sh
bash $Task &
PidFirst=$!
bash $Task &
bash $Task &
PidThird=$!
cpulimit -l 10 -p $PidFirst &
kill $PidThird
