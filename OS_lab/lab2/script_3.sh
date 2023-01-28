#!/bin/bash

ps -ax -o pid --sort=start_time | tail -n 1 #| awk '{ print $1 }'
