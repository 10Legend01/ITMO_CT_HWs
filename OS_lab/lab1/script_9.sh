#!/bin/bash

wc -l /var/log/*.log | tail -n 1 | awk '{ print $1 }'
