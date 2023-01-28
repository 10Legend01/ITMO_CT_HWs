#!/bin/bash

mkdir ~/test && {
	echo "catalog test was created successfully" >> ~/report
	> ~/test/$(date +%F_%T)
}
ping -c 1 www.net_nikogo.ru || echo $(date +%F_%T) "server not found" >> ~/report
