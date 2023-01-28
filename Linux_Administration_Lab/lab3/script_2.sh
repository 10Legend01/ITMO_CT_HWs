#!/bin/bash

# set -e

# cd ~

sudo userdel u1
sudo userdel u2

sudo groupdel g1

sudo rm work3.log
sudo rm readme.txt
sudo rm /etc/skel/readme.txt

sudo rm -rf /home/u1
sudo rm -rf /home/u2
sudo rm -rf /home/test13
sudo rm -rf /home/test14
sudo rm -rf /home/test15
