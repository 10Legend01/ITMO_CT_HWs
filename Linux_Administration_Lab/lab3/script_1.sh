#!/bin/bash

set -e

# cd ~

#1
cat /etc/passwd | awk -F ":" '{ print "user "$1" has id "$3 }' > work3.log

#2
sudo chage -l root | head -n 1 >> work3.log

#3
cat /etc/group | awk -F ":" '{print $1}' | paste -s -d ',' | sed "s/,/, /g" >> work3.log

#4
sudo echo "Be careful!" > readme.txt
sudo mv readme.txt /etc/skel/readme.txt

#5
#sudo useradd -m -p 12345678 u1
#sudo useradd -m -p $(openssl passwd –crypt 12345678) u1
sudo useradd -m -p $(perl -e 'print crypt($ARGV[0], "password")' 12345678) u1
# https://losst.pro/kak-sozdat-polzovatelya-linux#comment-56915

#6
sudo groupadd g1

#7
sudo usermod -a -G g1 u1

#8
id u1 >> work3.log

#9
sudo usermod -a -G g1 legend

#10
cat /etc/group | grep "^g1" | awk -F ":" '{print $4}' >> work3.log

#11
# sudo apt install mc
sudo usermod -s /usr/bin/mc u1

#12
sudo useradd -m -p $(perl -e 'print crypt($ARGV[0], "password")' 87654321) u2

#13
sudo mkdir -p /home/test13
sudo cp work3.log /home/test13/work3-1.log
sudo cp work3.log /home/test13/work3-2.log

#14
# Добавим пользователя u2 к группе u1
# И дадим права пользователю u1 и группе u2
#sudo usermod -a -G u1 u2
#sudo chown -R u1:u1 /home/test13

# Либо попробуем отдать права пользователю u1 и группе u2
sudo chown -R u1:u2 /home/test13

sudo chmod -R u+rwt,g+r,g-w,o-rw,a-x /home/test13
sudo chmod ug+x /home/test13

#15
sudo mkdir -p /home/test14
sudo chown -R u1:u1 /home/test14

sudo chmod -R a+rw,a+t,u-t /home/test14

#16
cp /bin/nano /home/test14/
sudo chmod a+s /home/test14/nano
# не уверен насчет безопасности, но пункт выполняет

#17
sudo mkdir -p /home/test15
sudo touch /home/test15/secret_file
sudo chmod a+r,a-wx /home/test15/secret_file
sudo chmod a-rw,a+x /home/test15
