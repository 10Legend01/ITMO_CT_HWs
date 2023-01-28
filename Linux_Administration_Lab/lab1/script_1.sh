#!/bin/bash

set -e

cd ~

#1
mkdir -p test
cd test/

#2
ls -1aAp /etc/ > list

#3
ls -1aAd /etc/*/ | wc -l >> list
find /etc/ -maxdepth 1 -name ".*" | wc -l >> list

#4
mkdir -p links
cd links/

#5
ln -P ../list list_hlink

#6
ln -s ../list list_slink

#7
stat --format=%h list_hlink ../list list_slink
# ls -l list_hlink | awk {'print $2'}
# ls -l ../list | awk {'print $2'}
# ls -l list_slink | awk {'print $2'}

#8
cat ../list | wc -l >> list_hlink

#9
cmp -s list_hlink list_slink && echo YES

#10
mv ../list ../list1

#11
cmp -s list_hlink list_slink && echo YES

#12
cd ..
ln -d ./links ~/links || true
# ln -s ./links ~/links
# https://askubuntu.com/questions/210741/why-are-hard-links-not-allowed-for-directories

#13
cd ~
sudo find /etc/ -name "*.conf" > list_conf

#14
ls -1aAd /etc/*.d/ > list_d

#15
cat list_conf > list_conf_d
cat list_d >> list_conf_d

#16
mkdir ~/test/.sub/

#17
cp list_conf_d ~/test/.sub/

#18
cp -b list_conf_d ~/test/.sub/

#19
find ~/test/ -type f,l

#20
man man > man.txt

#21
mkdir -p ~/man_split
split -b 1KB -d ~/man.txt ~/man_split/man_

#22
mkdir -p ~/test/man.dir/

#23
cp ~/man_split/* ~/test/man.dir/

#24
cat ~/test/man.dir/* > ~/test/man.dir/man.txt

#25
cmp -s ~/man.txt ~/test/man.dir/man.txt && echo YES

#26
cd ~
echo -e "qwe\nrty\nuio\n$(cat man.txt)\nasd\nfgh\njkl" > man.txt

#27
diff ./test/man.dir/man.txt man.txt > man.patch

#28
mv man.patch ./test/man.dir/

#29
cd ./test/man.dir/
patch man.txt -i man.patch

#30
cmp -s ~/man.txt ~/test/man.dir/man.txt && echo YES
