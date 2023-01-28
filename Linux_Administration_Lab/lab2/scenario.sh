# 1
# fdisk /dev/sdc
# fdisk n -> (type) 'p' -> (number) _ -> (from) _ -> (to) +300M
# May be should +300MB???
# fdisk w

# 2 
sudo blkid -p /dev/sdc1 | grep -wo "UUID=\S*"
# Haven't UUID

# 3
# sudo mkfs.ext4 -b 4096 /dev/sdc1
# 

# 4
sudo dumpe2fs /dev/sdc1

# 5
# sudo tune2fs -c 2 /dev/sdc1

# 6
# sudo mkdir /mnt/newdisk
# sudo mount /dev/sdc1 /mnt/newdisk

# 7
# ln -s /mnt/newdisk/ ~/

# 8 
# sudo mkdir ~/newdisk/test_dir

# 9 
# sudo nano -cl /etc/fstab
# >> # lab 2
# >> /dev/sdc1 /mnt/newdisk ext4 auto,noatime,noexec 0 0
# UUID

# 10
# sudo umount
# sudo parted /dev/sdc
# resizepart 1 350MB -> q
# sudo resize2fs /dev/sdc1
# sudo fdisk /dev/sdc -> print -> q

# 11
# sudo mount -o ro /dev/sdc1 /mnt/newdisk

# 12
# Создание dev/sdc2
# sudo mke2fs -O journal_dev /dev/sdc2
# sudo mke2fs -J device=/dev/sdc2 /dev/sdc1
# WILL BE FORMATTED!!!
# Надо было не заново форматировать, а тюнинговать:
# sudo tune2fs -J device=/dev/sdc2 /dev/sdc1

# 13
# Создание dev/sdc3 и dev/sdc4

# 14
# sudo apt install lvm2
# sudo pvcreate /dev/sdc3 /dev/sdc4
# sudo vgcreate vol_grp1 /dev/sdc3 /dev/sdc4
# sudo lvcreate -L 80M -n logical_vol1 vol_grp1
# sudo mkfs.ext4 /dev/vol_grp1/logical_vol1
# sudo mkdir /mnt/supernewdisk
# sudo mount /dev/vol_grp1/logical_vol1 /mnt/supernewdisk/

# 15
# sudo apt-get install cifs-utils
# sudo mkdir /mnt/share/
# Настроить сетевой мост, найти ip хоста
# sudo mount.cifs //192.168.0.85/Users/Legend/Kubuntu_network /mnt/share -o user=legend

# 16
# sudo nano -cl /root/.smbshare
# >> user=legend
# >> password=...
# sudo nano -cl /etc/fstab
# >> //192.168.0.85/Users/Legend/Kubuntu_network /mnt/share cifs user,rw,credentials=/root/.smbshare 0 0