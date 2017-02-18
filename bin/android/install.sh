#!/bin/bash

# see /home/slo/perso/perso/bin/debinstall.sh

target="${1:-i9300}"

mount -oremount,rw /system
for file in launch_ssh.sh konix_start_ssh_maybe.sh deb.sh
do
	cp /sdcard/${file} /system/bin/
	chmod 777 /system/bin/${file}
	rm /sdcard/${file}
done
for file in debsetup.sh #sdcard1_umount.sh
do
	cp /sdcard/${target}${file} /system/bin/${file}
	chmod 777 /system/bin/${file}
	rm /sdcard/${target}${file}
done
rm /sdcard/install.sh
