#!/bin/bash

# see /home/slo/perso/perso/bin/debinstall.sh

mount -oremount,rw /system
cp /sdcard/{deb.sh,launch_ssh.sh,konix_start_ssh_maybe.sh,debsetup.sh,sdcard1_umount.sh} /system/bin/
chmod 777 /system/bin/{deb.sh,launch_ssh.sh,konix_start_ssh_maybe.sh,debsetup.sh,sdcard1_umount.sh}
rm /sdcard/{deb.sh,launch_ssh.sh,konix_start_ssh_maybe.sh,install.sh,debsetup.sh,sdcard1_umount.sh}
