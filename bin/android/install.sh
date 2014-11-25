#!/bin/bash

# see /home/slo/perso/perso/bin/debinstall.sh

mount -oremount,rw /system
cp /sdcard/{debbootstrap.sh,deb.sh,launch_ssh.sh,undeb.sh,konix_start_ssh_maybe.sh,debsetup.sh,sdcard1_umount.sh} /system/bin/
chmod 777 /system/bin/{debbootstrap.sh,deb.sh,launch_ssh.sh,undeb.sh,konix_start_ssh_maybe.sh,debsetup.sh,sdcard1_umount.sh}
rm /sdcard/{debbootstrap.sh,deb.sh,launch_ssh.sh,undeb.sh,konix_start_ssh_maybe.sh,install.sh,debsetup.sh,sdcard1_umount.sh}
