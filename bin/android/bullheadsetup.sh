#!/bin/bash

set -eux
sudo aptitude install debootstrap

sudo debootstrap --arch=armhf --variant=minbase --foreign stable debian http://httpredir.debian.org/debian
sudo cp $(which android_setup_perso.sh) "debian/sbin/"
sudo cp $(which setup_debian_second_stage.sh) "debian/sbin/"
sudo tar cf debian.tar debian
adb push debian.tar /sdcard/
adb shell su -c 'rm -rf /data/debian* && mv /sdcard/debian.tar /data && cd /data && tar xf debian.tar && rm debian.tar'
adb shell su -c 'mount -oremount,exec,dev /data'
echo "run su -c deb.sh, then setup_debian_second_stage.sh, then reboot the phone"
adb shell
