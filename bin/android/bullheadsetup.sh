#!/bin/bash

set -eux
#sudo aptitude install debootstrap
arch=${1:-arm64}

# testing won't work because of resolv.conf problems
sudo debootstrap --arch=${arch} --variant=minbase --foreign stable debian-${arch} http://httpredir.debian.org/debian
sudo cp $(which android_setup_perso.sh) "debian-${arch}/sbin/"
sudo cp $(which setup_debian_second_stage.sh) "debian-${arch}/sbin/"
sudo tar cf debian-${arch}.tar debian-${arch}
adb push debian-${arch}.tar /sdcard/
adb shell su -c 'rm -rf /data/debian* && cp /sdcard/debian-${arch}.tar /data/debian-${arch}.tar && cd /data && tar xf debian-${arch}.tar && rm debian-${arch}.tar && mv debian-${arch} debian'
echo "run su -c deb.sh, then setup_debian_second_stage.sh, then reboot the phone"
adb shell
