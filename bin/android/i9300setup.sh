#!/bin/bash

set -eux
#sudo aptitude install debootstrap
sudo debootstrap --arch=armhf --variant=minbase --foreign stable /media/i9300/ http://http.debian.net/debian
cp $(which android_setup_perso.sh) sbin/
cp $(which setup_debian_second_stage.sh) $targetdir/sbin/
read -p "Plug the card into the phone"
echo "Run su -c deb.sh"
echo "then run setup_debian_second_stage.sh"
adb shell
