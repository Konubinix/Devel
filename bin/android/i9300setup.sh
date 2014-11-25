#!/bin/bash

sudo debootstrap --arch=armhf --variant=minbase --foreign testing /media/i9300/ http://http.debian.net/debian
read -p "Plug the card into the phone"
adb shell su -c deb.sh debbootstrap.sh
