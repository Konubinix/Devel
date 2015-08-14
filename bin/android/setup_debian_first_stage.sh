#!/bin/bash

set -eu
targetdir=rootfs
distro=testing

mkdir -p $targetdir
sudo debootstrap --arch=armhf --foreign $distro $targetdir

sudo cp /usr/bin/qemu-arm-static $targetdir/usr/bin/
sudo cp /etc/resolv.conf $targetdir/etc

echo "Now take a loook at setup_debian_second_stage.sh"

sudo chroot $targetdir
