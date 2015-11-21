#!/bin/bash

set -eu
targetdir=rootfs
distro=testing

mkdir -p $targetdir
debootstrap --arch=armhf --foreign $distro $targetdir

cp /usr/bin/qemu-arm-static $targetdir/usr/bin/
cp /etc/resolv.conf $targetdir/etc
rm -rf $targetdir/etc/mtab
ln -s /proc/mount $targetdir/etc/mtab

setup_debian_fs_setup.sh

cp $(which android_setup_perso.sh) $targetdir/sbin/
cp $(which setup_debian_second_stage.sh) $targetdir/sbin/
echo "Now take a look at setup_debian_second_stage.sh"
chroot $targetdir
