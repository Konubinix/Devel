#!/bin/bash

set -eu
targetdir=rootfs
distro=testing

mkdir -p $targetdir
debootstrap --arch=armhf --foreign $distro $targetdir

cp /usr/bin/qemu-arm-static $targetdir/usr/bin/
cp /etc/resolv.conf $targetdir/etc
rm -r $targetdir/etc/mtab
ln -s /proc/mount $targetdir/etc/mtab

setup_debian_fs_setup.sh

chroot $targetdir /bin/addgroup --gid 3003 aid_net
chroot $targetdir /bin/addgroup --gid 1015 sdcard-rw
chroot $targetdir /bin/adduser --uid 10054 --gid 10054 ${USERNAME}
chroot $targetdir /bin/adduser ${USERNAME} disk
chroot $targetdir /bin/adduser ${USERNAME} adm
chroot $targetdir /bin/adduser ${USERNAME} sudo
chroot $targetdir /bin/adduser ${USERNAME} aid_net
chroot $targetdir /bin/adduser ${USERNAME} sdcard-rw

mkdir $targetdir/media/host
mount --bind / $targetdir/media/host

cp $(which android_setup_perso.sh) $targetdir/deboostrap/
echo "Now take a loook at setup_debian_second_stage.sh"
chroot $targetdir
