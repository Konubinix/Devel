#!/system/xbin/env bash

set -u
deb_root="$1"
cur_dir="$(dirname $0)"

CARD_DEVICE=/dev/block/mmcblk1p1

mkdir_maybe ( ) {
    [ -d "$1" ] || mkdir -p "$1"
}

is_mounted ( ) {
    mount | grep -q " $1 "
}

say_and_do ( ) {
    echo "$*"
    "$@"
}

source /system/bin/sdcard1_umount.sh
echo "attempt to check the filesystem"
if test -e "${deb_root}/etc"
then
    echo "No need to check the filesystem"
else
    say_and_do e2fsck "${CARD_DEVICE}"
fi

CHECK_RES="$?"
mkdir_maybe $deb_root
is_mounted $deb_root || \
    mount -text4 -oacl,exec,dev,suid "${CARD_DEVICE}" $deb_root || \
    { echo "failed to mount the card" ; exit 1 ; }

echo "Mounting filesystem"
mkdir_maybe $deb_root/sdcard
is_mounted $deb_root/sdcard || say_and_do mount -o bind /sdcard $deb_root/sdcard

mkdir_maybe $deb_root/data
is_mounted $deb_root/data || say_and_do mount -o bind /data $deb_root/data

mkdir_maybe $deb_root/system
is_mounted $deb_root/system || say_and_do mount -o bind /system $deb_root/system

echo "Mounting special file systems"
is_mounted $deb_root/dev || say_and_do mount -o bind /dev $deb_root/dev
is_mounted $deb_root/dev/pts || say_and_do mount -o bind /dev/pts $deb_root/dev/pts
is_mounted $deb_root/proc || say_and_do mount -t proc proc $deb_root/proc
is_mounted $deb_root/sys || say_and_do mount -t sysfs sysfs $deb_root/sys
cat /proc/mounts > $deb_root/etc/mtab
say_and_do chmod +r $deb_root/etc/mtab
# allow a local user to use screen
/system/bin/chmod 777 $deb_root/dev/pts/*
# allow everyone to use fuse
/system/bin/chmod 777 /dev/fuse
# setup the sound
/system/bin/chmod 777 /dev/snd/*
ln -s /dev/snd/* /dev/

