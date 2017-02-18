#!/system/bin/env bash

set -eux
deb_root="$1"
cur_dir="$(dirname $0)"

image="/data/debian.img"

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
#/system/bin/chmod 777 /dev/snd/*
#ln -s /dev/snd/* /dev/
#ln -s /proc/self/fd /dev/fd
