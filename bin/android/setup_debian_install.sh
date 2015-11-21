#!/bin/bash

set -x
dd if=/dev/zero of=/dev/sdb2 bs=1M
set -eu
mkfs.ext3 /dev/sdb2
mkdir -p rootfs_device
mount /dev/sdb2 rootfs_device
mkdir -p rootfs_device/proc rootfs_device/dev rootfs_device/sys
rsync -aPz --numeric-ids --exclude='/dev' --exclude='/proc' --exclude='/sys' rootfs/ rootfs_device/
umount rootfs_device
