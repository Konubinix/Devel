#!/bin/bash

set -x
sudo dd if=/dev/zero of=/dev/sdb2 bs=1M
set -eu
sudo mkfs.ext3 /dev/sdb2
sudo mkdir -p rootfs_device
sudo mount /dev/sdb2 rootfs_device
sudo mkdir -p rootfs_device/proc rootfs_device/dev rootfs_device/sys
sudo rsync -aPz --numeric-ids --exclude='/dev' --exclude='/proc' --exclude='/sys' rootfs/ rootfs_device/
sudo umount rootfs_device
