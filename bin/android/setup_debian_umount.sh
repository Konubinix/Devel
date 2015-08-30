#!/bin/bash

sudo umount rootfs_device/proc
sudo umount rootfs_device/dev/pts
sudo umount rootfs_device/dev
sudo umount rootfs_device/sys
sudo umount rootfs_device
sudo eject /dev/sdb
