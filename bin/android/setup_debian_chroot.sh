#!/bin/bash

sudo mkdir -p rootfs_device/proc rootfs_device/dev rootfs_device/sys
sudo mount -B /proc rootfs_device/proc
sudo mount -B /dev rootfs_device/dev
sudo mount -B /dev/pts rootfs_device/dev/pts
sudo mount -B /sys rootfs_device/sys
sudo chroot rootfs_device
