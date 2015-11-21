#!/bin/bash

sudo mkdir -p rootfs/proc rootfs/dev rootfs/sys
sudo mount -B /proc rootfs/proc
sudo mount -B /dev rootfs/dev
sudo mount -B /dev/pts rootfs/dev/pts
sudo mount -B /sys rootfs/sys
