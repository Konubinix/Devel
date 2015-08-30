#!/bin/bash

sudo chroot rootfs_device /bin/adduser ${USERNAME}
sudo chroot rootfs_device su ${USERNAME}
