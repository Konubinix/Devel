#! /bin/bash

fusermount -u -z "/sdcard/${USER}"
fs_deref_symlink.py "$@" -oallow_other -ogat=False -oreadonly=False -oroot=$HOME "/sdcard/${USER}"
