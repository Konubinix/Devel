#!/system/bin/env bash

#set -x
deb_root=/data/debian

echo "attempt to check the filesystem"
if test -e "${deb_root}/proc/cpuinfo"
then
    echo "System already set up"
else
    su -c "bash /system/bin/debsetup.sh ${deb_root}" \
        || { echo "Failed to setup, exiting" ; exit 1 ; }
fi

umask 0022
echo "Chrooting"
unset TMPDIR
TERM=linux \
	TERMINFO=/etc/terminfo \
    SHELL=/bin/bash \
    HOME=/root \
    PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/system/sbin:/system/bin:/system/xbin" \
    /system/bin/chroot $deb_root /bin/bash -l "$@"
