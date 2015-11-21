#!/bin/bash

distro=testing
export LANG=C

/debootstrap/debootstrap --second-stage

cat <<EOF > /etc/apt/sources.list
deb http://ftp.fr.debian.org/debian/ testing main contrib non-free
deb-src http://ftp.fr.debian.org/debian/ testing main contrib non-free
EOF
cat <<EOF > /etc/resolv.conf
nameserver 8.8.8.8
EOF
apt-get update
echo Y|apt-get install aptitude
aptitude update
setcap cap_net_raw+epi /bin/ping
dpkg-reconfigure locales

/usr/sbin/addgroup --gid 3003 aid_net
/usr/sbin/addgroup --gid 1015 sdcard-rw

/debootstrap/android_setup_perso.sh
