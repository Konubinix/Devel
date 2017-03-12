#!/bin/bash -eu

export LANG=C

/debootstrap/debootstrap --second-stage

cat <<EOF > /etc/apt/sources.list
deb http://httpredir.debian.org/debian/ stable main contrib non-free
deb-src http://httpredir.debian.org/debian/ stable main contrib non-free
EOF
cat <<EOF > /etc/resolv.conf
nameserver 8.8.8.8
EOF
apt-get update
apt-get install -y aptitude
aptitude update
aptitude install -y iputils-ping
setcap cap_net_raw+epi /bin/ping
dpkg-reconfigure locales

/usr/sbin/addgroup --gid 3003 aid_net
/usr/sbin/addgroup --gid 1015 sdcard-rw

/debootstrap/android_setup_perso.sh
