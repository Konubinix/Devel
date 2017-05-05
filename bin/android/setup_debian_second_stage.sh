#!/bin/bash -eux

export LANG=C

/debootstrap/debootstrap --second-stage

cat <<EOF > /etc/apt/sources.list
deb http://httpredir.debian.org/debian/ stable main contrib non-free
deb http://httpredir.debian.org/debian/ testing main contrib
EOF

cat <<EOF >> /etc/apt/apt.conf
APT::Get::Install-Recommends "false";
APT::Get::Install-Suggests "false";
EOF

cat <<EOF > /etc/apt/preferences
Package: *
Pin: release a=stable
Pin-Priority: 600

Package: *
Pin: release a=testing
Pin-Priority: 900

Package: systemd
Pin: origin ""
Pin-Priority: -1
EOF

cat <<EOF > /etc/resolv.conf
nameserver 8.8.8.8
EOF

apt-get update
apt-get -y install aptitude
sed -i 's/_apt:x:104:65534/_apt:x:104:3004/g' /etc/passwd
aptitude update
aptitude -y full-upgrade
aptitude -y install iputils-ping locales
setcap cap_net_raw+epi /bin/ping
dpkg-reconfigure locales
# en_GB + en_US + fr_FR : 133 134 135 150 151 152 225 226 227

/usr/sbin/addgroup --gid 3003 aid_net
/usr/sbin/addgroup --gid 1015 sdcard-rw

/sbin/android_setup_perso.sh
