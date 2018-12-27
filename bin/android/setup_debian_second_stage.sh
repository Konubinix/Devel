#!/bin/bash -eux

export LANG=C

groupadd -g 3001 aid_net_bt_admin
groupadd -g 3002 aid_net_bt
groupadd -g 3003 aid_inet
groupadd -g 3004 aid_inet_raw
groupadd -g 3005 aid_inet_admin
groupadd -g 1015 sdcard-rw

gpasswd -a root aid_net_bt_admin
gpasswd -a root aid_net_bt
gpasswd -a root aid_inet
gpasswd -a root aid_inet_raw
gpasswd -a root aid_inet_admin

usermod -g 3003 _apt

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
aptitude update
aptitude -y full-upgrade
aptitude -y install iputils-ping locales dialog
setcap cap_net_raw+epi /bin/ping
dpkg-reconfigure locales
# en_GB + en_US + fr_FR : 133 134 135 150 151 152 225 226 227


/sbin/android_setup_perso.sh
