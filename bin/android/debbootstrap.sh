#!/bin/bash

cat <<EOF > /etc/apt/sources.list
deb http://ftp.fr.debian.org/debian/ testing main contrib non-free
deb-src http://ftp.fr.debian.org/debian/ testing main contrib non-free
EOF
apt-get install aptitude
aptitude update
aptitude install vim git-annex python bash-completion openssh-server screen myrepos
