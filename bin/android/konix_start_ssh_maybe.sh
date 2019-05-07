#!/bin/bash

if ! ps aux|grep sshd|grep -v grep|grep -q sshd
then
	echo "Starting sshd"
	/usr/sbin/sshd -p 1235 -E /var/log/sshd.log
else
	echo "sshd already started"
fi
