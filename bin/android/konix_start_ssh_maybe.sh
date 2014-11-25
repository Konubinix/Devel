#!/bin/bash

if ! ps aux|grep sshd|grep -v grep|grep -q sshd
then
	echo "Starting sshd"
	/usr/sbin/sshd -p 1235
else
	echo "sshd already started"
fi
