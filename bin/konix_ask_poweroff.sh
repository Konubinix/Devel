#!/usr/bin/env bash

if zenity --question --text="Poweroff ?"
then
	/sbin/poweroff
fi
