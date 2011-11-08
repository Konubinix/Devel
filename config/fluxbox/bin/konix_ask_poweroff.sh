#!/bin/bash

if zenity --question --text="Poweroff ?"
then
	/sbin/poweroff
fi
