#!/bin/bash

#--- set a fullscreen image in background
xloadimage -onroot -quiet -fullscreen "/etc/X11/xdm/background.jpg"

#--- set Shutdown/Reboot buttons
(
xmessage -buttons Shutdown:20,Reboot:21 "" ;
case $? in
 20)
 exec /sbin/poweroff;;
 21)
 exec /sbin/reboot;;
 *)
 echo "Xmessage closed on `date`";;
esac
)&
