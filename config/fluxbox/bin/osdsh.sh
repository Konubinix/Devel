#!/bin/bash
#Load the daemon
osdsh

#give it time to start
sleep 1

#Load a style/theme. Optional.  This is my theme and does NOT come with osdsh, so it will need changing to your own custom theme if you want to use one.
#osdctl -S ~/.osdTheme

#Load the volume change daemon.  CAUTION:  THIS MAY SEND CPU TO 100% IN GUTSY GIBBON.  WORKS IN EDGY EFT. (SEE WORKAROUND BELOW)
osdctl -m 1