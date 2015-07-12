#!/bin/bash

cherrymusicd "$@"
while [ -e /tmp/cherrymusic.pid ]
do
	sleep 1
done
