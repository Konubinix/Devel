#!/bin/bash

if [ "$(konix_video_fps.py "$1")" == "15" ]
then
	rm -v "$1"
fi
