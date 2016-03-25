#!/bin/bash

# http://techedemic.com/2014/09/18/creating-a-timelapse-clip-with-avconv/
avconv -y -r 10 -i %4d.jpg -r 10 -vcodec mpeg4 -q:v 3 output.mkv
