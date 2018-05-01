#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import sys

from moviepy.editor import VideoFileClip

if __name__ == "__main__":
    if len(sys.argv > 1) and sys.argv[1] == "--help":
        print("{} :TODO".format(sys.argv[0]))
        sys.exit(0)
    clip = VideoFileClip(sys.argv[1])
    print(round(clip.fps))
