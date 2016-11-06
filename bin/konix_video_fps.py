#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import sys

from moviepy.editor import VideoFileClip

if __name__ == "__main__":
    clip = VideoFileClip(sys.argv[1])
    print(round(clip.fps))
