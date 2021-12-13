#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import os

# pip insall python-mpd2
import mpd

if __name__ == "__main__":
    c = mpd.MPDClient()
    c.connect(os.environ.get("MPD_HOST", "localhost"),
              os.environ.get("MPD_PORT", "6600"))
    print(c.status()["state"])
