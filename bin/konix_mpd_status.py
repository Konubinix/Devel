#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import os

import mpd

if __name__ == "__main__":
    c = mpd.MPDClient()
    c.connect(
        os.environ.get("MPD_HOST", "0.0.0.0"),
        os.environ.get("MPD_PORT", "6660")
    )
    print(c.status()["state"])
