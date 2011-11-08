#!/usr/bin/env python
# -*- coding:utf-8 -*-

import re
import os

gpgdirrc_name = os.path.expanduser("~/.gpgdirrc")
gpgdirrc = open(gpgdirrc_name, "r")
for line in gpgdirrc.readlines():
        match = re.match("^use_key +(.+)$", line)
        if match:
                print match.group(1)
