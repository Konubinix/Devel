#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import os
import re

def get_default_key():
    GPGDIRRCFILENAME=os.environ["KONIX_PERSO_DIR"]

    gpgdirrc_name = os.path.expanduser(os.path.join(GPGDIRRCFILENAME, "gpgdirrc"))
    gpgdirrc = open(gpgdirrc_name, "r")
    for line in gpgdirrc.readlines():
        match = re.match("^use_key +(.+)$", line)
        if match:
            return match.group(1)
