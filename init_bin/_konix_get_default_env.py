#!/usr/bin/env python2
# -*- coding:utf-8 -*-

import re
import os
import sys

SEARCH=sys.argv[1]
PLATFORM=os.environ["WANTED_PLATFORM"]
assert(PLATFORM)

ENV_FILE = open(os.path.join(os.path.expanduser("~"), ".env_"+PLATFORM+".conf"), "r")
RES=""
for line in ENV_FILE.readlines():
    match = re.match("^"+SEARCH+"=['\"]?(.*)['\"]?$", line)
    if match:
        RES=match.group(1)
        break
sys.stdout.write(RES)
ENV_FILE.close()
