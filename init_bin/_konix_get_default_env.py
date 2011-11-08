#!/usr/bin/env python
# -*- coding:utf-8 -*-

import re
import os
import sys

SEARCH=sys.argv[1]
PLATFORM=os.environ["PLATFORM"]
assert(PLATFORM)

ENV_FILE = open(os.path.join(os.path.expanduser("~"), ".env_"+PLATFORM+".conf"), "r")
RES=""
for line in ENV_FILE.readlines():
    match = re.match("^"+SEARCH+"=['\"]?(.*)['\"]?$", line)
    if match:
        RES=match.group(1)
        break
print RES
ENV_FILE.close()
