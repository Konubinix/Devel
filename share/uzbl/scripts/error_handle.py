#!/usr/bin/env python
# -*- coding:utf-8 -*-

import re
import sys
import os
import subprocess

uri=sys.argv[1]
reason=sys.argv[2]
description=sys.argv[3]
FIFO=os.environ["UZBL_FIFO"]

if re.match('SSL handshake failed', description):
    with open(FIFO, "w") as f:
        f.write("js alert('" + uri + "\\\\n" + description + "');\n")
