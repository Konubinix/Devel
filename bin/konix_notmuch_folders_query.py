#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import sys
import subprocess

p = subprocess.Popen(
    ["konix_notmuch_folders.sh",] + sys.argv[1:],
    stdout=subprocess.PIPE,
    encoding="utf-8",
)
print(" or ".join(["folder:" + line[:-1] for line in p.stdout.readlines()]))
