#!/usr/bin/env python
# -*- coding:utf-8 -*-

import sys
import subprocess
import shlex

p = subprocess.Popen(["konix_notmuch_folders.sh",] + sys.argv[1:], stdout=subprocess.PIPE)
print " or ".join(["folder:" + line[:-1] for line in p.stdout.readlines()])
