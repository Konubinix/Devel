#!/usr/bin/env python
# -*- coding:utf-8 -*-

import sys
import subprocess
import shlex

p = subprocess.Popen(shlex.split("konix_notmuch_trash_folders.sh"), stdout=subprocess.PIPE)
print " or ".join(["folder:" + line[:-1] for line in p.stdout.readlines()])
