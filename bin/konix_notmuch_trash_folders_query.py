#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import sys
import subprocess
import shlex

p = subprocess.Popen(
    shlex.split("konix_notmuch_trash_folders.sh"),
    stdout=subprocess.PIPE,
    encoding="utf-8",
)
print(" or ".join(["folder:" + line[:-1] for line in p.stdout.readlines()]))
