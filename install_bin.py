#!/usr/bin/env python
# -*- coding:utf-8 -*-

from install_lib import *

import os
import sys
import subprocess
from string import Template
from tempfile import NamedTemporaryFile

def install_bin():
    environ = get_environ()
    subprocess.call(["gcc", os.path.join(environ["KONIX_SRC_DIR"],"gnome-run.c"), "-o", os.path.join(environ["HOME"],".fluxbox/bin/gnome-run"), "-L/usr/X11R6/lib", "-lX11"])

    print "Successful installed bin config"

if __name__ == '__main__':
    install_bin()
