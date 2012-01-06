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

    # install the emacs_tray_daemon
    # export the code from the template
    TRAY_PROGRAM_TPL_FILE = open(os.path.join(environ["KONIX_SRC_DIR"],"emacs_tray_daemon.c.tpl"), "r")
    TRAY_PROGRAM_FILE = NamedTemporaryFile(delete=False, suffix=".c")
    TRAY_PROGRAM_TPL = Template(TRAY_PROGRAM_TPL_FILE.read())
    TRAY_PROGRAM_FILE.write(TRAY_PROGRAM_TPL.substitute(environ))
    TRAY_PROGRAM_FILE.close()
    subprocess.call(["sh","-c", "gcc '%s' `pkg-config --cflags --libs gtk+-2.0` -o %s/emacs_tray_daemon"
                     %(TRAY_PROGRAM_FILE.name,
                       os.path.join(os.environ["HOME"],"bin")
                       )
                     ])
    os.remove(TRAY_PROGRAM_FILE.name)

    print "Successful installed bin config"

if __name__ == '__main__':
    install_bin()
