#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import os
import shutil
import sys

from install_lib import *


def install_gdbinit():
    environ = get_environ()
    # ####################################################################################################
    # Generate a gdbinit file that sources the config ones
    # ####################################################################################################
    GDBINIT_FILE_NAME = os.path.expanduser("~/.gdbinit")
    gdbinit_config = os.path.join(environ["KONIX_CONFIG_DIR"],
                                  "gdbinit").replace(" ", r"\ ")
    gdbinit_stl_views = os.path.join(environ["KONIX_CONFIG_DIR"],
                                     "stl-views.gdb").replace(" ", r"\ ")
    if os.path.exists(GDBINIT_FILE_NAME):
        os.remove(GDBINIT_FILE_NAME)
    with open(GDBINIT_FILE_NAME, "w") as gdbinitfile:
        gdbinitfile.write('source %s\nsource %s\n' %
                          (gdbinit_stl_views, gdbinit_config))

    print("Successful installed gdbinit config")


if __name__ == '__main__':
    install_gdbinit()
