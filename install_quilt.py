#!/usr/bin/env python
# -*- coding:utf-8 -*-

import os
from install_lib import *
import logging
logging.basicConfig(level=logging.DEBUG)

def install_quilt():
    environ = get_environ()
    pc = os.path.join(environ["KONIX_PERSO_DIR"],
                           os.environ["HOSTNAME"],
                           ".pc")
    patches = os.path.join(environ["KONIX_PERSO_DIR"],
                                os.environ["HOSTNAME"],
                                "patches"
    )
    pc_home = os.path.join(environ["HOME"] , ".pc")
    patches_home = os.path.join(environ["HOME"] , "patches")
    if not os.path.exists(pc):
        logging.info("Creating "+pc)
        os.makedirs(pc)
    if not os.path.exists(patches):
        logging.info("Creating "+patches)
        os.makedirs(patches)
    if os.path.exists(pc_home):
        logging.info("Removing "+pc_home)
        os.unlink(pc_home)
    if os.path.exists(patches_home):
        logging.info("Removing "+patches_home)
        os.unlink(patches_home)
    os.symlink(pc, pc_home)
    os.symlink(patches, patches_home)

    print("Successfully installed quilt config")

if __name__ == "__main__":
    install_quilt()
