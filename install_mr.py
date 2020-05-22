#!/usr/bin/env python
# -*- coding:utf-8 -*-

from install_lib import *

def install_mr():
    environ = get_environ()
    replace_file_content(os.path.join(environ["HOME"] , ".mrconfig"), """[DEFAULT]
include = cat $KONIX_MR_CONFIG
""")

    print("Successfully installed mr config")
if __name__ == "__main__":
    install_mr()
