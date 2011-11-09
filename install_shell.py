#!/usr/bin/env python
# -*- coding:utf-8 -*-
import os
import sys

from install_lib import *

def install_shell():
        environ = get_environ()
        # ####################################################################################################
        # install shrc_var
        # ####################################################################################################
        SHRC_VAR_FILE_NAME = os.path.join(environ["HOME"], ".shrc_var")
        SHRC_VAR_FILE_CONTENT = """#!/bin/bash
# put the python of this platform in front of the path
# the only hacky stuf I need is the platform
export WANTED_PLATFORM="$(source "${HOME}/init_bin/_konix_platform.sh")"
PATH_SEPARATOR="$(cd "${HOME}/init_bin" && "./_konix_get_default_env.py" PATH_SEPARATOR)"
PYTHON_BIN="$(cd "${HOME}/init_bin" && "./_konix_get_default_env.py" PYTHON_BIN)"
PYTHON_PATH="$(cd "${HOME}/init_bin" && ./konix_dirname.py "$PYTHON_BIN")"
export PATH="$PYTHON_PATH${PATH_SEPARATOR}$PATH"
# now, I am sure the python path is in first position before running the import env
# Import env variables
source "${HOME}/init_bin/konix_import_env.sh"
# importing custom environnement variables
import_env
"""
        replace_file_content(SHRC_VAR_FILE_NAME, SHRC_VAR_FILE_CONTENT)

        if not os.path.exists(environ["SH_CUSTOM_FILE"]):
                replace_file_content(environ["SH_CUSTOM_FILE"], """#!/bin/bash
# custom sh commands
""")

        replace_file_content(os.path.join(environ["HOME"], ".bashrc"), """#!/bin/bash
source "${HOME}/.shrc_var"
source "${CONFIG_DIR}/bashrc"
source "${SH_CUSTOM_FILE}"
""")

        replace_file_content(os.path.join(environ["HOME"], ".shrc"), """#!/bin/sh
source "${HOME}/.shrc_var"
source "${CONFIG_DIR}/shrc"
source "${SH_CUSTOM_FILE}"
""")

        replace_file_content(os.path.join(environ["HOME"], ".zshrc"), """#!/bin/zsh
source "${HOME}/.shrc_var"
source "${CONFIG_DIR}/zshrc"
source "${SH_CUSTOM_FILE}"
""")

        print "Successful installed shell config"
if __name__ == '__main__':
        install_shell()
