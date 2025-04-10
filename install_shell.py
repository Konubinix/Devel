#!/usr/bin/env python3
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
source "${HOME}/init_bin/konix_init_lib.sh"
# importing custom environnement variables
import_env
"""
    replace_file_content(SHRC_VAR_FILE_NAME, SHRC_VAR_FILE_CONTENT)

    # ####################################################################################################
    # Install KONIX_SH_CUSTOM_FILE
    # ####################################################################################################
    if not os.path.exists(environ["KONIX_SH_CUSTOM_FILE"]):
        replace_file_content(environ["KONIX_SH_CUSTOM_FILE"], """#!/bin/bash
# custom sh commands
""")

    # ####################################################################################################
    # Install bashrc
    # ####################################################################################################
    replace_file_content(
        os.path.join(environ["HOME"], ".bashrc"), """#!/bin/bash
[ -n "${IN_NIX_SHELL}" ] && return
[ -z "$PS1" ] && return
test "${TERM}" = "dumb" && return

# must be done asap to increase chances the started shell is still in the active window
if test -n "${TMUX_PANE}"
then
    export KONIX_TMUX_WINDOW="$(tmux display-message -p '#{window_index}')"
fi

# if the computer is in bad shape, do not load the whole configuration
if [ "$(cut -d. -f1 /proc/loadavg)" -gt "$(expr 2 \* $(nproc))" ]
then
    read -p 'Busy system, load the conf anyway? (Y/n)' answer
    if test "n" = "${answer}"
    then
        return
    fi
fi

# don't put this code inside a function, or stuff like preexec won't be
# persisted in the session
source "${HOME}/.shrc_var"
source "${KONIX_CONFIG_DIR}/bashrc"
source "${KONIX_SH_CUSTOM_FILE}"
""")

    print("Successful installed shell config")


if __name__ == '__main__':
    install_shell()
