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

konix_load_init () {
    # mist be done asap to increase chances the started shell is still in the active window
    if test -n "${TMUX_PANE}"
    then
        export KONIX_TMUX_WINDOW="$(tmux display-message -p '#{window_index}')"
    fi
    source "${HOME}/.shrc_var"
    source "${KONIX_CONFIG_DIR}/bashrc"
    source "${KONIX_SH_CUSTOM_FILE}"
}

# if the computer is in bad shape, do not load the whole configuration
[ "$(cut -d. -f1 /proc/loadavg)" -gt "$(expr 2 \* $(nproc))" ] && return
konix_load_init
""")

    # ####################################################################################################
    # SHRC
    # ####################################################################################################
    replace_file_content(
        os.path.join(environ["HOME"], ".shrc"), """#!/bin/bash
[ -n "${IN_NIX_SHELL}" ] && return
[ -z "$PS1" ] && return

konix_load_init () {
    # mist be done asap to increase chances the started shell is still in the active window
    if test -n "${TMUX_PANE}"
    then
        export KONIX_TMUX_WINDOW="$(tmux display-message -p '#{window_index}')"
    fi
    source "${HOME}/.shrc_var"
    source "${KONIX_CONFIG_DIR}/bashrc"
    source "${KONIX_SH_CUSTOM_FILE}"
}

# if the computer is in bad shape, do not load the whole configuration
[ "$(cut -d. -f1 /proc/loadavg)" -gt "$(expr 2 \* $(nproc))" ] && return
konix_load_init
""")
    print("Successful installed shell config")


if __name__ == '__main__':
    install_shell()
