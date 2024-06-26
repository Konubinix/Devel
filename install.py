#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import logging
import os
import re
import shutil
import subprocess
import sys
import tarfile
from pathlib import Path

logging.basicConfig(level=logging.DEBUG)
import tempfile


def check_deps():
    import argparse


check_deps()
# ####################################################################################################
# Fill the environ variables
# ####################################################################################################
from install_lib import *

environ = get_environ()
# ####################################################################################################
# Install ~/.env.conf default env file. It is the only absolute prerequisite for
# everything else
# ####################################################################################################
DEFAULT_ENV_FILE_NAME = os.path.join(os.path.expanduser("~"),
                                     ".env_" + sys.platform + ".conf")
DEFAULT_ENV_FILE_CONTENT = "[replace]\n"
for key in environ:
    DEFAULT_ENV_FILE_CONTENT += key + "=" + environ[key] + "\n"
replace_file_content(DEFAULT_ENV_FILE_NAME, DEFAULT_ENV_FILE_CONTENT)

# ####################################################################################################
# Put the default ~/bin folder and put the env getters
# ####################################################################################################
substitute(os.path.join(environ["KONIX_PWD"], "init_bin"),
           os.path.join(environ["HOME"], "init_bin"))

if not (bindir := Path("~/bin").expanduser()).exists():
    os.makedirs(bindir)

from install_bin import install_bin
# ####################################################################################################
# Install shell and emacs
# ####################################################################################################
from install_emacs import install_emacs
from install_gdbinit import install_gdbinit
from install_git import install_git
from install_shell import install_shell
from install_vim import install_vim

install_shell()
install_emacs()
install_git()
install_bin()
install_gdbinit()
install_vim()

config_dir = Path(environ["KONIX_CONFIG_DIR"])

substitute(config_dir / "byobu", "~/.byobu")
substitute(config_dir / "ledgerrc", "~/.ledgerrc")
substitute(os.path.join(environ["KONIX_CONFIG_DIR"], "mailcap"),
           os.path.join(environ["HOME"], ".mailcap"))
substitute(os.path.join(environ["KONIX_CONFIG_DIR"], "xscreensaver"),
           os.path.join(environ["HOME"], ".xscreensaver"))
substitute(os.path.join(environ["KONIX_CONFIG_DIR"], "gitk"),
           os.path.join(environ["HOME"], ".gitk"))
substitute(os.path.join(environ["KONIX_CONFIG_DIR"], "inputrc"),
           os.path.join(environ["HOME"], ".inputrc"))
substitute(os.path.join(environ["KONIX_CONFIG_DIR"], "B.blend"),
           os.path.join(environ["HOME"], ".B.blend"))
substitute(os.path.join(environ["KONIX_CONFIG_DIR"], "screenrc"),
           os.path.join(environ["HOME"], ".screenrc"))
substitute(os.path.join(environ["KONIX_CONFIG_DIR"], "vimrc"),
           os.path.join(environ["HOME"], ".vimrc"))
substitute(os.path.join(environ["KONIX_CONFIG_DIR"], "terminator"),
           os.path.join(environ["HOME"], ".config/terminator/config"))
substitute(os.path.join(environ["KONIX_CONFIG_DIR"], "procmailrc"),
           os.path.join(environ["HOME"], ".procmailrc"))
substitute(os.path.join(environ["KONIX_SHARE_DIR"], "icons"),
           os.path.join(environ["HOME"], ".icons"))

if confirm(prompt="Fetch submodules?"):
    print("Initializing the submodules")
    subprocess.call(["git", "submodule", "init"])
    print("Updating the submodules")
    subprocess.call(["git", "submodule", "update"])

if is_on_linux():
    substitute(
        os.path.join(environ["KONIX_CONFIG_DIR"], "Xdefaults"),
        os.path.join(environ["HOME"], ".Xdefaults-" + environ["HOSTNAME"]))
    substitute(os.path.join(environ["KONIX_CONFIG_DIR"], "xinitrc"),
               os.path.join(environ["HOME"], ".xinitrc"))
    substitute(os.path.join(environ["HOME"], ".xinitrc"),
               os.path.join(environ["HOME"], ".xsession"))
    substitute(os.path.join(environ["KONIX_CONFIG_DIR"], "gtkrc-2.0"),
               os.path.join(environ["HOME"], ".gtkrc-2.0"))
    substitute(os.path.join(environ["KONIX_CONFIG_DIR"], "rdesktop"),
               os.path.join(environ["HOME"], ".rdesktop"))
    substitute(os.path.join(environ["KONIX_CONFIG_DIR"], "starship.toml"),
               os.path.join(environ["HOME"], ".config", "starship.toml"))

    subprocess.call([
        "chmod",
        "+x",
        "-Rv",
        os.path.join(environ["HOME"], "bin"),
    ])
