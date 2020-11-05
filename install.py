#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import os
import sys
import re
import shutil
import tarfile
import subprocess
import logging
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
DEFAULT_ENV_FILE_NAME = os.path.join(os.path.expanduser("~"),".env_"+sys.platform+".conf")
DEFAULT_ENV_FILE_CONTENT = "[replace]\n"
for key in environ:
        DEFAULT_ENV_FILE_CONTENT += key+"="+environ[key]+"\n"
replace_file_content(DEFAULT_ENV_FILE_NAME, DEFAULT_ENV_FILE_CONTENT)

# ####################################################################################################
# Put the default ~/bin folder and put the env getters
# ####################################################################################################
substitute(os.path.join(environ["KONIX_PWD"],"init_bin"), os.path.join(environ["HOME"],"init_bin"))

os.makedirs(os.path.expanduser("~/bin"))

# ####################################################################################################
# Install shell and emacs
# ####################################################################################################
from install_emacs import install_emacs
from install_vim import install_vim
from install_shell import install_shell
from install_git import install_git
from install_bin import install_bin
from install_gdbinit import install_gdbinit
install_shell()
install_emacs()
install_git()
install_bin()
install_gdbinit()
install_vim()

substitute(os.path.join(environ["KONIX_CONFIG_DIR"], "mailcap"),   os.path.join(environ["HOME"], ".mailcap"))
substitute(os.path.join(environ["KONIX_CONFIG_DIR"], "Makefile"),   os.path.join(environ["HOME"], "Makefile"))
substitute(os.path.join(environ["KONIX_CONFIG_DIR"], "gitk"),       os.path.join(environ["HOME"], ".gitk"))
substitute(os.path.join(environ["KONIX_CONFIG_DIR"], "inputrc"),    os.path.join(environ["HOME"], ".inputrc"))
substitute(os.path.join(environ["KONIX_CONFIG_DIR"], "B.blend"),    os.path.join(environ["HOME"], ".B.blend"))
substitute(os.path.join(environ["KONIX_CONFIG_DIR"], "screenrc"),   os.path.join(environ["HOME"], ".screenrc"))
substitute(os.path.join(environ["KONIX_CONFIG_DIR"], "vimrc"),      os.path.join(environ["HOME"], ".vimrc"))
substitute(os.path.join(environ["KONIX_CONFIG_DIR"], "terminator"), os.path.join(environ["HOME"], ".config/terminator/config"))
substitute(os.path.join(environ["KONIX_CONFIG_DIR"], "procmailrc"), os.path.join(environ["HOME"], ".procmailrc"))
substitute(os.path.join(environ["KONIX_SHARE_DIR"], "icons"),      os.path.join(environ["HOME"], ".icons"))

if confirm(prompt="Fetch submodules?"):
    print("Initializing the submodules")
    subprocess.call(["git", "submodule", "init"])
    print("Updating the submodules")
    subprocess.call(["git", "submodule", "update"])

if is_on_linux():
        substitute(os.path.join(environ["KONIX_CONFIG_DIR"], "fluxbox"),   os.path.join(environ["HOME"], ".fluxbox"))

        substitute(os.path.join(environ["KONIX_DEVEL_DIR"],  "bin"),       os.path.join(environ["HOME"], ".fluxbox/devel_bin"))
        substitute(os.path.join(environ["KONIX_CONFIG_DIR"], "ideskrc"),   os.path.join(environ["HOME"], ".ideskrc"))
        substitute(os.path.join(environ["KONIX_CONFIG_DIR"], "idesktop"),  os.path.join(environ["HOME"], ".idesktop"))
        substitute(os.path.join(environ["KONIX_CONFIG_DIR"], "Xdefaults"),
                   os.path.join(environ["HOME"], ".Xdefaults-" + environ["HOSTNAME"]))
        substitute(os.path.join(environ["KONIX_CONFIG_DIR"], "xinitrc"),   os.path.join(environ["HOME"], ".xinitrc"))
        substitute(os.path.join(environ["HOME"],       ".xinitrc"),  os.path.join(environ["HOME"], ".xsession"))
        substitute(os.path.join(environ["KONIX_CONFIG_DIR"], "gtkrc-2.0"), os.path.join(environ["HOME"], ".gtkrc-2.0"))
        substitute(os.path.join(environ["KONIX_CONFIG_DIR"], "rdesktop"), os.path.join(environ["HOME"], ".rdesktop"))
        substitute(os.path.join(environ["KONIX_CONFIG_DIR"], ".percol.d"), os.path.join(environ["HOME"], ".percol.d"))
        substitute(os.path.join(environ["KONIX_CONFIG_DIR"], "starship.toml"), os.path.join(environ["HOME"], ".config", "starship.toml"))

        subprocess.call(["chmod", "+x", "-Rv",
                         os.path.join(environ["HOME"], "bin"),
                         os.path.join(environ["HOME"], ".fluxbox/startup"),
                         os.path.join(environ["HOME"], ".fluxbox/bin")
                         ])
