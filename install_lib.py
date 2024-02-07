#!/usr/bin/env python
# -*- coding:utf-8 -*-

import logging
import os
import re
import shutil
import sys
from pathlib import Path

logging.basicConfig(level=logging.DEBUG)


def get_environ():
    environ = {}
    environ["KONIX_PLATFORM"] = sys.platform
    logging.info("Installing for platform " + environ["KONIX_PLATFORM"])
    KONIX_PWD = os.getcwd().replace("\\", "/")
    environ["KONIX_DEVEL_DIR"] = KONIX_PWD
    environ["KONIX_BIN_DIR"] = environ["KONIX_DEVEL_DIR"] + "/" + "bin"
    environ["KONIX_LIB_DIR"] = environ["KONIX_DEVEL_DIR"] + "/" + "lib"
    environ["KONIX_SHARE_DIR"] = environ["KONIX_DEVEL_DIR"] + "/" + "share"
    environ["KONIX_SRC_DIR"] = environ["KONIX_DEVEL_DIR"] + "/" + "src"
    # add the lib dir to sys path in order to use the which lib so that I
    # can find python executable. sys.executable won't work with cygwin
    sys.path.insert(0, environ["KONIX_LIB_DIR"])
    python_bin = sys.executable
    environ["PYTHON_BIN"] = python_bin
    logging.info("Python bin is : " + python_bin)
    environ["PYTHON_PATH"] = os.path.dirname(python_bin)
    environ["KONIX_PWD"] = KONIX_PWD
    environ["KONIX_CONFIG_DIR"] = KONIX_PWD + "/" + "config"
    environ["KONIX_TUNING_DIR"] = KONIX_PWD + "/" + "tuning"
    environ["HOME"] = os.path.expanduser("~").replace("\\", "/")
    environ["KONIX_PERSO_DIRS"] = os.path.join(environ["HOME"], "perso")
    environ["KONIX_PERSO_DIR"] = os.path.join(environ["KONIX_PERSO_DIRS"],
                                              "perso")
    environ["PATH_SEPARATOR"] = os.pathsep
    environ["HOSTNAME"] = os.environ["HOSTNAME"]
    environ["KONIX_SH_CUSTOM_FILE"] = environ["HOME"] + "/" + ".shrc_custo"
    environ["KONIX_EMACS_CUSTOM_FILE"] = environ["HOME"] + "/" + ".emacs_custo"
    return environ


def is_on_linux():
    return re.match("linux", sys.platform)


def confirm(prompt=None, resp=False):
    """prompts for yes or no response from the user. Returns True for yes and
    False for no.

    'resp' should be set to the default value assumed by the caller when
    user simply types ENTER.

    >>> confirm(prompt='Create Directory?', resp=True)
    Create Directory? [y]|n:
    True
    >>> confirm(prompt='Create Directory?', resp=False)
    Create Directory? [n]|y:
    False
    >>> confirm(prompt='Create Directory?', resp=False)
    Create Directory? [n]|y: y
    True

    """
    if prompt is None:
        prompt = 'Confirm'
    if resp:
        prompt = '%s [%s]|%s: ' % (prompt, 'y', 'n')
    else:
        prompt = '%s [%s]|%s: ' % (prompt, 'n', 'y')
    while True:
        sys.stdout.write(prompt)
        sys.stdout.flush()
        ans = input("")
        if not ans:
            return resp
        if ans not in ['y', 'Y', 'n', 'N']:
            print('please enter y or n.')
            continue
        if ans == 'y' or ans == 'Y':
            return True
        if ans == 'n' or ans == 'N':
            return False


def substitute(src, dst):
    src = Path(src).expanduser()
    dst = Path(dst).expanduser()
    if os.path.isdir(dst) and not os.path.islink(dst):
        rm_action = shutil.rmtree
    else:
        rm_action = os.remove
    if os.path.exists(dst) or os.path.islink(dst):
        if confirm(prompt=f"Remove {dst}", resp=True):
            rm_action(dst)
        else:
            logging.info("Aborting substitution")
            return False
    if is_on_linux():
        action = os.symlink
    else:
        if os.path.isdir(src):
            action = shutil.copytree
        else:
            action = shutil.copyfile
    # make sure the parent directory exists
    if not os.path.isdir(os.path.dirname(dst)):
        os.makedirs(os.path.dirname(dst))
    print(f"{action.__name__} {dst} to {src}")
    action(src, dst)


def replace_file_content(file_name, content):
    try:
        os.remove(file_name)
    except:
        pass
    the_file = open(file_name, "wb")
    the_file.write(content.encode("utf-8"))
    the_file.close()
