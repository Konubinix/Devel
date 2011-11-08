#!/usr/bin/env python
# -*- coding:utf-8 -*-

import os
import sys
import shutil

from install_lib import *

def install_git():
    environ = get_environ()
    # ####################################################################################################
    # extract the username and the email from the gitconfig file. then destroy the file
    # ####################################################################################################
    GITCONFIG_FILE_NAME=os.path.expanduser("~/.gitconfig")
    # set default values
    USER_NAME=os.environ.get("LOGNAME")
    EMAIL=os.environ.get("LOGNAME", "")+"@"+os.environ.get("HOSTNAME", "")
    from configobj import ConfigObj
    if os.path.exists(GITCONFIG_FILE_NAME):
        config = ConfigObj(GITCONFIG_FILE_NAME)
        USER_DICT = config.get("user")
        if USER_DICT:
            USER_NAME=USER_DICT.get("name", USER_NAME)
            EMAIL=USER_DICT.get("email", EMAIL)
        os.remove(GITCONFIG_FILE_NAME)
    # ####################################################################################################
    # Install a new git config file
    # ####################################################################################################
    GITCONFIG_FILE=open(GITCONFIG_FILE_NAME, "w")
    GITCONFIG_DEFAULT_FILE=open(os.path.join(environ["CONFIG_DIR"], "gitconfig"), "r")
    GITCONFIG_FILE.write("""[user]
	name = %s
	email = %s
""" % (USER_NAME, EMAIL)
                              )
    GITCONFIG_FILE.write(GITCONFIG_DEFAULT_FILE.read())
    GITCONFIG_DEFAULT_FILE.close()
    GITCONFIG_FILE.close()

    print "Successful installed git config"
if __name__ == '__main__':
    install_git()
