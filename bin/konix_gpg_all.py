#!/usr/bin/env python
# -*- coding:utf-8 -*-

import os
import konix_gpg
import sys
import re
import subprocess
import socket                   # for hostname
import which
import logging
LOGGER = logging.getLogger(__name__)
#logging.basicConfig(level=logging.DEBUG)
#LOGGER.setLevel(logging.DEBUG)

# ####################################################################################################
# Parsing arguments
# ####################################################################################################
import argparse
parser = argparse.ArgumentParser(description="""Encrypt of decrypt recursively
current directory but this script.""")
parser.add_argument('-d','--decrypt', nargs='?', const="a", help="""Use decrypt (by default,
encryption is done)""")

args = parser.parse_args()

# ####################################################################################################
# Real script
# ####################################################################################################
# **********************************************************************
# Need gpg to use it...
# **********************************************************************
gpg_path=None
for gpg_exe_name in ["gpg", "gpg2"]:
    try:
        gpg_path=which.which(gpg_exe_name)
        break
    except:
        pass
if not gpg_path:
    LOGGER.error("No gpg found, cannot go further")
    exit(2)
# **********************************************************************
# Try to start a gpg agent file or use an already launched one
# **********************************************************************
def load_gpg_agent_info_file(gpg_agent_info_file):
    with open(gpg_agent_info_file,"r") as f:
        gpg_agent_info = f.readline().partition("=")
        os.environ[gpg_agent_info[0]] = gpg_agent_info[2]
        LOGGER.debug("Loading gpg-agent info: " + os.environ[gpg_agent_info[0]])

gpg_start_process = subprocess.Popen(["konix_gpg_agent_start.sh"], stdout=subprocess.PIPE)
rc = gpg_start_process.wait()
if rc == 0:
    gpg_agent_info_file = gpg_start_process.stdout.readlines()[0].replace("\n","")
    load_gpg_agent_info_file(gpg_agent_info_file)

DEFAULT_RECIPIENT=konix_gpg.get_default_key()

dir_walker = os.walk(".")
def remove_hidden_files(walk_res):
    hidden_file_indices = []
    index = 0
    to_remove = []
    for _file in walk_res:
        if _file.startswith("."):
            to_remove.append(_file)
    for to_remove_file in to_remove:
        walk_res.remove(to_remove_file)

def process_file(abs_file, decrypt):
    if abs_file.endswith(".gpg"):
        LOGGER.info(abs_file+" looks like an encrypted file")
        if decrypt:
            if "_nd" in abs_file:
                LOGGER.info("File does not want decryption")
                return
            decrypted_file_name = abs_file.replace(".gpg","")
            if os.path.exists(decrypted_file_name):
                LOGGER.info("already a decrypted file for "+abs_file+", don't touch it")
            else:
                LOGGER.info("decrypt "+abs_file+" to "+decrypted_file_name)
                retcode = subprocess.call([gpg_path,"-o",decrypted_file_name,"-d",abs_file])
                if retcode == 0:
                    LOGGER.info("removing old file "+abs_file)
                    os.remove(abs_file)
        else:
            LOGGER.info("Nothing to do for "+abs_file)
    else:
        LOGGER.info(abs_file+" looks like a decrypted file")
        if not decrypt:
            encrypted_file_name = abs_file+".gpg"
            if os.path.exists(encrypted_file_name):
                LOGGER.info("already a gpg file for "+abs_file+", replace it")
                os.remove(encrypted_file_name)
            LOGGER.info("encrypt "+abs_file+" to "+encrypted_file_name)
            retcode = subprocess.call([gpg_path,"-o",encrypted_file_name,"-r",DEFAULT_RECIPIENT,"-e",abs_file])
            if retcode == 0:
                LOGGER.info("removing old file "+abs_file)
                os.remove(abs_file)
        else:
            LOGGER.info("Nothing to do for "+abs_file)

for root, dirs, files in dir_walker:
    remove_hidden_files(dirs)
    remove_hidden_files(files)
    for _file in files:
        if os.path.split(sys.argv[0])[1] == _file:
            LOGGER.info("Not touching to me...")
            continue
        abs_file = os.path.join(root,_file)
        process_file(abs_file, args.decrypt)
