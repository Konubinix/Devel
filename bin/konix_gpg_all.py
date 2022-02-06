#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import os
import konix_gpg
import sys
import re
import subprocess
import socket  # for hostname
import logging

LOGGER = logging.getLogger(__name__)

# ####################################################################################################
# Parsing arguments
# ####################################################################################################
import argparse

parser = argparse.ArgumentParser(description="""Encrypt of decrypt recursively
current directory but this script.""")
parser.add_argument('-d',
                    '--decrypt',
                    nargs='?',
                    const="a",
                    help="""Use decrypt (by default, encryption is done)""")
parser.add_argument('-v',
                    '--verbose',
                    action='store_true',
                    help="""Verbose mode""")

args = parser.parse_args()

# ####################################################################################################
# Real script
# ####################################################################################################
# **********************************************************************
# Hanlde verbosity
# **********************************************************************
if args.verbose:
    logging.basicConfig(level=logging.DEBUG)
    LOGGER.setLevel(logging.DEBUG)

# **********************************************************************
# Need gpg to use it...
# **********************************************************************
gpg_path = "gpg"
# **********************************************************************
# Try to start a gpg agent file or use an already launched one
# **********************************************************************
DEFAULT_RECIPIENT = konix_gpg.get_default_key()

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
        LOGGER.info(abs_file + " looks like an encrypted file")
        if decrypt:
            if "_nd" in abs_file:
                LOGGER.info("File does not want decryption")
                return
            decrypted_file_name = abs_file.replace(".gpg", "")
            if os.path.exists(decrypted_file_name):
                LOGGER.info("already a decrypted file for " + abs_file +
                            ", don't touch it")
            else:
                LOGGER.info("decrypt " + abs_file + " to " +
                            decrypted_file_name)
                retcode = subprocess.call(
                    [gpg_path, "-o", decrypted_file_name, "-d", abs_file])
                if retcode == 0:
                    LOGGER.info("removing old file " + abs_file)
                    os.remove(abs_file)
        else:
            LOGGER.info("Nothing to do for " + abs_file)
    else:
        LOGGER.info(abs_file + " looks like a decrypted file")
        if not decrypt:
            encrypted_file_name = abs_file + ".gpg"
            if os.path.exists(encrypted_file_name):
                LOGGER.info("already a gpg file for " + abs_file +
                            ", replace it")
                os.remove(encrypted_file_name)
            LOGGER.info("encrypt " + abs_file + " to " + encrypted_file_name)
            retcode = subprocess.call([
                gpg_path, "-o", encrypted_file_name, "-r", DEFAULT_RECIPIENT,
                "--sign", "-e", abs_file
            ])
            if retcode == 0:
                LOGGER.info("removing old file " + abs_file)
                os.remove(abs_file)
        else:
            LOGGER.info("Nothing to do for " + abs_file)


for root, dirs, files in dir_walker:
    remove_hidden_files(dirs)
    remove_hidden_files(files)
    for _file in files:
        if os.path.split(sys.argv[0])[1] == _file:
            LOGGER.info("Not touching to me...")
            continue
        abs_file = os.path.join(root, _file)
        process_file(abs_file, args.decrypt)
