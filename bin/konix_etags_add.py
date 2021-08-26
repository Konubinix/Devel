#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import os
import re
import logging
logging.basicConfig(level=logging.DEBUG)
# ####################################################################################################
# Parsing arguments
# ####################################################################################################
import argparse
parser = argparse.ArgumentParser(description="""Add some information about includes and tags_dir in current directory's for current TAGS file
-d or -i must be providen
""")
parser.add_argument('-d','--dir',
                    help="""Directory to add to the TAGS_DIRS file""",
                    required=False
                    )
parser.add_argument('-i','--include',
                    help="""TAGS file to include in current TAGS_INCLUDES file""",
                    required=False
                    )
parser.add_argument('--cwd',
                    help="""The directory in which to add the new information""",
                    required=False
                    )

args = parser.parse_args()
# ####################################################################################################
# Initializing
# ####################################################################################################
TAGS_DIRS=args.dir and os.path.normpath(os.path.expanduser(args.dir)) or ""
TAGS_INCLUDES=args.include and os.path.normpath(os.path.expanduser(args.include)) or ""
TAGS_DIRS_FILE="./TAGS_DIRS"
TAGS_INCLUDES_FILE="./TAGS_INCLUDES"
CWD=args.cwd and os.path.normpath(os.path.expanduser(args.cwd)) or "."
os.chdir(CWD)
logging.debug("chdir to "+CWD)
if not ( TAGS_INCLUDES or TAGS_DIRS ):
    raise Exception("-d or -i must be providen, look at help with -h")
# checking arguments
if TAGS_DIRS and not os.path.isdir(TAGS_DIRS):
    logging.warning(TAGS_DIRS+" is not an existing directory. It is yet added to the TAGS_DIRS file")
if TAGS_INCLUDES and not os.path.isfile(TAGS_INCLUDES):
    logging.warning(TAGS_INCLUDES+" is not an existing file. It is yet added to the TAGS_INCLUDES file")

# ####################################################################################################
# Doing stuff
# ####################################################################################################
def add_uniquely_entry_to_file (entry, filename):
    # particular case : the file does not exists yet
    # import pdb
    # pdb.set_trace()
    # I don't want backslash style paths
    entry = entry.replace("\\","/")
    if not os.path.exists(filename):
        fil = open(filename, "w")
        fil.close()
    fil = open(filename, "r+")
    for checked_entry in fil.readlines():
        if re.match(entry+"[\n\r \t]*", checked_entry):
            logging.warning(entry+" already in the "+filename+" file, not adding it")
            fil.close()
            return 1
    fil.write(entry+"\n")
    fil.close()
if TAGS_DIRS:
    add_uniquely_entry_to_file(TAGS_DIRS, TAGS_DIRS_FILE)
if TAGS_INCLUDES:
    add_uniquely_entry_to_file(TAGS_INCLUDES, TAGS_INCLUDES_FILE)
