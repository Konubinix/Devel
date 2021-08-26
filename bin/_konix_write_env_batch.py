#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import re
import sys
import os
import time
import logging
logging.basicConfig(level=logging.DEBUG)

if len(sys.argv) > 1:
        ENV_FILE_NAME = sys.argv[1]
else:
        ENV_FILE_NAME = "env.bat"

if len(sys.argv) > 2:
        ENV_DIR = sys.argv[2]
else:
        ENV_DIR = os.getcwd()

while os.path.exists(ENV_FILE_NAME):
        try:
                os.remove(ENV_FILE_NAME)
        except:
                logging.warning("Not able to remove "+ENV_FILE_NAME+". Will try it in 1 second")
                time.sleep(1)

ENV_FILE = open(ENV_FILE_NAME, "w")

ENV_FILE.write("@echo off\r\n")
for line in sys.stdin.readlines():
        ENV_FILE.write("set "+line)
ENV_FILE.write("pushd \""+ENV_DIR+"\"\r\n")

ENV_FILE.close()
