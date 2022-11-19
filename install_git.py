#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import os
import shutil
import subprocess
import sys

from install_lib import *
from pathlib import Path


def install_git():
    environ = get_environ()
    Path("~/.gitconfig").expanduser().write_text("""[user]
    email = konubinixweb@gmail.com
    name = "Samuel Loury"
""")
    print("Successful installed git config")


if __name__ == '__main__':
    install_git()
