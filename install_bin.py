#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import os
import subprocess
import sys
from string import Template
from tempfile import NamedTemporaryFile

from install_lib import *


def install_bin():
    environ = get_environ()

    print("Successful installed bin config")


if __name__ == '__main__':
    install_bin()
