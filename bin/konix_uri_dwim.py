#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import os
import sys
import re
import ConfigParser
import subprocess

def main():
    files = os.environ["KONIX_URI_DWIM_FILES"].split(os.pathsep)
    args = sys.argv[1:]
    uri = args[0]
    for file in files:
        if os.path.exists(file):
            config = ConfigParser.ConfigParser()
            config.optionxform = str    # keys not converted into lower case
            config.read(file)
            for section in config.sections():
                regexp = config.get(section, "uri_re")
                handler = config.get(section, "handler").split(" ")
                if re.search(regexp, uri):
                    subprocess.Popen(handler + args)
                    sys.exit(0)
    sys.exit(1)

if __name__ == "__main__":
    main()
