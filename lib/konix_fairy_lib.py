#!/usr/bin/env python3
# -*- coding:utf-8 -*-
import os
import re
import sys

def get_files(MATCH, ALL, env_variable="PATH"):
        result = set()
        for match in MATCH:
                # take extensions into account
                match = match+"(\.[^\.]+)?$"
                for directory in os.environ[env_variable].split(os.pathsep):
                        if not (os.path.exists(directory) and os.path.isdir(directory)):
                                if os.environ.get("DEBUG_FAIRY"):
                                        sys.stderr.write(directory+" does not exist or is not a directory\n")
                                continue
                        files = os.listdir(directory)
                        found_files = set([os.path.join(directory,fil) for fil in files if re.match(match, fil)])
                        if found_files:
                                result.update(found_files)
                                if not ALL:
                                        break
        return result
