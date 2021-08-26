#!/usr/bin/env python3
# -*- coding:utf-8 -*-
import sys
import re
import os
import fairy_lib

SCRIPT=sys.argv[1]
EXT=sys.argv[2]
ARGS=sys.argv[3:]
match=re.match("^(.+)\.[^\.]+$", SCRIPT)
if not match:
        print SCRIPT,"has not extension"
else:
        SCRIPT=match.group(1)
NEW_SCRIPT=SCRIPT+"."+EXT
# NEW_SCRIPT_PATH=fairy.get_files(NEW_SCRIPT, False)
new_command=""
for elem in [NEW_SCRIPT,]+ARGS:
        new_command += ' "'+elem.replace('"','\\"')+'"'
os.system(new_command)
