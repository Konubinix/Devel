#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import sys
import os
import subprocess
import shlex
import dateutil.parser
import re

if __name__ == "__main__":
    file_name = sys.argv[1]
    ext = os.path.splitext(file_name)[1]

    process = subprocess.Popen(
        shlex.split("exiftool -p '$CreateDate|$Model' -d '%Y/%m/%d %H:%M:%S' '{}'".format(file_name)),
        stdout=subprocess.PIPE
    )
    process.wait()
    output = process.stdout.read().decode("utf-8")
    match = re.match("^(?P<create_date>[^|]+)\|(?P<model>.+)$", output)
    assert match, "Could not parse {}".format(output)
    create_date = dateutil.parser.parse(match.group("create_date"))
    model = match.group("model").replace(" ", "_")
    print(
        create_date.strftime(
            "%y%m%d_%H%M%S_{}{}".format(
                model,
                ext,
            )
        )
    )
