#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import sys
import os
import subprocess
import shlex
import dateutil.parser
import re
import hashlib
import konix_time_helper

def hash_of_file(file_name):
    m = hashlib.md5()
    m.update(open(os.path.realpath(file_name), "rb").read())
    return m.hexdigest()

if __name__ == "__main__":
    file_name = sys.argv[1]
    ext = os.path.splitext(file_name)[1]
    dirname = os.path.dirname(file_name)
    process = subprocess.Popen(
        shlex.split("extract -p 'creation time' -g '{}'".format(file_name)),
        stdout=subprocess.PIPE
    )
    output, error = process.communicate()
    output = output.decode("utf-8")
    match = re.match("^[^`]+`(?P<creation_time>.+)'", output)
    assert match, "Could not parse '{}'".format(output)
    create_date = dateutil.parser.parse(match.group("creation_time"))
    create_date = konix_time_helper.date_to_tz(create_date)
    new_name = create_date.strftime(
        "%y%m%d_%H%M%S{}".format(
            ext,
        )
    )
    index = 0
    while (
            os.path.exists(os.path.join(dirname, new_name))
            and
            hash_of_file(os.path.join(dirname, new_name)) != hash_of_file(file_name)
    ):
        sys.stderr.write("{} already exists, find another name\n".format(new_name))
        index += 1
        new_name = create_date.strftime(
            "%y%m%d_%H%M%S_{}{}".format(
                index,
                ext,
            )
        )
    print(os.path.join(dirname, new_name))
