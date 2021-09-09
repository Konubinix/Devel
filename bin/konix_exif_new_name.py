#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import hashlib
import os
import re
import shlex
import subprocess
import sys

import dateutil.parser


def hash_of_file(file_name):
    m = hashlib.md5()
    m.update(open(os.path.realpath(file_name), "rb").read())
    return m.hexdigest()


if __name__ == "__main__":
    file_name = sys.argv[1]
    ext = os.path.splitext(file_name)[1]
    dirname = os.path.dirname(file_name)
    process = subprocess.Popen(shlex.split(
        "exiftool -p '$CreateDate|$Model' -d '%Y/%m/%d %H:%M:%S' '{}'".format(
            file_name)),
                               stdout=subprocess.PIPE)
    process.wait()
    output = process.stdout.read().decode("utf-8")
    match = re.match("^(?P<create_date>[^|]+)\|(?P<model>.+)$", output)
    assert match, "Could not parse '{}'".format(output)
    create_date = dateutil.parser.parse(match.group("create_date"))
    model = match.group("model").replace(" ", "_")
    new_name = create_date.strftime("%y%m%d_%H%M%S_{}{}".format(
        model,
        ext,
    ))
    index = 0
    while (os.path.exists(os.path.join(dirname, new_name)) and hash_of_file(
            os.path.join(dirname, new_name)) != hash_of_file(file_name)):
        sys.stderr.write(
            "{} already exists, find another name\n".format(new_name))
        index += 1
        new_name = create_date.strftime("%y%m%d_%H%M%S_{}_{}{}".format(
            model,
            index,
            ext,
        ))
    print(os.path.join(dirname, new_name))
