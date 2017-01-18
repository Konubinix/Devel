#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import hashlib
import tempfile
import os
import mailbox

import argparse
parser = argparse.ArgumentParser(description="""TODO.""")

parser.add_argument('source')
parser.add_argument('dest')


def main(source, dest):
    invalid = os.path.join(dest, "invalid")
    m = mailbox.Maildir(source)
    for mess in m:
        l = None
        try:
            if "Message-ID" in mess:
                elem = mess["Message-ID"].encode("utf-8")
            else:
                elem = mess.as_bytes()
            dir = os.path.join(dest, mess.get_subdir())
            if not os.path.exists(dir):
                os.makedirs(dir)
            l = os.path.join(
                dir,
                hashlib.md5(elem).hexdigest()
            )
            l += ":" + mess.get_info()
        except Exception as e:
            if not os.path.exists(invalid):
                os.makedirs(invalid)
            invalidfile = tempfile.NamedTemporaryFile(
                delete=False,
                prefix=invalid + "/")
            invalidfile.write(mess.as_bytes())
            invalidfile.close()
            print("Error {}, recorded invalid file in {}".format(e, invalidfile.name))

        if l is not None and not os.path.exists(l):
            with open(l, "wb") as f:
                f.write(mess.as_bytes())


if __name__ == "__main__":
    args = parser.parse_args()
    main(args.source, args.dest)
