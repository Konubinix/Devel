#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import base64
import os
import mailbox

import argparse
parser = argparse.ArgumentParser(description="""TODO.""")

parser.add_argument('source')
parser.add_argument('dest')


def main(source, dest):
    if not os.path.exists(dest):
        os.makedirs(dest)
    m = mailbox.Maildir(source)
    for mess in m:
        l = os.path.join(
            dest,
            base64.b64encode(
                mess["Message-ID"].encode("utf-8")
            ).decode("utf-8")
        )
        open(l, "wb").write(mess.as_bytes())


if __name__ == "__main__":
    args = parser.parse_args()
    main(args.source, args.dest)
