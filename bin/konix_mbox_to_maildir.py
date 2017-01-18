#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import os
import shutil
import mailbox

import argparse
parser = argparse.ArgumentParser()


parser.add_argument('inbox')
parser.add_argument('-r','--replace', action="store_true")


def main():
    args = parser.parse_args()
    box = mailbox.mbox(args.inbox)
    mdir_path = os.path.join(
        os.path.dirname(args.inbox),
        "mail.{}".format(os.path.basename(args.inbox)),
    )
    if os.path.exists(mdir_path) and args.replace:
        print("Removing {}".format(mdir_path))
        shutil.rmtree(mdir_path)
    print("{} -> {}".format(args.inbox, mdir_path))
    mdir = mailbox.Maildir(mdir_path)
    for index, message in box.iteritems():
        mdir.add(message)


if __name__ == "__main__":
    main()
