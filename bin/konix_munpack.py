#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import email
import tempfile
import re
import os
import logging
logger = logging.getLogger("munpack")
from collections import namedtuple

import argparse
parser = argparse.ArgumentParser(description="""Munpack, with better support for cid.""")

parser.add_argument('-i','--input',
                    help="""Input message to read""",
                    type=str,
                    required=True)

parser.add_argument('-o','--output',
                    help="""Output directory""",
                    type=str,
                    required=True)

LEVELS = {'debug': logging.DEBUG,
          'info': logging.INFO,
          'warning': logging.WARNING,
          'error': logging.ERROR,
          'critical': logging.CRITICAL
         }

parser.add_argument('-v','--verbosity',
                    help="""Log level""",
                    default="warning",
                    choices=LEVELS,
                    required=False)

MessageInfo = namedtuple(
    "MessageInfo",
    [
        "content",
    ]
)

CID_TO_NAME = {}

def get_parts(message):
    """Make sure to get the images first.

    They must be  parsed first to fill the
    cid dictionary before the html parts."""
    parts = list(message.walk())
    parts.sort(
        key=lambda part: 0 if part.get_content_maintype() == "image" else 1
    )
    return parts

def munpack(message, directory):
    for part in get_parts(message):
        content = part.get_payload(decode=True)
        if content is None:
            continue
        content_type = part.get_content_type()
        main_type = part.get_content_maintype()
        sub_type = part.get_content_subtype()
        prefix = main_type
        extension = sub_type
        file_ = tempfile.NamedTemporaryFile(
            mode="bw",
            dir=directory,
            suffix=".{}".format(extension),
            prefix=prefix,
            delete=False)
        file_name = file_.name
        for cid, name in CID_TO_NAME.items():
            content = content.replace(
                "cid:{}".format(cid).encode("utf-8"),
                name.encode("utf-8")
            )
        logger.debug("Writing in file {}".format(file_name))
        file_.write(content)
        file_.close()
        cid = part.get("Content-Id", None)
        if cid:
            cid = cid.strip("<>")
            logger.debug("Adding the association {} -> {}".format(cid, file_name))
            CID_TO_NAME[cid] = file_name

if __name__ == "__main__":
    args = parser.parse_args()
    logging.basicConfig(level=LEVELS[args.verbosity])
    munpack(
        email.message_from_file(open(args.input, "r")),
        args.output
    )
