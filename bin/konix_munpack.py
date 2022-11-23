#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import argparse
import email
import logging
import os
import tempfile
from collections import namedtuple
from email.header import decode_header

from konix_file_helper import sanitize_filename
from konix_mail import make_part_harmless, use_relative_links

logger = logging.getLogger("munpack")
parser = argparse.ArgumentParser(
    description="""Munpack, with better support for cid.""")

parser.add_argument('-i',
                    '--input',
                    help="""Input message to read""",
                    type=str,
                    required=True)

parser.add_argument('-o',
                    '--output',
                    help="""Output directory""",
                    type=str,
                    required=True)

parser.add_argument('--rel-path',
                    help="""Use relative paths when adding cid links""",
                    action="store_true")

LEVELS = {
    'debug': logging.DEBUG,
    'info': logging.INFO,
    'warning': logging.WARNING,
    'error': logging.ERROR,
    'critical': logging.CRITICAL
}

parser.add_argument('-v',
                    '--verbosity',
                    help="""Log level""",
                    default="warning",
                    choices=LEVELS,
                    required=False)

MessageInfo = namedtuple("MessageInfo", [
    "content",
])

CID_TO_NAME = {}


def get_parts(message):
    """Make sure to get the images first.

    They must be  parsed first to fill the
    cid dictionary before the html parts."""
    parts = list(message.walk())
    parts.sort(
        key=lambda part: 0 if part.get_content_maintype() == "image" else 1)
    return parts


def munpack(message, directory, rel_path=False):
    plain_filename = None
    html_filename = None

    def decode_part(message, encoding):
        if isinstance(message, str):
            return message
        elif encoding is None:
            return message.decode()
        else:
            return message.decode(encoding)

    decoded_subject = ", ".join([
        decode_part(b, encoding)
        for b, encoding in decode_header(message["Subject"])
    ])
    sanitized_subject = sanitize_filename(decoded_subject)
    if len([
            part for part in get_parts(message)
            if part.get_content_subtype() == "html"
    ]) == 1:
        html_filename = sanitized_subject
    if len([
            part for part in get_parts(message)
            if part.get_content_subtype() == "plain"
    ]) == 1:
        plain_filename = sanitized_subject

    for part in get_parts(message):
        text = None
        content = part.get_payload(decode=True)
        bcontent = part.get_payload()
        if content is None:
            continue
        charset = part.get_content_charset()
        if charset is not None and "ascii" in charset:
            charset = "utf-8"
        sub_type = part.get_content_subtype()
        if charset is None and sub_type == "html":
            charset = "utf-8"
        main_type = part.get_content_maintype()
        if main_type == "text":
            try:
                text = content.decode(charset) if charset else None
            except UnicodeDecodeError:
                # taken from the code of email/message.py:get_payload:
                # This won't happen for RFC compliant messages (messages
                # containing only ASCII code points in the unicode input).
                # If it does happen, turn the string into bytes in a way
                # guaranteed not to fail.
                text = bcontent

        prefix = main_type
        extension = sub_type
        name = part.get_filename()
        if name is None and sub_type == "html" and html_filename:
            name = html_filename + ".html"
        if name is None and sub_type == "plain" and plain_filename:
            name = plain_filename + ".plain"
        if name:
            file_name = os.path.join(directory, name)
            file_ = open(file_name, "bw")
        else:
            file_ = tempfile.NamedTemporaryFile(mode="bw",
                                                dir=directory,
                                                suffix=".{}".format(extension),
                                                prefix=prefix,
                                                delete=False)
            file_name = os.path.relpath(file_.name,
                                        directory) if rel_path else file_.name
        if text is not None:
            for cid, name in CID_TO_NAME.items():
                text = text.replace(
                    "cid:{}".format(cid), "{}".format(name)
                    if rel_path else "file://{}".format(name))
            content = text.encode(charset)
        logger.debug("Writing in file {}".format(file_name))
        if sub_type == "html":
            text = make_part_harmless(text)
            text = use_relative_links(text, directory)
            text = """<?xml version="1.0" encoding="utf-8"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" lang="en" xml:lang="en">
<head>
<title>{}</title>
<meta  http-equiv="Content-Type" content="text/html;charset=utf-8" />
</head>
<body>
{}
</body>
</html>
""".format("Mail", text)
            content = text.encode("utf-8")
        file_.write(content)
        file_.close()
        cid = part.get("Content-Id", None)
        if cid:
            cid = cid.strip("<>")
            logger.debug("Adding the association {} -> {}".format(
                cid, file_name))
            CID_TO_NAME[cid] = file_name


if __name__ == "__main__":
    args = parser.parse_args()
    logging.basicConfig(level=LEVELS[args.verbosity])

    def get_mail(encoding):
        try:
            return email.message_from_file(
                open(args.input, "r", encoding=encoding))
        except UnicodeDecodeError:
            return None

    for encoding in ("utf-8", "latin-1"):
        mail = get_mail(encoding)
        if mail:
            break
    assert mail, "Could not read file"
    munpack(mail, args.output, rel_path=args.rel_path)
