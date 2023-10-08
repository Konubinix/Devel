#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import argparse
import email
import logging
import os
from collections import namedtuple
from email.header import decode_header
from hashlib import sha256

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

    def decode_value(value):
        return ", ".join(
            [decode_part(b, encoding) for b, encoding in decode_header(value)])

    decoded_subject = decode_value(message["Subject"])
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

    def compute_name(part):
        sub_type = part.get_content_subtype()
        main_type = part.get_content_maintype()
        prefix = main_type
        extension = sub_type

        name = part.get_filename()
        name = name and decode_value(name)
        if name is None and sub_type == "html" and html_filename:
            name = html_filename + ".html"
        if name is None and sub_type == "plain" and plain_filename:
            name = plain_filename + ".plain"
        if not name:
            suffix = sha256(part.get_payload(decode=True)).hexdigest()[:10]
            name = f"{prefix}-{suffix}.{extension}"
        return name

    for part in get_parts(message):
        content = part.get_payload(decode=True)
        if content is None:
            continue
        name = compute_name(part)
        file_name = os.path.join(directory, name)
        cid = part.get("Content-Id", None)
        if cid:
            cid = cid.strip("<>")
            logger.debug("Adding the association {} -> {}".format(
                cid, file_name))
            CID_TO_NAME[cid] = file_name

    for part in get_parts(message):
        text = None
        content = part.get_payload(decode=True)
        if content is None:
            continue

        bcontent = part.get_payload()
        sub_type = part.get_content_subtype()
        main_type = part.get_content_maintype()
        name = compute_name(part)

        charset = part.get_content_charset()
        if charset is not None and "ascii" in charset:
            charset = "utf-8"
        if charset is None and main_type == "text":
            charset = "utf-8"
        if main_type == "text":
            if charset:
                try:
                    text = content.decode(charset)
                except UnicodeDecodeError:
                    # taken from the code of email/message.py:get_payload:
                    # This won't happen for RFC compliant messages (messages
                    # containing only ASCII code points in the unicode input).
                    # If it does happen, turn the string into bytes in a way
                    # guaranteed not to fail.
                    text = bcontent
            else:
                text = content.decode()

        file_name = os.path.join(directory, name)
        file_ = open(file_name, "bw")

        if text is not None:
            for cid, name in CID_TO_NAME.items():
                text = text.replace("cid:{}".format(cid), f"file://{name}")
            text = use_relative_links(text, directory)
            content = text.encode(charset)

        logger.debug("Writing in file {}".format(file_name))
        if sub_type == "html":
            text = make_part_harmless(text)
            text = """<?xml version="1.0" encoding="utf-8"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" lang="en" xml:lang="en">
<head>
<title>{}</title>
<meta  http-equiv="Content-Type" content="text/html;charset=utf-8" />
</head>
<body>
<a href="./">Folder</a>
{}
<a href="./">Folder</a>
</body>
</html>
""".format("Mail", text)
            content = text.encode("utf-8")
        file_.write(content)
        file_.close()


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
