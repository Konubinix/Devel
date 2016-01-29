#!/usr/bin/env python3
# -*- coding:utf-8 -*-

from email.mime.multipart import MIMEMultipart
from email.mime.text import MIMEText
import mailbox
import sys
import html2text

import argparse
parser = argparse.ArgumentParser(description="""format an html mail.""")

parser.add_argument('-f','--from_',
                    type=str,
                    required=True)
parser.add_argument('-t','--to',
                    type=str,
                    required=True)
parser.add_argument('-s','--subject',
                    type=str,
                    required=True)
parser.add_argument('-d','--date',
                    type=str,
                    required=False)

parser.add_argument('directory', type=str,
                    help='The maildir directory')

if __name__ == "__main__":
    args = parser.parse_args()
    content = sys.stdin.read()
    content_lines = content.splitlines()
    pure_text_prefix = ""
    try:
        html_separator_index = content_lines.index("KONIX_HTML")
        pure_text_prefix = "\n".join(content_lines[:html_separator_index])
        content = "\n".join(content_lines[html_separator_index+1:])
    except ValueError:
        pass

    text_content = pure_text_prefix + "\n" + html2text.html2text(content)
    html_content = pure_text_prefix + "\n" + content

    msg = MIMEMultipart('alternative')
    msg['Subject'] = args.subject
    msg['From'] = args.from_
    msg['To'] = args.to
    if args.date:
        msg['Date'] = args.date
    plain_part = MIMEText(text_content, _subtype='plain', _charset="utf-8")
    html_part = MIMEText(html_content, _subtype='html', _charset="utf-8")
    msg.attach(plain_part)
    msg.attach(html_part)
    box = mailbox.Maildir(args.directory)
    print(box.add(msg))
