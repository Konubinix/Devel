#!/usr/bin/python3
# -*- coding:utf-8 -*-
import argparse
import sys

import konix_notify

parser = argparse.ArgumentParser(
    description="Display a message using the notification daemon.", )

parser.add_argument('-u',
                    '--unique',
                    action="store_true",
                    help="Unique message")

parser.add_argument(
    '-t',
    '--type',
    type=str,
    help="Type of the notification",
    choices=["normal", "annoying", "boring"],
    default="normal",
)
parser.add_argument(
    '-T',
    '--title',
    type=str,
)

parser.add_argument(
    '-o',
    '--to-phone',
    action="store_true",
    help="Use the phone",
)

parser.add_argument('message', type=str, nargs="*", help="The message")

args = parser.parse_args()

message = ' '.join(args.message)

duration = int(max(len(message) / 10, 3) * 1000)

konix_notify.main(message,
                  title=args.title,
                  unique=args.unique,
                  duration=duration,
                  type_=args.type,
                  to_phone=args.to_phone)
