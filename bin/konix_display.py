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
    '-p',
    '--priority',
    type=int,
    help="Adjusts the type, based on ntfy type priority",
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

if args.priority:
    if 0 < args.priority <= 1:
        type = "normal"
    elif 1 < args.priority <= 3:
        type = "annoying"
    else:
        type = "boring"
else:
    type = args.type

konix_notify.main(message,
                  title=args.title,
                  unique=args.unique,
                  duration=duration,
                  type_=type,
                  to_phone=args.to_phone)
