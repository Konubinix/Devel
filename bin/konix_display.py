#!/usr/bin/python
# -*- coding:utf-8 -*-
import sys
import konix_notify
import argparse

parser = argparse.ArgumentParser(
    description="Display a message using the notification daemon.",
)

parser.add_argument('-d', '--duration', default=3000, type=int,
                    help="The duration of the notification in ms, defaults to 3000")

parser.add_argument('-u', '--unique', action="store_true",
                    help="Unique message")

parser.add_argument('-t', '--type', type=str,
                    help="Type of the notification",
                    choices=["normal", "annoying", "boring"],
                    default="normal",
)

parser.add_argument('message', type=str, nargs="*",
                    help="The message")

args = parser.parse_args()

message = ' '.join(args.message)
konix_notify.main(message, unique=args.unique, duration=args.duration, type_=args.type)
