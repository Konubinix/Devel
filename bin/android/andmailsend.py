#! /usr/bin/env python3

from konix_android import droid
import argparse
import sys

parser = argparse.ArgumentParser(
            description="Send a mail from the command line.",
            )

parser.add_argument('-t', '--to', type=str,
                            help="Recipient")

parser.add_argument('-s', '--subject', type=str,
                            help="Subject")

body = sys.stdin.read()
args = parser.parse_args()

droid.sendEmail(args.to, args.subject, body)
