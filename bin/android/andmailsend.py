#! /usr/bin/env python

import os
os.environ["AP_HOST"]="127.0.0.1"
os.environ["AP_PORT"]="45001"
import android

droid = android.Android()
import argparse

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
