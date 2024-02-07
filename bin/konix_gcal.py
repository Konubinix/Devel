#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import argparse
import sys

import konix_gcalib

parser = argparse.ArgumentParser(description="""Gcalendar.""")

parser.add_argument('-m',
                    '--make-place',
                    help="""Set this option for new events to move existing
                    ones, this is very intrusive but prevents any conflicts of appointment.""",
                    action="store_true")

parser.add_argument('-a', '--account', help="""TODO.""", type=str, default="")

parser.add_argument('--event-formatter', help="""TODO.""", type=str)

parser.add_argument('argv', nargs=argparse.REMAINDER)
args = parser.parse_args()

if __name__ == "__main__":
    program = konix_gcalib.GCall(make_place=args.make_place,
                                 account=args.account)
    if args.event_formatter:
        program.event_formatter = args.event_formatter
    if len(args.argv) > 0:
        cmd = args.argv[0]
        func = getattr(program, 'do_' + cmd)
        func(*args.argv[1:])
    else:
        program.cmdloop()
