#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import os
import md5
import shutil

import logging
logger = logging.getLogger("rsync")

import argparse
parser = argparse.ArgumentParser(description="""rsync.""")

parser.add_argument('-s','--source',
                    type=str,
                    required=False)

parser.add_argument('-d','--dest',
                    type=str,
                    required=False)

parser.add_argument('-e','--excludes',
                    nargs="+",
                    type=str,
                    default=["Build"],
                    required=False)

LEVELS = {'debug': logging.DEBUG,
          'info': logging.INFO,
          'warning': logging.WARNING,
          'error': logging.ERROR,
          'critical': logging.CRITICAL
         }
parser.add_argument('-v', '--verbosity',
                      help="""Level of verbosity, (defaults to info)""",
                      choices=LEVELS,
                      default="info",
                      required=False)

def get_files(directory, excludes=None):
    excludes = excludes or []
    for dir, subdirs, files in os.walk(directory):
        for exclude in excludes:
            try:
                del subdirs[subdirs.index(exclude)]
            except ValueError:
                pass
        for file in files:
            yield os.path.relpath(os.path.join(dir, file), directory)

def main():
    args = parser.parse_args()
    logging.basicConfig(level=LEVELS[args.verbosity])
    srcs = set(get_files(args.source, excludes=args.excludes))
    dsts = set(get_files(args.dest, excludes=args.excludes))
    for extra in dsts - srcs:
        dst = os.path.join(args.dest, extra)
        logger.info("del {}".format(dst))
        os.unlink(dst)
    for common in srcs & dsts:
        src = os.path.join(args.source, common)
        dst = os.path.join(args.dest, common)
        src_sig = md5.md5(open(src, "rb").read()).hexdigest()
        dst_sig = md5.md5(open(dst, "rb").read()).hexdigest()
        if src_sig != dst_sig:
            logger.info("{} X-> {}".format(src, dst))
            logger.debug("{} vs {}".format(src_sig, dst_sig))
            os.unlink(dst)
            shutil.copy2(
                src,
                dst,
            )
    for tocreate in srcs - dsts:
        src = os.path.join(args.source, tocreate)
        dst = os.path.join(args.dest, tocreate)
        logger.info("{} -> {}".format(src, dst))
        directory = os.path.dirname(dst)
        if not os.path.exists(directory):
            os.makedirs(directory)
        shutil.copy2(
            src,
            dst,
        )

if __name__ == "__main__":
    main()
