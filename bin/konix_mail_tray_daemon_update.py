#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import configparser
import subprocess
import shlex
import os
import sys
import konix_notify
import tempfile
import redis

db = redis.StrictRedis(port=6380)

def write(letter):
    db.rpush("mail_tray_daemon", letter)


def main():
    files = os.environ["KONIX_NOTMUCH_SAVED_SEARCHES"].split(os.pathsep)
    default_letter = "i"
    existing_files = [file for file in files if os.path.exists(file)]
    if not existing_files:
      konix_notify.main("No valid file in KONIX_NOTMUCH_SAVED_SEARCHES")
      sys.exit(1)
    file = existing_files[0]
    config = configparser.ConfigParser()
    config.optionxform = str    # keys not converted into lower case
    config.read(file)
    sections = config.sections()
    if "default" in sections:
        default_letter = config.get("default", "icon_letter")
    sections = [section for section in sections if section != "default"]
    import notmuch
    db = notmuch.Database()
    for section in config.sections():
        try:
            icon_letter = config.get(section, "icon_letter")
        except:
            icon_letter = None
        search = config.get(section, "search")
        if icon_letter:
            q = notmuch.Query(db, search)
            res = q.count_messages()
            if res != 0:
                write(icon_letter)
                sys.exit(0)
    write(default_letter)
    sys.exit(0)


if __name__ == "__main__":
    main()
