#!/usr/bin/env python3
# -*- coding:utf-8 -*-
import logging
import os
import shlex
import subprocess
import sys
from functools import partial

from six.moves.xmlrpc_client import ServerProxy

logging.basicConfig(level=logging.DEBUG)
LOGGER = logging.getLogger(__name__)


def by_pynotify(message, urgency, duration, title):
    import notify2
    if not notify2.init("Message"):
        sys.exit(1)
    n = notify2.Notification("Message", message)
    n.set_timeout(3000)
    n.show()


def by_notify_send(message, urgency, duration, title):
    urgency = {
        "normal": "normal",
        "annoying": "normal",
        "boring": "critical",
    }[urgency]
    args = [
        "notify-send",
        "--expire-time",
        str(duration),
        "--urgency",
        urgency,
    ]
    if title:
        args += [title]
    args += [message]

    subprocess.check_call(args)


def by_sl4a(message, type_, duration, title):
    import andlib
    from konix_android import droid
    if type_ == "normal":
        droid.makeToast(message)
    if type_ == "annoying":
        droid.notify("!", message)
        droid.vibrate(1000)
    if type_ == "boring":
        droid.vibrate(1000)
        droid.notify("!", message)
        droid.dialogDismiss()
        andlib.display(droid, "!", message, wait=False)


def send_to_phone(message, type_, duration, title):
    here = os.path.exists(os.path.expanduser("~/.here"))
    priority = {
        "normal": 1,
        "annoying": 2,
        "boring": 3,
    }[type_]
    subprocess.check_call(
        ["clk", "ntfy", "--priority",
         str(priority), message])


# type_ is normal or annoying or boring
def main(message,
         unique=False,
         duration=3000,
         type_="normal",
         to_phone=False,
         title=""):
    message = message.replace("<", "-")
    if to_phone:
        send_to_phone(message, type_, duration, title)
    else:
        try:
            local_display(message, unique, type_, duration, title)
        except Exception as e:
            LOGGER.critical("Could not display locally")
            LOGGER.exception(e)


def local_display(message,
                  unique=False,
                  type_="normal",
                  duration=3000,
                  title=""):
    candidates = [
        by_notify_send,
        by_pynotify,
        by_sl4a,
    ]

    for candidate in candidates:
        try:
            candidate(message, type_, duration, title)
        except Exception as e:
            LOGGER.exception(e)
        else:
            break
    else:
        raise NotImplementedError()
