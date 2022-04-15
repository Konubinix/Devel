#!/usr/bin/env python3
# -*- coding:utf-8 -*-
import logging
import subprocess
import shlex
from functools import partial
from six.moves.xmlrpc_client import ServerProxy
import os
import sys

logging.basicConfig(level=logging.DEBUG)
LOGGER = logging.getLogger(__name__)


def by_pynotify(message, urgency):
    import notify2
    if not notify2.init("Message"):
        sys.exit(1)
    n = notify2.Notification("Message", message)
    n.set_timeout(3000)
    n.show()


def by_notify_send(message, urgency):
    urgency = {
        "normal": "normal",
        "annoying": "normal",
        "boring": "critical",
    }[urgency]
    subprocess.check_call(["notify-send", "--urgency", urgency, message])


def by_sl4a(message, type_):
    from konix_android import droid
    import andlib
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


def send_to_phone(message, type_):
    here = os.path.exists(os.path.expanduser("~/.here"))
    priority = {
        "normal": 0 if here else 2,
        "annoying": 4,
        "boring": 8,
    }[type_]
    os.system("clk gotify 'Notify' '{}' -p '{}'".format(message, priority))


# type_ is normal or annoying or boring
def main(message, unique=False, duration=3000, type_="normal", to_phone=False):
    message = message.replace("<", "-")
    try:
        local_display(message, unique, duration, type_)
    except Exception as e:
        LOGGER.critical("Could not display locally")
        LOGGER.exception(e)
    if to_phone:
        send_to_phone(message, type_)


def local_display(message, unique=False, duration=3000, type_="normal"):
    candidates = [
        by_notify_send,
        by_pynotify,
        by_sl4a,
    ]

    for candidate in candidates:
        try:
            candidate(message, type_)
        except Exception as e:
            LOGGER.exception(e)
        else:
            break
    else:
        raise NotImplementedError()
